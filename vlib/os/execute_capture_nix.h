// v_os_execute_set_cloexec marks an fd as close-on-exec. When multiple threads
// each call os.execute, every pipe() they create is briefly visible to all of
// them; without FD_CLOEXEC, one thread's spawned child can inherit another
// thread's pipe write end, keeping that pipe open after the intended writer
// exits. The reader then never observes EOF and the captured output ends up
// empty (seen on macOS arm64 in CI). Setting CLOEXEC closes the fd
// automatically across exec, which fixes the race for both fork/execvp and
// posix_spawn paths.
static void v_os_execute_set_cloexec(int fd) {
	int flags = fcntl(fd, F_GETFD, 0);
	if (flags >= 0) {
		fcntl(fd, F_SETFD, flags | FD_CLOEXEC);
	}
}

#if defined(__ANDROID__) && (!defined(__ANDROID_API__) || __ANDROID_API__ < 28)
// Android API levels below 28 do not provide posix_spawn(). Fall back to
// fork()/execvp() with a pipe; this is what popen() does internally and
// is sufficient for capturing the merged stdout+stderr of a shell command.
static int v_os_execute_capture_start(const char *cmd, int *child_pid, int *read_fd) {
	int pipefd[2];
	if (pipe(pipefd) != 0) {
		return -1;
	}
	v_os_execute_set_cloexec(pipefd[0]);
	v_os_execute_set_cloexec(pipefd[1]);
	pid_t pid = fork();
	if (pid < 0) {
		close(pipefd[0]);
		close(pipefd[1]);
		return -1;
	}
	if (pid == 0) {
		// child: redirect stdout+stderr to the pipe, then exec the shell.
		// dup2 clears FD_CLOEXEC on the destination, so STDOUT/STDERR stay
		// open across exec while the original pipe fds are auto-closed.
		dup2(pipefd[1], STDOUT_FILENO);
		dup2(pipefd[1], STDERR_FILENO);
		close(pipefd[0]);
		close(pipefd[1]);
		execlp("sh", "sh", "-c", cmd, (char *)NULL);
		_exit(127);
	}
	close(pipefd[1]);
	*child_pid = (int)pid;
	*read_fd = pipefd[0];
	return 0;
}
#else
#if defined(__has_include) && __has_include(<spawn.h>)
#include <spawn.h>
#else
// Provide minimal posix_spawn declarations when <spawn.h> is not available
// (e.g. during cross-compilation with an incomplete sysroot).
typedef struct { int __allocated; int __used; void *__actions; int __pad[16]; } posix_spawn_file_actions_t;
typedef struct { short __flags; pid_t __pgrp; sigset_t __sd; sigset_t __ss; struct sched_param __sp; int __policy; int __pad[16]; } posix_spawnattr_t;
int posix_spawn(pid_t *, const char *, const posix_spawn_file_actions_t *, const posix_spawnattr_t *, char *const [], char *const []);
int posix_spawn_file_actions_init(posix_spawn_file_actions_t *);
int posix_spawn_file_actions_destroy(posix_spawn_file_actions_t *);
int posix_spawn_file_actions_adddup2(posix_spawn_file_actions_t *, int, int);
int posix_spawn_file_actions_addclose(posix_spawn_file_actions_t *, int);
#endif

extern char **environ;

static int v_os_execute_capture_start(const char *cmd, int *child_pid, int *read_fd) {
	int pipefd[2];
	if (pipe(pipefd) != 0) {
		return -1;
	}
	v_os_execute_set_cloexec(pipefd[0]);
	v_os_execute_set_cloexec(pipefd[1]);
	posix_spawn_file_actions_t actions;
	if (posix_spawn_file_actions_init(&actions) != 0) {
		close(pipefd[0]);
		close(pipefd[1]);
		return -1;
	}
	int err = 0;
	if ((err = posix_spawn_file_actions_adddup2(&actions, pipefd[1], STDOUT_FILENO)) != 0
		|| (err = posix_spawn_file_actions_adddup2(&actions, pipefd[1], STDERR_FILENO)) != 0
		|| (err = posix_spawn_file_actions_addclose(&actions, pipefd[0])) != 0
		|| (err = posix_spawn_file_actions_addclose(&actions, pipefd[1])) != 0) {
		posix_spawn_file_actions_destroy(&actions);
		close(pipefd[0]);
		close(pipefd[1]);
		return -1;
	}
	char *const argv[] = {(char *)"/bin/sh", (char *)"-c", (char *)cmd, NULL};
	err = posix_spawn(child_pid, "/bin/sh", &actions, NULL, argv, environ);
	posix_spawn_file_actions_destroy(&actions);
	if (err != 0) {
		close(pipefd[0]);
		close(pipefd[1]);
		return -1;
	}
	close(pipefd[1]);
	*read_fd = pipefd[0];
	return 0;
}
#endif
