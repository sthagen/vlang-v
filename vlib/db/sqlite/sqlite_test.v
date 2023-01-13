import db.sqlite

type Connection = sqlite.DB

struct User {
pub:
	id   int    [primary; sql: serial]
	name string
}

type Content = []u8 | string

struct Host {
pub:
	db Connection
}

fn (back Host) get_users() []User {
	return []
}

fn create_host(db Connection) Host {
	sql db {
		create table User
	}

	return Host{
		db: db
	}
}

fn test_sqlite() {
	$if !linux {
		return
	}
	mut db := sqlite.connect(':memory:') or { panic(err) }
	assert db.is_open
	db.exec('drop table if exists users')
	db.exec("create table users (id integer primary key, name text default '');")
	db.exec("insert into users (name) values ('Sam')")
	assert db.last_insert_rowid() == 1
	assert db.get_affected_rows_count() == 1
	db.exec("insert into users (name) values ('Peter')")
	assert db.last_insert_rowid() == 2
	db.exec("insert into users (name) values ('Kate')")
	assert db.last_insert_rowid() == 3
	nr_users := db.q_int('select count(*) from users')
	assert nr_users == 3
	name := db.q_string('select name from users where id = 1')
	assert name == 'Sam'

	// this insert will be rejected due to duplicated id
	db.exec("insert into users (id,name) values (1,'Sam')")
	assert db.get_affected_rows_count() == 0

	users, mut code := db.exec('select * from users')
	assert users.len == 3
	assert code == 101
	code = db.exec_none('vacuum')
	assert code == 101
	user := db.exec_one('select * from users where id = 3') or { panic(err) }
	println(user)
	assert user.vals.len == 2

	db.exec("update users set name='zzzz' where name='qqqq'")
	assert db.get_affected_rows_count() == 0

	db.exec("update users set name='Peter1' where name='Peter'")
	assert db.get_affected_rows_count() == 1

	db.exec("delete from users where name='qqqq'")
	assert db.get_affected_rows_count() == 0

	db.exec("delete from users where name='Sam'")
	assert db.get_affected_rows_count() == 1

	db.close() or { panic(err) }
	assert !db.is_open
}

fn test_can_access_sqlite_result_consts() {
	assert sqlite.sqlite_ok == 0
	assert sqlite.sqlite_error == 1
	// assert sqlite.misuse == 21
	assert sqlite.sqlite_row == 100
	assert sqlite.sqlite_done == 101
}

fn test_alias_db() {
	create_host(sqlite.connect(':memory:')!)
	assert true
}
