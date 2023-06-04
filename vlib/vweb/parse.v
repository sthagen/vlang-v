module vweb

import net.urllib
import net.http

// Parsing function attributes for methods and path.
fn parse_attrs(name string, attrs []string) !([]http.Method, string, string, string) {
	if attrs.len == 0 {
		return [http.Method.get], '/${name}', '', ''
	}

	mut x := attrs.clone()
	mut methods := []http.Method{}
	mut middleware := ''
	mut path := ''
	mut host := ''

	for i := 0; i < x.len; {
		attr := x[i]
		attru := attr.to_upper()
		m := http.method_from_str(attru)
		if attru == 'GET' || m != .get {
			methods << m
			x.delete(i)
			continue
		}
		if attr.starts_with('/') {
			if path != '' {
				return http.MultiplePathAttributesError{}
			}
			path = attr
			x.delete(i)
			continue
		}
		if attr.starts_with('middleware:') {
			middleware = attr.all_after('middleware:').trim_space()
			x.delete(i)
			continue
		}
		if attr.starts_with('host:') {
			host = attr.all_after('host:').trim_space()
			x.delete(i)
			continue
		}
		i++
	}
	if x.len > 0 {
		return http.UnexpectedExtraAttributeError{
			attributes: x
		}
	}
	if methods.len == 0 {
		methods = [http.Method.get]
	}
	if path == '' {
		path = '/${name}'
	}
	// Make path and host lowercase for case-insensitive comparisons
	return methods, path.to_lower(), middleware, host.to_lower()
}

fn parse_query_from_url(url urllib.URL) map[string]string {
	mut query := map[string]string{}
	for qvalue in url.query().data {
		query[qvalue.key] = qvalue.value
	}
	return query
}

fn parse_form_from_request(request http.Request) !(map[string]string, map[string][]http.FileData) {
	mut form := map[string]string{}
	mut files := map[string][]http.FileData{}
	if request.method in methods_with_form {
		ct := request.header.get(.content_type) or { '' }.split(';').map(it.trim_left(' \t'))
		if 'multipart/form-data' in ct {
			boundary := ct.filter(it.starts_with('boundary='))
			if boundary.len != 1 {
				return error('detected more that one form-data boundary')
			}
			// omit 'boundary="' and the last '"'
			boundary_str := boundary[0].substr(10, boundary[0].len - 1)
			form, files = http.parse_multipart_form(request.data, boundary_str)
		} else {
			form = http.parse_form(request.data)
		}
	}
	return form, files
}
