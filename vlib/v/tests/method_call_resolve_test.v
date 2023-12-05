struct Human {
	name string
}

enum Animal {
	dog
	cat
}

type Entity = Animal | Human

@[sumtype_to: Animal]
fn (ent Entity) json_cast_to_animal() Animal {
	return ent as Animal
}

@[sumtype_to: Human]
fn (ent Entity) json_cast_to_human() Human {
	return ent as Human
}

fn encode[T](val T) {
	$if T is $sumtype {
		$for method in T.methods {
			if method.attrs.len >= 1 {
				if method.attrs[0].contains('sumtype_to') {
					if val.type_name() == method.attrs[0].all_after('sumtype_to:').trim_space() {
						encode(val.$method())
					}
				}
			}
		}
	} $else $if T is $struct {
		assert val == Human{
			name: 'Monke'
		}
	} $else $if T is $enum {
		assert val == Animal.cat
	}
}

fn test_main() {
	encode(Entity(Human{'Monke'}))
	encode(Entity(Animal.cat))
}
