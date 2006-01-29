
:- object(list(_type),
	extends(list)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2006/1/29,
		comment is 'List predicates with elements constrained to a single type.']).

	valid(List) :-
		nonvar(List),
		parameter(1, Type),
		\+ \+ valid2(List, Type).

	valid2([], _).
	valid2([Value| List], Type) :-
		Type::valid(Value),
		valid2(List, Type).

:- end_object.
