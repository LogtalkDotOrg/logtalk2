
:- object(list(_Type),
	extends(list)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2008/4/22,
		comment is 'List predicates with elements constrained to a single type.']).

	valid(-) :-		% catch variables and lists with unbound tails
		!,
		fail.
	valid([]).
	valid([Element| List]) :-
		parameter(1, Type),
		Type::valid(Element),
		valid(List).

:- end_object.
