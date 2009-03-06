
:- object(list(_Type),
	extends(list)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2009/3/6,
		comment is 'List predicates with elements constrained to a single type.']).

	valid(-) :-		% catch variables and lists with unbound tails
		!,
		fail.
	valid([]).
	valid([Element| List]) :-
		parameter(1, Type),
		Type::valid(Element),
		valid(List).

	check(Term) :-
		this(This),
		sender(Sender),
		(	valid(Term) ->
			true
		;	var(Term) ->
			throw(error(instantiation_error, This::check(Term), Sender))
		;	throw(error(type_error(This, Term), This::check(Term), Sender))
		).

:- end_object.
