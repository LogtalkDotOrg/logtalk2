
:- object(list(_Type),
	extends(list)).

	:- info([
		version is 1.21,
		author is 'Paulo Moura',
		date is 2010/2/10,
		comment is 'List predicates with elements constrained to a single type.',
		parnames is ['Type']]).

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
