
:- object(set(_Type),
	extends(set)).

	:- info([
		version is 1.21,
		author is 'Paulo Moura',
		date is 2010/2/10,
		comment is 'Set predicates with elements constrained to a single type.',
		parnames is ['Type']]).

	valid((-)) :-				% catch variables
		!,
		fail.
	valid([]) :-
		!.
	valid([Element| Set]) :-
		check_order(Set, Element).

	check_order((-), _) :-	% catch unbound tails
		!,
		fail.
	check_order([], _) :-
		!.
	check_order([Element2| Set], Element1) :-
		parameter(1, Type),
		Type::valid(Element1),
		Type::valid(Element2),
		Element2 @> Element1,
		check_order(Set, Element2).

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
