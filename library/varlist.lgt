
:- object(varlist,
	extends(list)).

	:- info([
		version is 1.3,
		author is 'Paulo Moura',
		date is 2009/3/6,
		comment is 'List of variables predicates.']).

	member(Element, [Head| _]) :-
		Element == Head.
	member(Element, [_| Tail]) :-
		member(Element, Tail).

	memberchk(Element, [Head| Tail]) :-
		(	Element == Head ->
			true
		;	memberchk(Element, Tail)
		).

	prefix([], _).
	prefix([Head1| Tail1], [Head2| Tail2]) :-
		Head1 == Head2,
		prefix(Tail1, Tail2).

	select(Element, [Head| Tail], Tail) :-
		Element == Head.
	select(Element, [Head| Tail], [Head| Tail2]) :-
		select(Element, Tail, Tail2).

	valid(-) :-		% catch variables and lists with unbound tails
		!,
		fail.
	valid([]).
	valid([Element| List]) :-
		var(Element),
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
