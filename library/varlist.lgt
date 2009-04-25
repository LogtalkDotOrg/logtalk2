
:- object(varlist,
	extends(list)).

	:- info([
		version is 1.4,
		author is 'Paulo Moura',
		date is 2009/4/25,
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

	nth0(Nth, List, Element) :-
		nth(Element, List, 0, Nth, _).

	nth0(Nth, List, Element, Tail) :-
		nth(Element, List, 0, Nth, Tail).

	nth1(Nth, List, Element) :-
		nth(Element, List, 1, Nth, _).

	nth1(Nth, List, Element, Tail) :-
		nth(Element, List, 1, Nth, Tail).

	nth(Element, List, Acc, Nth, Tail) :-
		(	integer(Nth),
			Nth >= Acc,
			nth_aux(NthElement, List, Acc, Nth, Tail) ->
			Element == NthElement
		;	var(Nth),
			nth_aux(Element, List, Acc, Nth, Tail)
		).

	nth_aux(Element, [Head| Tail], Position, Position, Tail) :-
		Element == Head.
	nth_aux(Element, [_| List], Count, Position, Tail) :-
		Count2 is Count + 1,
		nth_aux(Element, List, Count2, Position, Tail).

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
