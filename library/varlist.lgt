
:- object(varlist,
	extends(list)).

	:- info([
		version is 1.4,
		author is 'Paulo Moura',
		date is 2009/4/25,
		comment is 'List of variables predicates.']).

	flatten(List, Flatted) :-
		flatten(List, [], Flatted).

	flatten(Var, Tail, [Var| Tail]) :-
		var(Var),
		!.
	flatten([], Flatted, Flatted) :-
		!.
	flatten([Head| Tail], List, Flatted) :-
		flatten(Tail, List, Aux),
		flatten(Head, Aux, Flatted).

	last([Head| Tail], Last) :-
		last(Tail, Head, Last).

	last([], Head, Last) :-
		Head == Last.
	last([Head| Tail], _, Last) :-
		last(Tail, Head, Last).

	member(Element, [Head| _]) :-
		Element == Head.
	member(Element, [_| Tail]) :-
		member(Element, Tail).

	memberchk(Element, [Head| Tail]) :-
		(	Element == Head ->
			true
		;	memberchk(Element, Tail)
		).

	nextto(Element1, Element2, List) :-
		(	var(List) ->
			List = [Element1, Element2| _]
		;	List = [Element11, Element22| _],
			Element1 == Element11,
			Element2 == Element22
		).
	nextto(Element1, Element2, [_| Tail]) :-
		nextto(Element1, Element2, Tail).

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
	prefix(Prefix, [Head2| Tail2]) :-
		(	var(Prefix) ->
			Prefix = [Head2| Tail1]
		;	Prefix = [Head1| Tail1],
			Head1 == Head2
		),
		prefix(Tail1, Tail2).

	select(Element, List, Tail) :-
		(	var(List) ->
			List = [Element| Tail]
		;	List = [Head| Tail],
			Element == Head
		).
	select(Element, [Head| Tail], [Head| Tail2]) :-
		select(Element, Tail, Tail2).

	sublist(Sublist, List) :-
		equal(List, Sublist).
	sublist(Sublist, [Head| Tail]) :-
		sublist(Tail, Head, Sublist).

	sublist(List, _, Sublist) :-
		equal(List, Sublist).
	sublist([Head| Tail], _, Sublist) :-
		sublist(Tail, Head, Sublist).
	sublist([Head| Tail], Element1, Sublist) :-
		(	var(Sublist) ->
			Sublist = [Element1| Subtail]		
		;	Sublist = [Element2| Subtail],
			Element1 == Element2
		),
		sublist(Tail, Head, Subtail).

	equal([], []).
	equal([Head| Tail], Sublist) :-
		(	var(Sublist) ->
			Sublist = [Head| Tail]
		;	Sublist = [Subhead| Subtail],
			Subhead == Head,
			equal(Tail, Subtail)
		).

	suffix(Sufix, List) :-
		equal(List, Sufix).
	suffix(Sufix, [_| Tail]) :-
		suffix(Sufix, Tail).

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
