
:- object(varlist,
	extends(list)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2008/7/24,
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

:- end_object.
