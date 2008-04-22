
:- object(varlist,
	extends(list)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2008/4/22,
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

	valid(-) :-		% catch variables and lists with unbound tails
		!,
		fail.
	valid([]).
	valid([Element| List]) :-
		var(Element),
		valid(List).

:- end_object.
