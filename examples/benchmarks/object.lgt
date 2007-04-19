
:- object(object).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2007/04/17,
		comment is 'Example object for benchmarking library predicate calls.']).

	:- public(length/2).

	length(List, Length) :-
		length(List, 0, Length).

	length([], Length, Length).
	length([_| Tail], Acc, Length) :-
		Acc2 is Acc + 1,
		length(Tail, Acc2, Length).

:- end_object.
