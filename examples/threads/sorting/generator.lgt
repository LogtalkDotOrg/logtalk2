
:- object(generator).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2008/2/7,
		comment is 'Simple object defining a predicate for generating lists of random values.']).

	:- public(list/2).

	list(N, List) :-
		random::randseq(N, 0.0, 1.0, List).

:- end_object.
