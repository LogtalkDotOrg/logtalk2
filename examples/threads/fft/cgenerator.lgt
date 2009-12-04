
:- object(cgenerator).

	:- info([
		version is 1.0,
		author is 'Paul Crocker',
		date is 2008/2/14,
		comment is 'Simple object defining a predicate for generating lists of random complex numbers.']).

	:- public(list/2).

	list(N, Cs) :-
		N2 is N*2,
		random::randseq(N2, 0.0, 1.0, Fs),
		convert(Fs, Cs).

	convert([], []).
	convert([F1, F2 | Fs] , [c(F1, F2)| Cs]) :-
		convert(Fs, Cs).

:- end_object.
