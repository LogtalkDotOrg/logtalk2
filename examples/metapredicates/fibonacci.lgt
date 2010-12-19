
:- object(fibonacci).

	:- info([
		version is 1.0,
		date is 2010/12/19,
		author is 'Paulo Moura',
		comment is 'Computation of Fibonacci numbers using a fold left meta-predicate.']).

	:- public(nth/2).
	:- mode(nth(+integer, -integer), one).
	:- info(nth/2, [
		comment is 'Calculates the Nth Fibonacci number.',
		argnames is ['Nth', 'Number']]).

	nth(N, F) :-
		meta::fold_left(next, 0-[0,1], _, N-[F, _]).

	next(N-[F1, F2], _, N2-[F2, F3]) :-
		F3 is F1 + F2,
		N2 is N + 1.

:- end_object.
