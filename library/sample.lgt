
:- object(sample,
	imports(statistics)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2008/11/11,
		comment is 'Statistical sample represented as a list of numbers.']).

	skewness([X| Xs], Skewness) :-
		:arithmetic_mean(Xs, 1, Length, X, Mean),
		X2 is (X - Mean) ** 2,
		X3 is (X - Mean) ** 3,
		:squares_and_cubes(Xs, Mean, X2, Squares, X3, Cubes),
		Skewness is (Cubes / Length) / (Squares / Length) ** 1.5.

	standard_deviation([X| Xs], Deviation) :-
		:variance(Xs, 1, Length, X, 0, M2),
		Deviation is sqrt(M2 / (Length - 1)).

	variance([X| Xs], Variance) :-
		:variance(Xs, 1, Length, X, 0, M2),
		Variance is M2 / (Length - 1).

:- end_object.
