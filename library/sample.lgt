
:- object(sample,
	imports(statistics)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2008/11/11,
		comment is 'Statistical sample represented as a list of numbers.']).

	skewness([X| Xs], Skewness) :-
		:arithmetic_mean(Xs, 1, N, X, Mean),
		Square is (X - Mean) ** 2,
		Cube is (X - Mean) ** 3,
		:squares_and_cubes(Xs, Mean, Square, Squares, Cube, Cubes),
		Skewness is (Cubes / N) / (Squares / N) ** 1.5.

	kurtosis([X| Xs], Kurtosis) :-
		:arithmetic_mean(Xs, 1, N, X, Mean),
		Square is (X - Mean) ** 2,
		Hyper is (X - Mean) ** 4,
		:squares_and_hypers(Xs, Mean, Square, Squares, Hyper, Hypers),
		Kurtosis is (Hypers / N) / (Squares / N) ** 2 - 3.

	standard_deviation([X| Xs], Deviation) :-
		:variance(Xs, 1, N, X, 0, M2),
		Deviation is sqrt(M2 / (N - 1)).

	variance([X| Xs], Variance) :-
		:variance(Xs, 1, N, X, 0, M2),
		Variance is M2 / (N - 1).

:- end_object.
