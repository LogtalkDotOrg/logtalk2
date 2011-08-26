
:- object(population,
	imports(statistics)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2008/11/11,
		comment is 'Statistical population represented as a list of numbers.']).

	skewness([X| Xs], Skewness) :-
		:arithmetic_mean(Xs, 1, N, X, Mean),
		Square is (X - Mean) ** 2,
		Cube is (X - Mean) ** 3,
		:squares_and_cubes(Xs, Mean, Square, Squares, Cube, Cubes),
		Skew is (Cubes / N) / (Squares / N) ** 1.5,
		Skewness is sqrt(N*(N - 1)) * Skew / (N - 2).

	kurtosis([X| Xs], Kurtosis) :-
		:arithmetic_mean(Xs, 1, N, X, Mean),
		Square is (X - Mean) ** 2,
		Hyper is (X - Mean) ** 4,
		:squares_and_hypers(Xs, Mean, Square, _, Hyper, Hypers),
		:variance(Xs, 1, N, X, 0, M2),
		K2 is M2 / (N - 1),
		Kurtosis is N * (N + 1) * Hypers / ((N - 1) * (N - 2) * (N - 3) * K2**2) - 3 * (N - 1)**2/((N - 2) * (N - 3)).

	standard_deviation([X| Xs], Deviation) :-
		:variance(Xs, 1, N, X, 0, M2),
		Deviation is sqrt(M2 / N).

	variance([X| Xs], Variance) :-
		:variance(Xs, 1, N, X, 0, M2),
		Variance is M2 / N.

:- end_object.
