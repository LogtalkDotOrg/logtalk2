
:- object(population,
	imports(statistics)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2008/10/4,
		comment is 'Statistical population represented as a list of numbers.']).

    skewness([X| Xs], Skewness) :-
        :arithmetic_mean(Xs, 1, Length, X, Mean),
        X2 is (X - Mean) ** 2,
        X3 is (X - Mean) ** 3,
        :squares_and_cubes(Xs, Mean, X2, Squares, X3, Cubes),
        Skew is (Cubes / Length) / (Squares / Length) ** 1.5,
        Skewness is sqrt(Length*(Length - 1)) * Skew / (Length - 2).

    variance([X| Xs], Variance) :-
        :variance(Xs, 1, Length, X, 0, M2),
        Variance is M2 / Length.

:- end_object.
