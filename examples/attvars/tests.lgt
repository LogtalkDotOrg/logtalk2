
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2011/03/02,
		comment is 'Unit tests for the "attvars" example.']).

	test(attvars_1) :-
		\+ (domain::domain(X, [a,b]), X = c).

	test(attvars_2) :-
		domain::domain(X, [a,b]), domain::domain(X, [a,c]),
		X == a.

	test(attvars_3) :-
		domain::domain(X,[a,b,c]), domain::domain(X,[a,c]), get_attr(X, domain, List),
		List == [a,c].

:- end_object.
