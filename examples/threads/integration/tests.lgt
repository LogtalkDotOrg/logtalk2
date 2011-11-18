
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.21,
		author is 'Parker Jones and Paulo Moura',
		date is 2011/11/17,
		comment is 'Unit tests for the "threads/integration" example.']).

	test(integration_1) :-
		quadrec(4)::integrate(quiver, 0.001, 0.999, 0, 1.0e-10, Integral),
		abs(Integral) =< 1.0e-10.

	test(integration_2) :-
		quadrec(8)::integrate(quiver, 0.001, 0.999, 4, 1.0e-10, Integral),
		abs(Integral) =< 1.0e-10.

	test(integration_3) :-
		quadsplit(8)::integrate(quiver, 0.001, 0.999, 4, 1.0e-10, Integral),
		abs(Integral) =< 1.0e-10.

:- end_object.
