
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.1,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/04/26,
		comment is 'Unit tests for the "threads/integration" example.']).

	:- uses(lgtunit, [op(700, xfx, '=~='), '=~='/2]).

	test(integration_1) :-
		quadrec(4)::integrate(quiver, 0.001, 0.999, 0, 1.0e-10, Integral),
		Integral =~= 6.66133814775e-16.

	test(integration_2) :-
		quadrec(8)::integrate(quiver, 0.001, 0.999, 4, 1.0e-10, Integral),
		Integral =~= 2.70827138493e-10.

	test(integration_3) :-
		quadsplit(8)::integrate(quiver, 0.001, 0.999, 4, 1.0e-10, Integral),
		Integral =~= 2.70827138493e-10.

:- end_object.
