
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "threads/integration2d" example.']).

	test(integration2d_1) :-
		quadsplit2d(1)::integrate(circle, -2, 2, -2, 2, 2, 2.5e-10, Integral),
		abs(Integral - -21.3333) < 0.001.

	test(integration2d_2) :-
		quadsplit2d(4)::integrate(circle, -2, 2, -2, 2, 2, 2.5e-10, Integral), 
		abs(Integral - -21.3333) < 0.001.

	test(integration2d_3) :-
		quadsplit2d(16)::integrate(circle, -2, 2, -2, 2, 2, 2.5e-10, Integral),
		abs(Integral - -21.3333) < 0.001.

	test(integration2d_4) :-
		quadrec2d(1)::integrate(circle, -2, 2, -2, 2, 2, 2.5e-10, Integral),
		abs(Integral - -21.3333) < 0.001.

	test(integration2d_5) :-
		quadrec2d(4)::integrate(circle, -2, 2, -2, 2, 2, 2.5e-10, Integral),
		abs(Integral - -21.3333) < 0.001.

	test(integration2d_6) :-
		quadrec2d(16)::integrate(circle, -2, 2, -2, 2, 2, 2.5e-10, Integral),
		abs(Integral - -21.3333) < 0.001.

	test(integration2d_7) :-
		quadrec2d(1)::integrate(i15, -2,2,-2,2, 2, 1.0e-4, Integral),
		abs(Integral - 7.73592) < 0.001.

	test(integration2d_8) :-
		quadrec2d(4)::integrate(i15, -2,2,-2,2, 2, 1.0e-4, Integral),
		abs(Integral - 7.73592) < 0.001.

	test(integration2d_9) :-
		quadrec2d(16)::integrate(i15, -2,2,-2,2, 2, 1.0e-4, Integral),
		abs(Integral - 7.73592) < 0.001.

:- end_object.
