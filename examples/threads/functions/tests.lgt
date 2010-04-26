
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "threads/functions" example.']).

	:- uses(lgtunit, [op(700, xfx, '=~='), '=~='/2]).

	:- discontiguous(succeeds/1).

	succeeds(functions_1) :-
		bisection::find_root(f1, 1.0, 2.3, 1.0e-15, Zero),
		Zero =~= 2.0.

	succeeds(functions_2) :-
		newton::find_root(f1, 1.0, 2.3, 1.0e-15, Zero),
		Zero =~= 2.0.

	succeeds(functions_3) :-
		muller::find_root(f1, 1.0, 2.3, 1.0e-15, Zero),
		Zero =~= 2.0.

	succeeds(functions_4) :-
		bisection::find_root(f2, 1.0, 1.3, 1.0e-15, Zero),
		Zero =~= 1.25809265664599.

	succeeds(functions_5) :-
		newton::find_root(f2, 1.0, 1.3, 1.0e-15, Zero),
		Zero =~= 1.25809265664599.

	succeeds(functions_6) :-
		muller::find_root(f2, 1.0, 1.3, 1.0e-15, Zero),
		Zero =~= 1.25809265664599.

	fails(functions_7) :-
		bisection::find_root(humps, -1.0, 2.0, 1.0e-15, _).

	succeeds(functions_8) :-
		muller::find_root(humps, -1.0, 2.0, 1.0e-15, Zero),
		Zero =~= 1.29954968258.

	throws(functions_9, error(evaluation_error(float_overflow), _)) :-
		newton::find_root(humps, -1.0, 2.0, 1.0e-15, _).

	succeeds(functions_10) :-
		function_root::find_root(f1, 1.0, 2.3, 1.0e-15, Zero, _),
		Zero =~= 2.0.

	succeeds(functions_11) :-
		function_root::find_root(f2, 1.0, 1.3, 1.0e-15, Zero, _),
		Zero =~= 1.25809265664599.

	succeeds(functions_12) :-
		function_root::find_root(f3, 0.0, 3.0, 1.0e-15, Zero, _),
		Zero =~= 1.4142135623731.

	succeeds(functions_13) :-
		function_root::find_root(f4, -1.0, 2.0, 1.0e-15, Zero, _),
		Zero =~= -8.88178419700125e-16.

	succeeds(functions_14) :-
		function_root::find_root(humps, -1.0, 2.0, 1.0e-15, Zero, _),
		Zero =~= 1.29954968258482.

:- end_object.
