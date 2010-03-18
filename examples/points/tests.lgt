
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "points" example.']).

	test(points_1) :-
		point::new(Point, [position-(1, 3)]),
		Point::(print, move(7, 4), print).

	test(points_2) :-
		bounded_point::new(Point, [position-(1, 3), bounds(x)-(0, 13), bounds(y)-(-7, 7)]),
		Point::(print, move(7, 4), print).

	test(points_3) :-
		history_point::new(Point, [position-(1, 3)]),
		Point::(print, move(7, 4), print).

	test(points_4) :-
		bounded_history_point::new(Point, [position-(1, 3), bounds(x)-(0, 13), bounds(y)-(-7, 7)]),
		Point::(print, move(7, 4), print).

	cleanup :-
		point::delete_all,
		bounded_point::delete_all,
		history_point::delete_all,
		bounded_history_point::delete_all.

:- end_object.
