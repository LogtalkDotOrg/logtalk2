
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "threads/tak" example.']).

	test(tak_1) :-
		tak(1)::tak(18, 12, 6, R),
		R == 7.

	test(tak_2) :-
		tak(3)::tak(18, 12, 6, R),
		R == 7.

	test(tak_3) :-
		tak(1)::tak(21, 14, 7, R),
		R == 14.

	test(tak_4) :-
		tak(3)::tak(21, 14, 7, R),
		R == 14.

:- end_object.
