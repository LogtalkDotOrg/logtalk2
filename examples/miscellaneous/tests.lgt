
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2010/04/25,
		comment is 'Unit tests for the "miscellaneous" example.']).

	test(miscellaneous_1) :-
		hanoi::run(3).

	test(miscellaneous_2) :-
		queens::queens(8).

:- end_object.
