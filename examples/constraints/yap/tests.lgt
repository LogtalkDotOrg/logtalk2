
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2010/04/10,
		comment is 'Unit tests for the "constraints/yap" example.']).

	test(constraints_yap_1) :-
		puzzle::solve(Sum=Rs),
		Sum == [9, 5, 6, 7] + [1, 0, 8, 5],
		Rs == [1, 0, 6, 5, 2].

:- end_object.
