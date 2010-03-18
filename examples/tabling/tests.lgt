
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "tabling" example.']).

	:- if(current_logtalk_flag(tabling, supported)).

		test(tabling_1) :-
			setof(Y, paths::path(1, Y), Ys),
			Ys = [2, 3, 4, 5].

		test(tabling_2) :-
			fibonacci::fib(30, F),
			F = 1346269.

	:- endif.

:- end_object.
