
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2010/04/10,
		comment is 'Unit tests for the "constraints/eclipse" example.']).

	test(constraints_eclipse_1) :-
		puzzle::sendmore1(Digits),
		Digits == [9, 5, 6, 7, 1, 0, 8, 2].

	test(constraints_eclipse_2) :-
		puzzle::sendmore2(Digits),
		Digits == [9, 5, 6, 7, 1, 0, 8, 2].

:- end_object.
