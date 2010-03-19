
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "ack" example.']).

	test(ack_1) :-
		ack::ack(2, 4, Result),
		Result == 11.

	test(ack_2) :-
		ack::ack(3, 3, Result),
		Result == 61.

	test(ack_3) :-
		ack::ack(3, 4, Result),
		Result == 125.

:- end_object.
