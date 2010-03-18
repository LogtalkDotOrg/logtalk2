
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "instvars" example.']).

	test(instvars_1) :-
		instance1::ivar(Value1), instance2::ivar(Value2), instance3::ivar(Value3),
		Value1 == 0, Value2 == 0, Value3 == 0.

	test(instvars_2) :-
		instance1::set_ivar(1).

	test(instvars_3) :-
		instance1::ivar(Value1), instance2::ivar(Value2), instance3::ivar(Value3),
		Value1 == 1, Value2 == 0, Value3 == 0.

:- end_object.
