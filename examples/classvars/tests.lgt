
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "classvars" example.']).

	test(classvars_1) :-
		instance1::cv(Value1), 
		instance2::cv(Value2), 
		instance3::cv(Value3),
		Value1 == 0, Value2 == 0, Value3 == 0.

	% test 2.  Note: Depends on previous test.
	test(classvars_2) :-
    	instance1::set_cv(1).

	% test 3.   Note: Depends on previous test.
	test(classvars_3) :-
		instance2::cv(Value2), 
		instance3::cv(Value3),
		Value2 == 1, Value3 == 1.

:- end_object.
