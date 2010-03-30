
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "instmethods" example.']).

	test(instmethods_1) :-
		instance1::predicate_property(method, defined_in(Object)),
		Object == root.

	test(instmethods_2) :-
		instance2::predicate_property(method, defined_in(Object)),
		Object == instance2.

	test(instmethods_3) :-
		instance3::predicate_property(method, defined_in(Object)),
		Object == instance3.

:- end_object.
