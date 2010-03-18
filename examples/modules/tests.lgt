
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "modules" example.']).

	test(modules_1) :-
		findall(Pred, exports::current_predicate(Pred), Solutions),
		Solutions == [p/1].

	% test 2.  % normally for a more robust unit test, the solutions should be sorted, but 'list' is already in use
	test(modules_2) :-
		findall(Prop, exports::predicate_property(p(_), Prop), Solutions),
		Solutions == [public,static,declared_in(exports),defined_in(exports)].

	test(modules_3) :-
		findall(N, exports::p(N), Solutions),
		Solutions == [1,2,3].

:- end_object.
