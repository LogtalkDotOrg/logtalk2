
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

	test(modules_2) :-
		setof(Prop, Pred^(exports::predicate_property(p(Pred), Prop)), Solutions),
		Solutions == [public, static, declared_in(exports), defined_in(exports)].

	test(modules_3) :-
		findall(N, exports::p(N), Solutions),
		Solutions == [1, 2, 3].

	test(modules_4) :-
		test::names(Names),
		Names == [paulo, carlos, helena].

	test(modules_5) :-
		test::test(Names),
		Names == [paulo, carlos, helena].

:- end_object.
