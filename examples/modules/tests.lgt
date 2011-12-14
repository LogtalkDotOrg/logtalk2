
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.2,
		author is 'Parker Jones and Paulo Moura',
		date is 2011/12/14,
		comment is 'Unit tests for the "modules" example.']).

	test(modules_1) :-
		findall(Pred, exports::current_predicate(Pred), Solutions),
		Solutions == [p/1].

	test(modules_2) :-
		setof(Prop, Pred^(exports::predicate_property(p(Pred), Prop)), Solutions),
		Solutions == [logtalk, public, static, declared_in(exports), defined_in(exports), scope(public)].

	test(modules_3) :-
		findall(N, exports::p(N), Solutions),
		Solutions == [1, 2, 3].

	test(modules_4) :-
		test::names(Names),
		Names == [paulo, carlos, helena].

	test(modules_5) :-
		test::test(Names),
		Names == [paulo, carlos, helena].

	:- if(current_object(client)).	% client for testing use_module/1 directives, which are
									% only supported for some back-end Prolog compilers
		test(modules_6) :-
			client::names(Names),
			Names == [paulo, carlos, helena].

		test(modules_7) :-
			client::test(Names),
			Names == [paulo, carlos, helena].

	:- endif.

:- end_object.
