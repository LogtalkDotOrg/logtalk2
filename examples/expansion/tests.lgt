
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "expansion" example.']).

	test(expansion_1) :-
		exp_public::expand_term(8, Term),
		Term == eight.

	test(expansion_2) :-
		exp_public::expand_goal(write(Term), EGoal),
		EGoal = write_term(Term, [quoted(true)]).

	test(expansion_3) :-
		exp_protected::expand_term(8, Term),
		Term == 8.

	test(expansion_4) :-
		exp_protected::expand_goal(write(Term), EGoal),
		EGoal = write(Term).

	test(expansion_5) :-
		exp_private::expand_term(8, Term),
		Term == 8.

	test(expansion_6) :-
		exp_private::expand_goal(write(Term), EGoal),
		EGoal = write(Term).

	test(expansion_7) :-
		desc_public::test_term_expansion(8, Term),
		Term == eight.

	test(expansion_8) :-
		desc_public::test_goal_expansion(write(Term), EGoal),
		EGoal = write_term(Term, [quoted(true)]).

	test(expansion_9) :-
		desc_protected::test_term_expansion(8, Term),
		Term == eight.

	test(expansion_10) :-
		desc_protected::test_goal_expansion(write(Term), EGoal),
		EGoal = write_term(Term, [quoted(true)]).

	test(expansion_11) :-
		desc_private::test_term_expansion(8, Term),
		Term == 8.

	test(expansion_12) :-
		desc_private::test_goal_expansion(write(Term), EGoal),
		EGoal = write(Term).

:- end_object.
