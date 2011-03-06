
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "expansion" example.']).

	test(expansion_01) :-
		exp_public::expand_term(8, Term),
		Term == eight.

	test(expansion_02) :-
		exp_public::expand_goal(write(Term), EGoal),
		EGoal = write_term(Term, [quoted(true)]).

	test(expansion_03) :-
		exp_protected::expand_term(8, Term),
		Term == 8.

	test(expansion_04) :-
		exp_protected::expand_goal(write(Term), EGoal),
		EGoal = write(Term).

	test(expansion_05) :-
		exp_private::expand_term(8, Term),
		Term == 8.

	test(expansion_06) :-
		exp_private::expand_goal(write(Term), EGoal),
		EGoal = write(Term).

	test(expansion_07) :-
		desc_public::test_term_expansion(8, Term),
		Term == eight.

	test(expansion_08) :-
		desc_public::test_goal_expansion(write(Term), EGoal),
		EGoal = write_term(Term, [quoted(true)]).

	test(expansion_09) :-
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

	test(expansion_13) :-
		cooked << (aa, bb, cc).

	test(expansion_14) :-
		cooked << (ha, hb, hc).

	test(expansion_14) :-
		cooked << p.

:- end_object.
