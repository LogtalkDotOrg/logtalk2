
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2010/04/10,
		comment is 'Unit tests for the "constraints/bp" example.']).

	test(constraints_bp_1) :-
		clique::go.

	test(constraints_bp_2) :-
		magic::go.

	test(constraints_bp_3) :-
		puzzle::solve(V),
		V == [9,5,6,7,1,0,8,2].

	test(constraints_bp_4) :-
		steiner::go.

	test(constraints_bp_5) :-
		queens3::top.

	test(constraints_bp_6) :-
		srq::q.

	test(constraints_bp_7) :-
		srq::q_all.

:- end_object.
