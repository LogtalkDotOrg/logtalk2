
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2010/04/10,
		comment is 'Unit tests for the "constraints/bp" example.']).

	test(constraints_bp_1) :-
		puzzle::solve(V),
		V == [9,5,6,7,1,0,8,2].

:- end_object.
