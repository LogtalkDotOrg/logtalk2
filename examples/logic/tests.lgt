
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "logic" example.']).

	test(logic_1) :-
		translator::translate((p v ~q) => (r & k), Cs),
		Cs = [cl([r],[p]),cl([k],[p]),cl([q,r],[]),cl([q,k],[])].

	test(logic_2) :-
		translator::step_by_step(all(X, exists(Y, p(X) v ~q(X) => r(X, Y))), Cs),
		Y = f1(X), Cs = [cl([r(X, f1(X))], [p(X)]), cl([q(X), r(X, f1(X))], [])].

	test(logic_3) :-
		translator::step_by_step(all(X, men(X) => mortal(X)), Cs),
		Cs = [cl([mortal(X)], [men(X)])].

:- end_object.
