
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "parametric" example.']).

	test(parametric_1) :-
		findall(X, [1, 2, 3]::member(X), Solutions),
		Solutions = [1,2,3].

	test(parametric_2) :-
		findall(X, [1, 2, 3]::last(X), Solutions),
		Solutions = [3].

	test(parametric_3) :-
		findall(X, [1, 2, 3]::nextto(2,X), Solutions),
		Solutions = [3].

	test(parametric_4) :-
		\+ []::member(_).

	test(parametric_5) :-
		rectangle(W, H, X, Y)::(init, move(3, 4, NR)), NR::position(X2, Y2),
		W  == 2,
		H  == 1,
		X  == 0,
		Y  == 0,
		NR == rectangle(2, 1, 3, 4),
		X2 == 3,
		Y2 == 4.

	test(parametric_6) :-
		person(sally, 20)::grow_older(NewId),
		NewId = person(sally, 21).

	test(parametric_7) :-
		employee(sally, 21, 1200)::give_raise(250, NewId),
		NewId = employee(sally, 21, 1450).

:- end_object.
