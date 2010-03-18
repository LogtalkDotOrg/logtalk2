
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "viewpoints" example.']).

	test(viewpoints_1) :-
		joePerson::age(Age),
		Age == 30.

	test(viewpoints_2) :-
		joeSportsman::age(Age),
		Age == 30.

	test(viewpoints_3) :-
		joePerson::getOlder,
		joeChessPlayer::age(Age),
		Age == 31.

	test(viewpoints_4) :-
		joeEmployee::getOlder,
		joePerson::age(Age),
		Age == 32.

	test(viewpoints_5) :-
		joePerson::score(Score),
		Score == 0.

	test(viewpoints_6) :-
		joeEmployee::score(Score),
		Score == 0.

	test(viewpoints_7) :-
		joeChessPlayer::(setScore(2200), score(Score)),
		Score == 2200.

	test(viewpoints_8) :-
		joePerson::score(Score),
		Score == 0.

	test(viewpoints_9) :-
		joeSportsman::score(Score),
		Score == 0.

:- end_object.
