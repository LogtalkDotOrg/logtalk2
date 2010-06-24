
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2010/06/24,
		comment is 'Unit tests for the "constraints/gnu" example.']).

	test(alpha) :-
		alpha::q(LD, _),
		LD == [5,13,9,16,20,4,24,21,25,17,23,2,8,12,10,19,7,11,15,3,1,26,6,22,14,18].

	test(donald) :-
		donald::q(LD, _),
		LD == [5,2,6,4,8,1,9,7,3,0].

:- end_object.
