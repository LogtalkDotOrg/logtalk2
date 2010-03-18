
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "classmethods" example.']).

	test(classmethods_1) :-
		circle::area(1.0, Area),
		Error is abs(Area - 3.14159),
		Error < 0.0001.

	test(classmethods_2) :-
		c42::area(Area),
		Error is abs(Area - 24.6301),
		Error < 0.0001.

	test(classmethods_3) :-
		circle::new(1.2, 7.9, 2.0, Circle),
		Circle::area(Area),
		Error is abs(Area - 4.52389),
		Error < 0.0001.

:- end_object.
