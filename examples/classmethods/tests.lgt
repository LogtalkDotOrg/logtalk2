
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.1,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/04/26,
		comment is 'Unit tests for the "classmethods" example.']).

	:- uses(lgtunit, [op(700, xfx, '=~='), '=~='/2]).

	test(classmethods_1) :-
		circle::area(1.0, Area),
		Area =~= 3.14159.

	test(classmethods_2) :-
		c42::area(Area),
		Area =~= 24.6301.

	test(classmethods_3) :-
		circle::new(1.2, 7.9, 2.0, Circle),
		Circle::area(Area),
		Area =~= 4.52389.

:- end_object.
