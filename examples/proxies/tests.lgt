
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "proxies" example.']).

	test(proxies_1) :-
		{circle('#2', Radius, Color)}::print,
		abs(Radius - 3.71) < 0.001,
		Color == yellow.

	test(proxies_2) :-
		findall(Area, {circle(_, _, _)}::area(Area), Areas),
		Areas = [Area1, Area2, Area3, Area4, Area5],
		abs(Area1 - 4.75291)  < 0.001,
		abs(Area2 - 43.2412)  < 0.001,
		abs(Area3 - 0.477836) < 0.001,
		abs(Area4 - 103.508)  < 0.001,
		abs(Area5 - 217.468)  < 0.001.

:- end_object.
