
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.1,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/04/26,
		comment is 'Unit tests for the "proxies" example.']).

	:- uses(lgtunit, [op(700, xfx, '=~='), '=~='/2]).

	test(proxies_1) :-
		{circle('#2', Radius, Color)}::print,
		Radius =~= 3.71,
		Color == yellow.

	test(proxies_2) :-
		findall(Area, {circle(_, _, _)}::area(Area), Areas),
		Areas = [Area1, Area2, Area3, Area4, Area5],
		Area1 =~= 4.75291552561599,
		Area2 =~= 43.2411954432752,
		Area3 =~= 0.477836242611007,
		Area4 =~= 103.507938113415,
		Area5 =~= 217.468583303854.

:- end_object.
