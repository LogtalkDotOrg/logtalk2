
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "proxies" example.']).

/*
	test(proxies_1) :-
		findall(Area, {circle(_, _, _)}::area(Area), Areas),
		Areas = [4.75291, 43.2412, 0.477836, 103.508, 217.468].
*/

:- end_object.
