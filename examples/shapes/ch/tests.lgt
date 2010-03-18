
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "shapes_ch" example.']).

	throws(ch_1, error(existence_error(_,_),_,_)) :-
		square::nsides(_).

	test(ch_2) :-
		q1::(color(Color), side(Side), position(X, Y)),
		Color == red, Side == 1, X == 0, Y == 0.

	test(ch_3) :-
		q2::(side(Side), area(Area), perimeter(Perimeter)),
		Side == 3, Area == 9, Perimeter == 12.

:- end_object.
