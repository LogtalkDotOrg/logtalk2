
:- object(shape,
	instantiates(abstract_class),
	specializes(object)).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2003/2/3,
		comment is 'Generic geometric shape.']).

	:- public(color/1).
	:- mode(color(?atom), zero_or_one).
	:- info(color/1, [
		comment is 'Shape color.',
		argnames is ['Color']]).

	:- public(position/2).
	:- mode(position(?integer, ?integer), zero_or_one).
	:- info(position/2, [
		comment is 'Shape position.',
		argnames is ['X', 'Y']]).

	color(red).      % default shape color

	position(0, 0).  % default shape position

:- end_object.


:- object(polygon,
    instantiates(abstract_class),
    specializes(shape)).

	:- info([
		author is 'Paulo Moura',
		version is 1.1,
		date is 2004/1/8,
		comment is 'Generic polygon.']).

	:- public(nsides/1).
	:- mode(nsides(?integer), zero_or_one).
	:- info(nsides/1, [
		comment is 'Polygon number of sides.',
		argnames is ['Number']]).

	:- public(area/1).
	:- mode(area(-float), zero_or_one).
	:- info(area/1, [
		comment is 'Polygon area.',
		argnames is ['Area']]).

	:- public(perimeter/1).
	:- mode(perimeter(?atom), zero_or_one).
	:- info(perimeter/1, [
		comment is 'Polygon perimeter.',
		argnames is ['Perimeter']]).

:- end_object.


:- object(regular_polygon,
    instantiates(abstract_class),
    specializes(polygon)).

	:- info([
		author is 'Paulo Moura',
		version is 1.1,
		date is 2004/1/8,
		comment is 'Generic regular polygon.']).

	:- public(side/1).
	:- mode(side(?atom), zero_or_one).
	:- info(side/1, [
		comment is 'Regular polygon side length.',
		argnames is ['Length']]).

	side(1).         % default side length

	perimeter(Perimeter) :-
		::nsides(Number),
		::side(Side),
		Perimeter is Number*Side.

:- end_object.


:- object(square,
    instantiates(class),
    specializes(regular_polygon)).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2003/2/3,
		comment is 'Geometric square.']).

	nsides(4).

	area(Area) :-
		::side(Side),
		Area is Side*Side.

:- end_object.


:- object(q1,
    instantiates(square)).

:- end_object.


:- object(q2,
    instantiates(square)).

	position(2, 3).

	color(blue).

	side(3).

:- end_object.
