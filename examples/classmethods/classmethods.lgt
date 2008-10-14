
:- object(metacircle,			% avoid infinite metaclass regression by
	instantiates(metacircle)).	% making the class its own metaclass

	:- public(new/4).
	:- mode(new(+float, +float, +float, -object_identifier), one).
	:- info(new/4, [
		comment is 'Creates a new circle in a given position and with a given radius.',
		argnames is ['X', 'Y', 'Radius', 'Cicle']]).

	new(Radius, X, Y, Circle) :-
		self(Self),
		create_object(Circle, [instantiates(Self)], [], [position(X, Y), radius(Radius)]).

	:- public(area/2).
	:- mode(area(+float, -float), one).
	:- info(area/2, [
		comment is 'Calculates the area of a circle given its radius.',
		argnames is ['Radius', 'Area']]).

	area(Radius, Area) :-
		Area is 4*atan(1.0)*Radius*Radius.

:- end_object.


:- object(circle,
	instantiates(metacircle)).

	:- public(position/2).
	:- mode(position(?float, ?float), zero_or_one).
	:- info(position/2, [
		comment is 'Circle position.',
		argnames is ['X', 'Y']]).

	position(0.0, 0.0).		% default position

	:- public(radius/1).
	:- mode(radius(?float), zero_or_one).
	:- info(radius/1, [
		comment is 'Circle radius.',
		argnames is ['Radius']]).

	radius(1.0).			% default radius

	:- public(area/1).
	:- mode(area(-float), one).
	:- info(area/1, [
		comment is 'Circle area.',
		argnames is ['Area']]).

	area(Area) :-
		::radius(Radius),
		Area is 4*atan(1.0)*Radius*Radius.

:- end_object.


:- object(c42,
	instantiates(circle)).

	position(3.7, 4.5).
	radius(2.8).

:- end_object.
