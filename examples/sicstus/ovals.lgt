
:- object(math_constants).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Implements predicates for retriving common mathematical constants.']).

	:- public(pi/1).
	:- mode(pi(-float), one).

	:- public(e/1).
	:- mode(e(-float), one).

	pi(Pi) :-
		Pi is 4.0*atan(1.0).

	e(E) :-
		E is exp(1.0).

:- end_object.


:- object(ellipse(_RX, _RY, _Color),
	imports(proto_hierarchy)).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Parametric object for representing geometric ellipses.',
		parnames is ['RX', 'RY', 'Color'],
		source is 'Example adopted from the SICStus Objects documentation.']).

	:- uses(math_constants).

	:- public(color/1).
	:- mode(color(?atom), zero_or_one).
	:- info(color/1, [
		comment is 'Ellipse color.',
		argnames is ['Color']]).

	:- public(rx/1).
	:- mode(rx(?number), zero_or_one).
	:- info(rx/1, [
		comment is 'Ellipse x axis.',
		argnames is ['Rx']]).

	:- public(ry/1).
	:- mode(ry(?number), zero_or_one).
	:- info(ry/1, [
		comment is 'Ellipse y axis.',
		argnames is ['Ry']]).

	:- public(area/1).
	:- mode(area(-number), one).
	:- info(area/1, [
		comment is 'Ellipse area.',
		argnames is ['Area']]).

	:- public(context/0).
	:- mode(context, one).
	:- info(context/0, [
		comment is 'Shows execution context (self, this and sender values).']).

	color(Color) :-
		parameter(3, Color).

	rx(Rx) :-
		parameter(1, Rx).

	ry(Ry) :-
		parameter(2, Ry).

	area(Area) :-
		::rx(Rx),
		::ry(Ry),
		math_constants::pi(Pi),
		Area is Rx*Ry*Pi.

	context :-
		write(ellipse3), nl,
		self(Self), write('self: '), writeq(Self), nl,
		this(This), write('this: '), writeq(This), nl,
		sender(Sender), write('sender: '), writeq(Sender), nl, nl.

:- end_object.


:- object(circle(Radius, Color),
	extends(ellipse(Radius, Radius, Color))).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Parametric object for representing geometric circles.',
		parnames is ['Radius', 'Color'],
		source is 'Example adopted from the SICStus Objects documentation.']).

	:- public(r/1).
	:- mode(r(?number), zero_or_one).
	:- info(r/1, [
		comment is 'Circle radius.',
		argnames is ['Radius']]).

	r(Radius) :-
		parameter(1, Radius).

	color(Color) :-
		parameter(2, Color).

	rx(Radius) :-
		::r(Radius).

	ry(Radius) :-
		::r(Radius).

	context :-
		write(circle2), nl,
		self(Self), write('self: '), writeq(Self), nl,
		this(This), write('this: '), writeq(This), nl,
		sender(Sender), write('sender: '), writeq(Sender), nl, nl,
		^^context.

:- end_object.


:- object(circle1(Color),
	extends(circle(1, Color))).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Parametric object for representing geometric circles with radius = 1.',
		parnames is ['Color'],
		source is 'Example adopted from the SICStus Objects documentation.']).

	context :-
		write(circle11), nl,
		self(Self), write('self: '), writeq(Self), nl,
		this(This), write('this: '), writeq(This), nl,
		sender(Sender), write('sender: '), writeq(Sender), nl,
		^^context.

:- end_object.


:- object(red_circle(Radius),
	extends(circle(Radius, red))).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Parametric object for representing geometric red circles.',
		parnames is ['Radius'],
		source is 'Example adopted from the SICStus Objects documentation.']).

	context :-
		write(red_circle1), nl,
		self(Self), write('self: '), writeq(Self), nl,
		this(This), write('this: '), writeq(This), nl,
		sender(Sender), write('sender: '), writeq(Sender), nl, nl,
		^^context.

:- end_object.
