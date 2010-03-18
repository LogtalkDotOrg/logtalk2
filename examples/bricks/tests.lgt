
:- object(tests,
	extends(lgtunit)).

	:- set_logtalk_flag(unknown, silent).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "bricks" example.']).

	test(bricks_1) :-
		brick::(new(a, [position-(8, 1)]), new(b, [position-(6, 1)]), new(c, [position-(4, 1)]), new(d, [position-(2, 1)])),
		brick_stack::(add_tuple([c,d]), add_tuple([b,c]), add_tuple([a,b])),
		findall(Tuple, brick_stack::tuple(Tuple), Tuples),
		list::msort(Tuples, TuplesSorted),
		TuplesSorted = [[a,b], [b,c], [c,d]].

	test(bricks_2) :-
		d::move(9, 1),
		a::position(Xa, Ya), b::position(Xb, Yb), c::position(Xc, Yc), d::position(Xd, Yd),
		Xa == 9,
		Xb == 9,
		Xc == 9,
		Xd == 9,
		Ya == 4,
		Yb == 3,
		Yc == 2,
		Yd == 1.

	test(bricks_3) :-
		findall(Tuple, brick_stack::tuple(Tuple), Tuples),
		list::msort(Tuples, TuplesSorted),
		TuplesSorted = [[a,b], [b,c], [c,d]].

	test(bricks_4) :-
		b::move(3, 1),
		a::position(Xa, Ya), b::position(Xb, Yb), c::position(Xc, Yc), d::position(Xd, Yd),
		Xa == 3,
		Xb == 3,
		Xc == 9,
		Xd == 9,
		Ya == 2,
		Yb == 1,
		Yc == 2,
		Yd == 1.

	test(bricks_5) :-
		findall(Tuple, brick_stack::tuple(Tuple), Tuples),
		list::msort(Tuples, TuplesSorted),
		TuplesSorted = [[a,b], [c,d]].

	test(bricks_6) :-
		brick_stack::add_tuple([d, a]),
		a::position(Xa, Ya), b::position(Xb, Yb), c::position(Xc, Yc), d::position(Xd, Yd),
		Xa == 3,
		Xb == 3,
		Xc == 3,
		Xd == 3,
		Ya == 2,
		Yb == 1,
		Yc == 4,
		Yd == 3.

	test(bricks_7) :-
		findall(Tuple, brick_stack::tuple(Tuple), Tuples),
		list::msort(Tuples, TuplesSorted),
		TuplesSorted = [[a,b], [c,d], [d,a]].

	test(bricks_8) :-
		b::move(5, 1),
		a::position(Xa, Ya), b::position(Xb, Yb), c::position(Xc, Yc), d::position(Xd, Yd),
		Xa == 5,
		Xb == 5,
		Xc == 5,
		Xd == 5,
		Ya == 2,
		Yb == 1,
		Yc == 4,
		Yd == 3.

	test(bricks_9) :-
		findall(Tuple, brick_stack::tuple(Tuple), Tuples),
		list::msort(Tuples, TuplesSorted),
		TuplesSorted = [[a,b], [c,d], [d,a]].

:- end_object.
