
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/01/14,
		comment is 'Unit tests for the "hailstone" example.']).

	test(hailstone_1) :-
		hailstone::generate_sequence(10, Sequence),
		Sequence == [10, 5, 16, 8, 4, 2, 1].

	test(hailstone_2) :-
		hailstone::write_sequence(10).

	test(hailstone_3) :-
		hailstone::sequence_length(27, Length),
		Length == 112.

	test(hailstone_4) :-
		hailstone::longest_sequence(1, 100000, N, Length),
		N == 77031,
		Length == 351.

:- end_object.
