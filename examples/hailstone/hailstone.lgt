
:- object(hailstone).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/01/14,
		comment is 'Hailstone sequence. Coded for a contribution to the Rosetta Stone website.'
	]).

	:- public(sequence/1).
	:- mode(sequence(+natural), zero_or_one).
	:- info(sequence/1, [
		comment is 'Writes to the standard output the Hailstone sequence that start with its argument. Fails if the argument is not a natural number.',
		argnames is ['Start']
	]).

	:- public(length/2).
	:- mode(length(+natural, -natural), zero_or_one).
	:- info(length/2, [
		comment is 'Calculates the length of the Hailstone sequence that start with its argument. Fails if the argument is not a natural number.',
		argnames is ['Start', 'Length']
	]).

	:- public(longest/4).
	:- mode(longest(+natural, +natural, -natural, -natural), zero_or_one).
	:- info(length/4, [
		comment is 'Calculates the longest Hailstone sequence in the interval [Start, End]. Fails if the interval is not valid.',
		argnames is ['Start', 'End', 'N', 'Length']
	]).

	sequence(Start) :-
		integer(Start),
		Start >= 1,
		write_sequence(Start).

	write_sequence(1) :-
		!,
		write(1), nl. 
	write_sequence(N) :-
		write(N), write(' '),
		(	N mod 2 =:= 0 ->
			M is N // 2
		;	M is (3 * N) + 1
		),
		write_sequence(M).

	length(Start, Length) :-
		integer(Start),
		Start >= 1,
		calculate_length(Start, 1, Length).

	calculate_length(1, Length, Length) :-
		!.
	calculate_length(N, Length0, Length) :-
		Length1 is Length0 + 1,
		(	N mod 2 =:= 0 ->
			M is N // 2
		;	M is (3 * N) + 1
		),
		calculate_length(M, Length1, Length).

	longest(Start, End, N, Length) :-
		integer(Start),
		integer(End),
		Start >= 1,
		Start =< End,
		longest_sequence(Start, End, 1, N, 1, Length).

	longest_sequence(Current, End, N, N, Length, Length) :-
		Current > End,
		!.
	longest_sequence(Current, End, N0, N, Length0, Length) :-
		length(Current, CurrentLength),
		Next is Current + 1,
		(	CurrentLength > Length0 ->
			longest_sequence(Next, End, Current, N, CurrentLength, Length)
		;	longest_sequence(Next, End, N0, N, Length0, Length)
		).

:- end_object.
