
:- object(nested).

	:- info([
		version is 0.1,
		author is 'Gopal Gupta et al. Adapted to Logtalk by Paulo Moura.',
		date is 2010/07/23,
		comment is 'Nested automata example.']).

	:- public(state/2).
	:- coinductive(state/2).

	state(s0, [s0, is1| T]) :- enter, work, state(s1, T).
	state(s1, [s1| T]) :-      exit, state(s2, T).
	state(s2, [s2| T]) :-      repeat, state(s0, T).
	state(s0, [s0| T]) :-      error, state(s3, T).
	state(s3, [s3| T]) :-      repeat, state(s0, T).

	work :- work.
	work.

	enter.	repeat.
	exit.	error.

:- end_object.
