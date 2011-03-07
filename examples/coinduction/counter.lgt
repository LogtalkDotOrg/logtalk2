
:- object(counter).

	:- info([
		version is 0.1,
		author is 'Gopal Gupta et al. Adapted to Logtalk by Paulo Moura.',
		date is 2010/07/23,
		comment is 'Module four counter coinductive example.']).

	:- public(verify/0).

	verify :-
		\+ (sm1(-1, X), lists::comember(sm1, X)).

	:- coinductive(sm1/2).
	sm1(N, [sm1| T]) :- N1 is (N + 1) mod 4, s0(N1, T), N1 >= 0.

	:- coinductive(s0/2).
	s0(N, [s0| T]) :- N1 is (N + 1) mod 4, s1(N1, T), N1 >= 0.

	:- coinductive(s1/2).
	s1(N, [s1| T]) :- N1 is (N + 1) mod 4, s2(N1, T), N1 >= 0.

	:- coinductive(s2/2).
	s2(N, [s2| T]) :- N1 is (N + 1) mod 4, s3(N1, T), N1 >= 0.

	:- coinductive(s3/2).
	s3(N, [s3| T]) :- N1 is (N + 1) mod 4, s0(N1, T), N1 >= 0.

:- end_object.
