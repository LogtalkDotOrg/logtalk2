
:- object(primes(_Threads)).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2008/2/29,
		comment is 'Simple example for comparing single and multi-threading calculation of prime numbers.',
		parameters is ['Threads'- 'Number of threads to use.']]).

	:- threaded.

	:- public(primes_custom/3).
	:- mode(primes_custom(+integer, +integer, -list), one).
	:- info(primes_custom/3, [
		comment is 'Returns a list of all prime numbers in the given interval. Valid number of threads are 1, 2, 4, and 8. Custom code is used for each valid number of threads.',
		argnames is ['Inf', 'Sup', 'Primes']]).

	:- public(primes_spawn/3).
	:- mode(primes_spawn(+integer, +integer, -list), one).
	:- info(primes_spawn/3, [
		comment is 'Returns a list of all prime numbers in the given interval. No restrictions on the number of threads to use besides those imposed by the backend Prolog compiler.',
		argnames is ['Inf', 'Sup', 'Primes']]).

	primes_spawn(N, M, Primes) :-
		parameter(1, Threads),
		M > N,
		(M - N + 1) mod Threads =:= 0,
		Size is (M - N + 1) // Threads,
		split(N, M, Size, Intervals),
		spawn(Intervals, [], Primes, Goals),
		collect(Goals).

	split(N, M, _, []) :-
		M < N,
		!.
	split(N, M, Size, [Inf-M| Intervals]) :-
		Sup is M - Size,
		Inf is Sup + 1,
		split(N, Sup, Size, Intervals).

	spawn([], Primes, Primes, []).
	spawn([N-M| Intervals], Acc, Primes, [prime_numbers(N, M, Acc, Acc2)| Goals]) :-
		threaded_once(prime_numbers(N, M, Acc, Acc2)),
		spawn(Intervals, Acc2, Primes, Goals).

	collect([]).
	collect([prime_numbers(N, M, Acc, Primes)| Goals]) :-
		threaded_exit(prime_numbers(N, M, Acc, Primes)),
		collect(Goals).

	primes_custom(N, M, Primes) :-
		parameter(1, Threads),
		M > N,
		(M - N + 1) mod Threads =:= 0,
		primes_custom(Threads, N, M, Primes).

	primes_custom(1, N, M, Primes) :-
		primes_custom_1(N, M, Primes).
	primes_custom(2, N, M, Primes) :-
		primes_custom_2(N, M, Primes).
	primes_custom(4, N, M, Primes) :-
		primes_custom_4(N, M, Primes).
	primes_custom(8, N, M, Primes) :-
		primes_custom_8(N, M, Primes).

	primes_custom_1(N, M, Primes) :-
		M > N,
		prime_numbers(N, M, [], Primes).

	primes_custom_2(N, M, Primes) :-
		M > N,
		N1 is N + (M - N) // 2, N2 is N1 + 1,
		threaded((
			prime_numbers(N2, M,  [],  Acc),
			prime_numbers(N,  N1, Acc, Primes))).

	primes_custom_4(N, M, Primes) :-
		M > N,
		N3 is N  + (M  - N ) // 2, N4 is N3 + 1,
		N1 is N  + (N3 - N ) // 2, N2 is N1 + 1,
		N5 is N4 + (M  - N4) // 2, N6 is N5 + 1,
		threaded((
			prime_numbers(N6, M,  [],   Acc1),
			prime_numbers(N4, N5, Acc1, Acc2),
			prime_numbers(N2, N3, Acc2, Acc3),
			prime_numbers(N,  N1, Acc3, Primes))).

	primes_custom_8(N, M, Primes) :-
		M > N,
		N7  is N   + (M   - N  ) // 2, N8  is N7  + 1,
		N3  is N   + (N7  - N  ) // 2, N4  is N3  + 1,
		N1  is N   + (N3  - N  ) // 2, N2  is N1  + 1,
		N5  is N4  + (N7  - N4 ) // 2, N6  is N5  + 1,
		N11 is N8  + (M   - N8 ) // 2, N12 is N11 + 1,
		N9  is N8  + (N11 - N8 ) // 2, N10 is N9  + 1,
		N13 is N12 + (M   - N12) // 2, N14 is N13 + 1,
		threaded((
			prime_numbers(N14, M,   [],   Acc1),
			prime_numbers(N12, N13, Acc1, Acc2),
			prime_numbers(N10, N11, Acc2, Acc3),
			prime_numbers(N8,  N9,  Acc3, Acc4),
			prime_numbers(N6,  N7,  Acc4, Acc5),
			prime_numbers(N4,  N5,  Acc5, Acc6),
			prime_numbers(N2,  N3,  Acc6, Acc7),
			prime_numbers(N,   N1,  Acc7, Primes))).

	prime_numbers(N, M, Primes, Primes) :-
		N > M,
		!.
	prime_numbers(N, M, Acc, Primes) :-
		(	is_prime(N) ->
			Primes = [N| Primes2]
		;	Primes = Primes2),
	    N2 is N + 1,
		prime_numbers(N2, M, Acc, Primes2).

	is_prime(2) :- !.
	is_prime(Prime):-
		Prime > 2,
		Prime mod 2 =:= 1,
		Sqrt is sqrt(Prime),
		is_prime(3, Sqrt, Prime).

	is_prime(N, Sqrt, Prime):-
		(	N > Sqrt ->
			true
		;	Prime mod N > 0,
 			N2 is N + 2,
			is_prime(N2, Sqrt, Prime)
		).

:- end_object.
