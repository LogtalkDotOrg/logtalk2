
:- object(primes(_Threads)).

	:- info([
		version is 1.3,
		author is 'Paulo Moura',
		date is 2007/12/20,
		comment is 'Simple example for comparing single and multi-threading calculation of prime numbers.',
		parameters is ['Threads'- 'Number of threads to use. Valid values are 1, 2, 4, and 8.']]).

	:- threaded.

	:- public(primes/3).
	:- mode(primes(+integer, +integer, -list), one).
	:- info(primes/3, [
		comment is 'Returns a list of all prime numbers in the given interval.',
		argnames is ['Inf', 'Sup', 'Primes']]).


	primes(N, M, Primes) :-
		parameter(1, Threads),
		primes(Threads, N, M, Primes).

	primes(1, N, M, Primes) :-
		st_primes(N, M, Primes).
	primes(2, N, M, Primes) :-
		mt_primes_2(N, M, Primes).
	primes(4, N, M, Primes) :-
		mt_primes_4(N, M, Primes).
	primes(8, N, M, Primes) :-
		mt_primes_8(N, M, Primes).

	st_primes(N, M, Primes) :-
		M > N,
		prime_numbers(N, M, [], Primes).

	mt_primes_2(N, M, Primes) :-
		M > N,
		N1 is N + (M - N) // 2, N2 is N1 + 1,
		threaded((
			prime_numbers(N2, M,  [],  Acc),
			prime_numbers(N,  N1, Acc, Primes))).

	mt_primes_4(N, M, Primes) :-
		M > N,
		N3 is N  + (M  - N ) // 2, N4 is N3 + 1,
		N1 is N  + (N3 - N ) // 2, N2 is N1 + 1,
		N5 is N4 + (M  - N4) // 2, N6 is N5 + 1,
		threaded((
			prime_numbers(N6, M,  [],   Acc1),
			prime_numbers(N4, N5, Acc1, Acc2),
			prime_numbers(N2, N3, Acc2, Acc3),
			prime_numbers(N,  N1, Acc3, Primes))).

	mt_primes_8(N, M, Primes) :-
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
