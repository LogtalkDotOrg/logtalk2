
:- object(sieve).

	:- info([
		version is 1.0,
		author is 'Gopal Gupta et al. Adapted to Logtalk by Paulo Moura.',
		date is 2011/07/04,
		comment is 'Sieve of Eratosthenes coinduction example.']).

	:- public(primes/2).
	primes(N, Primes) :-
		generate_infinite_list(N, List),
		sieve(List, Primes).

	generate_infinite_list(N, List) :-
		sequence(2, N, List, List).

	sequence(Sup, Sup, [Sup| List], List) :-
		!.
	sequence(Inf, Sup, [Inf| List], Tail) :-
		Next is Inf + 1,
		sequence(Next, Sup, List, Tail).

	:- coinductive(sieve/2).
	sieve([H| T], [H| R]) :-
		filter(H, T, F),
		sieve(F, R).

	:- coinductive(filter/3).
	filter(H, [K| T], L) :-
		(	K > H, K mod H =:= 0 ->
			L = T1
		;	L = [K| T1]
		),
		filter(H, T, T1).

:- end_object.
