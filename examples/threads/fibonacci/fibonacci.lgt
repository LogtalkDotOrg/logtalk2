
:- object(fibonacci(_Threads)).

	:- info([
		version is 1.0,
		date is 2007/12/19,
		author is 'Paulo Moura',
		comment is 'Multi-threaded version of the computation of Fibonacci numbers.',
		parameters is ['Threads'- 'Number of threads to use. Valid values are 1, 2, 4, 8, 16, etc.']]).

	:- threaded.

	:- public(fib/2).
	:- mode(fib(+integer, -integer), one).
	:- info(fib/2, [
		comment is 'Calculates the Nth Fibonacci number.',
		argnames is ['Nth', 'Number']]).

	fib(N, F) :-
		parameter(1, Threads),
		M is Threads//2 + 1,
		mt_fib(M, N, F),
		!.

	mt_fib(_, 0, 1).
	mt_fib(_, 1, 1).
	mt_fib(1, N, F) :-
		st_fib(N, F).
	mt_fib(Forks, N, F) :-
		Forks > 1,
		Forks1 is Forks//2,
		Forks2 is Forks - Forks1,
		N > 1,
		N1 is N - 1,
		N2 is N - 2,
		threaded((
			mt_fib(Forks1, N1, F1),
			mt_fib(Forks2, N2, F2)
		)),
		F is F1 + F2.

	st_fib(0, 1).
	st_fib(1, 1).
	st_fib(N,F) :-
		N > 1,
		N1 is N - 1,
		N2 is N - 2,
		st_fib(N1, F1),
		st_fib(N2, F2),
		F is F1 + F2.

:- end_object.
