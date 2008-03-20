
:- object(mtbatch(_Prolog)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2008/03/04,
		comment is 'Multi-threading benchmarks.',
		parameters is ['Prolog'- 'Prolog backend compiler. Supported compilers are SWI-Prolog (swi), YAP (yap), and XSB (xsb).']]).

	:- uses(integer, [between/3]).

	:- public(run/0).
	:- mode(run, one).
	:- info(run/0, [
		comment is 'Runs all benchmarks the default number of times.']).

	:- public(run/1).
	:- mode(run(+integer), one).
	:- info(run/1, [
		comment is 'Runs all benchmarks the specified number of times.',
		argnames is ['N']]).

	:- public(run/2).
	:- mode(run(+atom, +integer), one).
	:- info(run/2, [
		comment is 'Runs a specific benchmark the specified number of times.',
		argnames is ['Id', 'N']]).

	% run all benchmarks the default number of times:
	run :-
		run(10).

	% run all benchmark tests N times:
	run(N) :-
		run(_, N),
		fail.
	run(_).

	% prime numbers benchmarks:
	run(primes, N) :-
		write('Prime numbers (average of '), write(N), write(' runs)'), nl,
		loop::forto(Threads, 1, 16,
			(	put_char('\t'), write(Threads)
			)), nl,
		loop::forto(S, 1, 10,
			(	Size is S*10000,
				write(Size),
				loop::forto(Threads, 1, 16,
					(	run(primes(Threads, Size), N, Average),
						put_char('\t'), write_float(Average), flush_output
					)), nl
			)), nl.

	% merge sort benchmarks:
	run(msort, N) :-
		write('Merge sort (average of '), write(N), write(' runs)'), nl,
		loop::forto(T, 0, 4,
			(	Threads is truncate(2**T),
				put_char('\t'), write(Threads)
			)), nl,
		loop::forto(S, 1, 10,
			(	Size is S*5000,
				write(Size),
				generator::list(Size, List),
				loop::forto(T, 0, 4,
					(	Threads is truncate(2**T),
						run(msort(Threads, List), N, Average),
						put_char('\t'), write_float(Average), flush_output
					)), nl
			)), nl.

	% quicksort benchmarks:
	run(qsort, N) :-
		write('Quicksort (average of '), write(N), write(' runs)'), nl,
		loop::forto(T, 0, 4,
			(	Threads is truncate(2**T),
				put_char('\t'), write(Threads)
			)), nl,
		loop::forto(S, 1, 10,
			(	Size is S*5000,
				write(Size),
				generator::list(Size, List),
				loop::forto(T, 0, 4,
					(	Threads is truncate(2**T),
						run(qsort(Threads, List), N, Average),
						put_char('\t'), write_float(Average), flush_output
					)), nl
			)), nl.

	% Fibonacci numbers benchmarks:
	run(fib, N) :-
		write('Fibonacci numbers (average of '), write(N), write(' runs)'), nl,
		loop::forto(T, 0, 4,
			(	Threads is truncate(2**T),
				put_char('\t'), write(Threads)
			)), nl,
		loop::forto(Nth, 20, 27,
			(	write(Nth),
				loop::forto(T, 0, 4,
					(	Threads is truncate(2**T),
						run(fibonacci(Threads, Nth), N, Average),
						put_char('\t'), write_float(Average), flush_output
					)), nl
			)), nl.

	% Towers of Hanoi benchmarks:
	run(hanoi, N) :-
		write('Towers of Hanoi (average of '), write(N), write(' runs)'), nl,
		loop::forto(T, 0, 4,
			(	Threads is truncate(2**T),
				put_char('\t'), write(Threads)
			)), nl,
		loop::forto(Disks, 20, 27,
			(	write(Disks),
				loop::forto(T, 0, 4,
					(	Threads is truncate(2**T),
						run(hanoi(Threads, Disks), N, Average),
						put_char('\t'), write_float(Average), flush_output
					)), nl
			)), nl.

	% Takeuchi function benchmarks:
	run(tak, N) :-
		write('Takeuchi function (average of '), write(N), write(' runs)'), nl,
		loop::forto(T, 0, 5,
			(	Threads is truncate(3**T),
				put_char('\t'), write(Threads)
			)), nl,
		write((21,14,7)),
		loop::forto(T, 0, 5,
			(	Threads is truncate(3**T),
				run(tak(Threads, 21, 14, 7), N, Average),
				put_char('\t'), write_float(Average), flush_output
			)), nl.

	% fast Fourier transform benchmarks:
	run(fft, N) :-
		write('Fast Fourier transform (average of '), write(N), write(' runs)'), nl,
		loop::forto(T, 0, 4,
			(	Threads is truncate(2**T),
				put_char('\t'), write(Threads)
			)), nl,
		loop::forto(S, 12, 16,
			(	Size is truncate(2**S),
				write(Size),
				cgenerator::list(Size, List),
				loop::forto(T, 0, 4,
					(	Threads is truncate(2**T),
						run(fft(Threads, 8, List), N, Average),
							put_char('\t'), write_float(Average), flush_output
					)), nl
			)), nl.


	% integration benchmarks:
	run(integration, N) :-
		write('Numerical integration using recursive calls to threaded/1 (average of '), write(N), write(' runs)'), nl,
		loop::forto(T, 0, 4,
			(	Threads is truncate(2**T),
				put_char('\t'), write(Threads)
			)), nl,
		forall(
			(Function = f1; Function = f2; Function = f3 /*Function = f4; Function = f5; Function = f6*/),
			(	write(Function),
				loop::forto(T, 0, 4,
					(	Threads is truncate(2**T),
						run(quadrec(Threads, Function), N, Average),
						put_char('\t'), write_float(Average), flush_output
					)), nl
			)), nl,
		write('Numerical integration using a thread for each integration sub-interval (average of '), write(N), write(' runs)'), nl,
		forall(
			(Function = f1; Function = f2; Function = f3 /*Function = f4; Function = f5; Function = f6*/),
			(	write(Function),
				loop::forto(T, 0, 4,
					(	Threads is truncate(2**T),
						run(quadsplit(Threads, Function), N, Average),
						put_char('\t'), write_float(Average), flush_output
					)), nl
			)), nl.

	run(Id, N, Average) :-
		walltime_begin(Walltime1),
		do_benchmark(empty_loop, N),
		walltime_end(Walltime2),
		Looptime is Walltime2 - Walltime1,
		walltime_begin(Walltime3),
		do_benchmark(Id, N),
		walltime_end(Walltime4),
		Goaltime is Walltime4 - Walltime3,
		Average is (Goaltime - Looptime)/N.

	% repeat a goal N times without using call/1 and using a failure-driven loop to 
	% avoid the interference of Prolog compiler memory management mechanism (such as 
	% garbage collection) on the results 
	do_benchmark(empty_loop, N) :-
		repeat(N),
		fail.
	do_benchmark(empty_loop, _).

	do_benchmark(primes(Threads, Size), N) :-
		repeat(N),
			primes(Threads)::primes(1, Size, _),
		fail.
	do_benchmark(primes(_, _), _).

	do_benchmark(msort(Threads, List), N) :-
		repeat(N),
			msort(Threads)::msort(List, _),
		fail.
	do_benchmark(msort(_, _), _).

	do_benchmark(qsort(Threads, List), N) :-
		repeat(N),
			qsort(Threads)::qsort(List, _),
		fail.
	do_benchmark(qsort(_, _), _).

	do_benchmark(fibonacci(Threads, Nth), N) :-
		repeat(N),
			fibonacci(Threads)::fib(Nth, _),
		fail.
	do_benchmark(fibonacci(_, _), _).

	do_benchmark(hanoi(Threads, Disks), N) :-
		repeat(N),
			hanoi(Threads)::run(Disks),
		fail.
	do_benchmark(hanoi(_, _), _).

	do_benchmark(tak(Threads, A, B, C), N) :-
		repeat(N),
			tak(Threads)::tak(A, B, C, _),
		fail.
	do_benchmark(tak(_, _, _, _), _).

	do_benchmark(fft(Threads, Size, List), N) :-
		repeat(N),
			fft(Threads)::fft(Size, List, _),
		fail.
	do_benchmark(fft(_, _, _), _).

	do_benchmark(quadrec(Threads, Function), N) :-
		repeat(N),
			quadrec(Threads)::integrate(Function, 0, 4, 4, 1.0e-15, _),
		fail.
	do_benchmark(quadrec(_, _), _).

	do_benchmark(quadsplit(Threads, Function), N) :-
		repeat(N),
			quadsplit(Threads)::integrate(Function, 0, 4, 4, 1.0e-15, _),
		fail.
	do_benchmark(quadsplit(_, _), _).

	walltime_begin(Walltime) :-
		parameter(1, Prolog),
		walltime_begin(Prolog, Walltime).

	walltime_begin(swi, Walltime) :-
		get_time(Walltime).
	walltime_begin(yap, 0.0) :-
		statistics(walltime, _).
	walltime_begin(xsb, Walltime) :-
		walltime(Walltime).

	walltime_end(Walltime) :-
		parameter(1, Prolog),
		walltime_end(Prolog, Walltime).

	walltime_end(swi, Walltime) :-
		get_time(Walltime).
	walltime_end(yap, Walltime) :-
		statistics(walltime, [_, Walltime]).
	walltime_end(xsb, Walltime) :-
		walltime(Walltime).

	repeat(_).
	repeat(N) :-
		N > 1,
		N2 is N - 1,
		repeat(N2).

	write_float(Float) :-
		parameter(1, Prolog),
		write_float(Prolog, Float).

	write_float(swi, Float) :-
		format('~6f', [Float]).
	write_float(yap, Float) :-
		format('~6f', [Float]).
	write_float(xsb, Float) :-
		fmt_write("%6f", Float).

:- end_object.
