
% benchmark a goal using the default number of repetitions and printing some 
% useful statistics

benchmark(Goal) :-
	N = 1000000,
	write('Number of repetitions: '), write(N), nl,
	'$lgt_cpu_time'(Seconds1),
	benchmark(N, Goal),
	'$lgt_cpu_time'(Seconds2),
	Average is (Seconds2 - Seconds1)/N,
	write('Average time: '), write(Average), nl,
	Speed is 1.0/Average,
	write('Calls per second: '), write(Speed), nl.


% repeat a goal N times using a failure-driven loop to avoid the interference 
% of Prolog compiler memory management mechanism (such as garbage collection) 
% on the results

benchmark(N, Goal) :-
	repeat(N),
		call(Goal),
	fail.

benchmark(_, _).


% some Prolog compilers define the predicate repeat/1 as a built-in predicate;
% if that's the case of the Prolog compiler you are using, then comment out 
% the definition that follows

repeat(0) :-
	!.

repeat(N) :-
	N2 is N - 1,
	repeat(N2).
