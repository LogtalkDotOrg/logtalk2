
:- object(benchmarks).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2007/04/17,
		comment is 'Benchmark utility predicates.']).

	:- public(run/1).

	:- public(run/2).

	:- public(benchmark/2).

	% run all benchmark tests:
	run(N) :-
		benchmark(Id, Goal),
		run(Id, N, Looptime, Goaltime, Average, Speed),
		report(Id, Goal, N, Looptime, Goaltime, Average, Speed),
		fail.
	run(_).

	% run a specific benchmark test:
	run(Id, N) :-
		benchmark(Id, Goal),
		run(Id, N, Looptime, Goaltime, Average, Speed),
		report(Id, Goal, N, Looptime, Goaltime, Average, Speed).

	run(Id, N, Looptime, Goaltime, Average, Speed) :-
		{'$lgt_cpu_time'(Seconds1)},		% defined in the config files
		do_benchmark(empty_loop, N),
		{'$lgt_cpu_time'(Seconds2)},
		Looptime is Seconds2 - Seconds1,
		{'$lgt_cpu_time'(Seconds3)},
		do_benchmark(Id, N),
		{'$lgt_cpu_time'(Seconds4)},
		Goaltime is Seconds4 - Seconds3,
		Average is (Goaltime - Looptime)/N,
		Speed is 1.0/Average.

	report(Id, Goal, N, Looptime, Goaltime, Average, Speed) :-
		write(Id), write(': '),
		writeq(Goal), nl,
		write('Number of repetitions: '), write(N), nl,
		write('Loop time: '), write(Looptime), nl,
		write('Goal time: '), write(Goaltime), nl,
		write('Average time per call: '), write(Average), nl,
		write('Number of calls per second: '), write(Speed), nl,
		nl.

	% some benchmark tests for static code:
	benchmark(s1, my_length(List, _)) :-
		{generate_list(20, List)}.
	benchmark(s2, object::length(List, _)) :-
		{generate_list(20, List)}.

	% some benchmark tests for dynamic code:
	benchmark(d1, (create_object(xpto, [], [], []), abolish_object(xpto))).
	benchmark(d2, db_test_plain).
	benchmark(d3, database::db_test_this).
	benchmark(d4, database::db_test_self).
	benchmark(d5, database::db_test_obj).

	% repeat a goal N times without using call/1 and using a failure-driven loop to 
	% avoid the interference of Prolog compiler memory management mechanism (such as 
	% garbage collection) on the results 
	do_benchmark(empty_loop, N) :-
		{repeat(N)},
			true,
		fail.
	do_benchmark(empty_loop, _).

	do_benchmark(s1, N) :-
		{generate_list(20, List)},
		{repeat(N)},
			{my_length(List, _)},
		fail.
	do_benchmark(s1, _).

	do_benchmark(s2, N) :-
		{generate_list(20, List)},
		{repeat(N)},
			object::length(List, _),
		fail.
	do_benchmark(s2, _).

	do_benchmark(d1, N) :-
		{repeat(N)},
			create_object(xpto, [], [], []),
			abolish_object(xpto),
		fail.
	do_benchmark(d1, _).

	do_benchmark(d2, N) :-
		{repeat(N)},
			{db_test_plain},
		fail.
	do_benchmark(d2, _).

	do_benchmark(d3, N) :-
		{repeat(N)},
			database::db_test_this,
		fail.
	do_benchmark(d3, _).

	do_benchmark(d4, N) :-
		{repeat(N)},
			database::db_test_self,
		fail.
	do_benchmark(d4, _).

	do_benchmark(d5, N) :-
		{repeat(N)},
			database::db_test_obj,
		fail.
	do_benchmark(d5, _).

:- end_object.
