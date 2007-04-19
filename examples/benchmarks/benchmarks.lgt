
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
	benchmark(d2, db_test_plain(_)).
	benchmark(d3, database::db_test_this(_)).
	benchmark(d4, database::db_test_self(_)).
	benchmark(d5, database::db_test_obj(_)).

	% repeat a goal N times without using call/1 and using a failure-driven loop to 
	% avoid the interference of Prolog compiler memory management mechanism (such as 
	% garbage collection) on the results 
	do_benchmark(empty_loop, N) :-
		{my_repeat(N)},
			true,
		fail.
	do_benchmark(empty_loop, _).

	do_benchmark(s1, N) :-
		{generate_list(20, List)},
		{my_repeat(N)},
			{my_length(List, _)},
		fail.
	do_benchmark(s1, _).

	do_benchmark(s2, N) :-
		{generate_list(20, List)},
		{my_repeat(N)},
			object::length(List, _),
		fail.
	do_benchmark(s2, _).

	do_benchmark(d1, N) :-
		{my_repeat(N)},
			create_object(xpto, [], [], []),
			abolish_object(xpto),
		fail.
	do_benchmark(d1, _).

	do_benchmark(d2, N) :-
		{my_repeat(N)},
			{db_test_plain(N)},
		fail.
	do_benchmark(d2, _).

	do_benchmark(d3, N) :-
		{my_repeat(N)},
			database::db_test_this(N),
		fail.
	do_benchmark(d3, _).

	do_benchmark(d4, N) :-
		{my_repeat(N)},
			database::db_test_self(N),
		fail.
	do_benchmark(d4, _).

	do_benchmark(d5, N) :-
		{my_repeat(N)},
			database::db_test_obj(N),
		fail.
	do_benchmark(d5, _).

:- end_object.
