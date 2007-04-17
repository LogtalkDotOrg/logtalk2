
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
		benchmark(Id, Description, Goal),
		run(Goal, N, Looptime, Goaltime, Average, Speed),
		report(Id, Description, N, Looptime, Goaltime, Average, Speed),
		fail.
	run(_).

	% run a specific benchmark test:
	run(Id, N) :-
		benchmark(Id, Description, Goal),
		run(Goal, N, Looptime, Goaltime, Average, Speed),
		report(Id, Description, N, Looptime, Goaltime, Average, Speed).

	run(Goal, N, Looptime, Goaltime, Average, Speed) :-
		{'$lgt_cpu_time'(Seconds1)},		% defined in the config files
		do_benchmark(N, true),
		{'$lgt_cpu_time'(Seconds2)},
		Looptime is Seconds2 - Seconds1,
		{'$lgt_cpu_time'(Seconds3)},
		do_benchmark(N, Goal),
		{'$lgt_cpu_time'(Seconds4)},
		Goaltime is Seconds4 - Seconds3,
		Average is (Goaltime - Looptime)/N,
		Speed is 1.0/Average.

	report(Id, Goal, N, Looptime, Goaltime, Average, Speed) :-
		write(Id), write(': '),
		report(Goal, N, Looptime, Goaltime, Average, Speed).

	report(Goal, N, Looptime, Goaltime, Average, Speed) :-
		writeq(Goal), nl,
		write('Number of repetitions: '), write(N), nl,
		write('Loop time: '), write(Looptime), nl,
		write('Goal time: '), write(Goaltime), nl,
		write('Average time per call: '), write(Average), nl,
		write('Number of calls per second: '), write(Speed), nl,
		nl.

	% repeat a goal N times using a failure-driven loop to avoid the interference 
	% of Prolog compiler memory management mechanism (such as garbage collection) 
	% on the results
	do_benchmark(N, Goal) :-
		{repeat(N)},		% another option would be to use a between/3 built-in predicate
			call(Goal),
		fail.
	do_benchmark(_, _).

	benchmark(Id, Goal) :-
		benchmark(Id, Goal, _).

	% some benchmark tests for static code:
	benchmark(s1, my_length(List, _), s1(List)) :-
		{generate_list(20, List)}.
	benchmark(s2, object::length(List, _), s2(List)) :-
		{generate_list(20, List)}.

	% some benchmark tests for dynamic code:
	benchmark(d1, (create_object(xpto, [], [], []), abolish_object(xpto)), d1).
	benchmark(d2, db_test_plain, d2).
	benchmark(d3, database::db_test_this, d3).
	benchmark(d4, database::db_test_self, d4).
	benchmark(d5, database::db_test_obj, d5).

	s1(List) :-
		{my_length(List, _)}.

	s2(List) :-
		object::length(List, _).

	d1 :-
		create_object(xpto, [], [], []),
		abolish_object(xpto).

	d2 :-
		{db_test_plain}.

	d3 :-
		database::db_test_this.

	d4 :-
		database::db_test_self.
		
	d5 :-
		database::db_test_obj.

:- end_object.
