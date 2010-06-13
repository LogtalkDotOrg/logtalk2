
:- object(shell(_Interpreters)).

	:- info([
		version is 1.0,
		author is 'Victor Lagerkvist and Paulo Moura',
		date is 2010/06/13,
		comment is 'Prolog shell for the interpreters.',
		parnames is ['Interpreters']]).

	:- public(init/0).

	init :-
		write('Welcome, noble adventurer, your destiny awaits you.'), nl,
		write('Type "help." for online help.'), nl,
		repl.

	repl :-
		write('>> '),
		flush_output,
		read(Command),
		user_reply(Command),
		repl.
	repl :-
		write('no'), nl,
		flush_output,
		repl.

	user_reply(Command) :-
		(	functor(Command, prove, _) ->
			dispatch(Command),
			write('Type "m." for more solutions or "e." to end proof:'), nl,
			flush_output,
			read(Reply),
			(	Reply == m ->
				fail
			;	!
			)
		;	dispatch(Command)
		).

	command(halt, 'Shuts down the Prolog system.').
	command(help, 'Prints this message.').
	command(load('Database'), 'Loads the specified database.').
	command(listing('Database'), 'Lists a currently loaded database.').
	command(programs('Database'), 'Prints the currently loaded database predicates.').
	command(databases, 'Prints a list of the available databases.').
	command(interpreters, 'Prints a list of the available meta-interpreters.').
	command(prove('Interpreter', 'Goal', 'Database'), 'Proves Goal with Interpreter using the specified Database.').
	command(prove('Interpreter', 'Goal', 'Limit', 'Database'), 'Proves Goal with Interpreter if Limit is not exceeded.').
	command(benchmark_all('Database'), 'Benchmarks all interpreters. Benchmarks are stored in Database as bench_goal/1 clauses.').
	command(benchmark('Interpreter', 'Goal', 'Database'), 'Benchmarks Interpreter with respect to Goal and prints the number of inferences.').
	:- if(predicate_property(statistics(_,_), built_in)).
		command(benchmark_all('Statistic', 'N', 'Database'), 'Benchmarks all interpreters with Statistic N times. Benchmarks are stored in the database as bench_goal/1 facts or rules.').
		command(benchmark('Interpreter', 'Statistic', 'N', 'Goal', 'Database'), 'Benchmarks Interpreter with respect to Statistic, N and Goal.').
	:- endif.

	dispatch(halt) :-
		halt.
	dispatch(help) :-
		write_help_message.
	dispatch(load(Database)) :-
		load_database(Database, rule_expansion(production)).
	dispatch(listing(Database)) :-
		findall(rule(Head, Body), 
			    (
				 Database::rule(Head, Body), 
				 numbervars(rule(Head, Body), 0, _)
				), 
			   Rules),
		meta::map(write_rule, Rules).
	dispatch(programs(Database)) :-
		findall(
			Functor/Arity,
			(Database::rule(Head, _), functor(Head, Functor, Arity)),
			Functors),
		list::sort(Functors, SortedFunctors),
		meta::map(writeln, SortedFunctors).
	dispatch(databases) :-
		findall(Database, implements_protocol(Database, databasep), Databases),
		meta::map(writeln, Databases).
	dispatch(interpreters) :-
		this(shell(Interpreters0)),
		pairs::keys(Interpreters0, Interpreters),
		numbervars(Interpreters, 0, _),				   
		meta::map(writeln, Interpreters).
	dispatch(prove(Interpreter, Goal, Database)) :-
		valid_interpreter(Interpreter, Expander),						
		load_database(Database, Expander),
		prove(Interpreter, Goal, Database).
	dispatch(prove(Interpreter, Goal, Limit, Database)) :-
		valid_interpreter(Interpreter, Expander),		
		load_database(Database, Expander),
		prove(Interpreter, Goal, Limit, Database).
	
	:- if(predicate_property(statistics(_,_), built_in)).

		dispatch(benchmark_all(Statistic, N, Database)) :-
			open('results.txt', append, Stream),
			(	valid_interpreter(Interpreter, Expander),
				nl(Stream),
				write(Stream, Interpreter),
				write(Stream, ':'),
				Database::bench_goal(Goal), %Assumes a set of bench_goal/1 clauses in the database.
				load_database(Database, Expander),
				write_benchmark(Stream, Interpreter, Statistic, N, Goal, Database),
				fail
			;	write('Done.'), nl, 
				close(Stream)
			).

	:- endif.

	dispatch(benchmark_all(Database)) :-
		open('results.txt', append, Stream),
		(	valid_interpreter(Interpreter, Expander),
			nl(Stream),
			write(Stream, Interpreter),
			write(Stream, ':'),
			Database::bench_goal(Goal), %Assumes a set of bench_goal/1 clauses in the database.
			load_database(Database, Expander),
			write_benchmark(Stream, Interpreter, Goal, Database),
			fail
		;	write('Done.'), nl, 
			close(Stream)
		).

	:- if(predicate_property(statistics(_,_), built_in)).

		dispatch(benchmark(Interpreter, Statistic, N, Goal, Database)) :-
			valid_interpreter(Interpreter, Expander),
			load_database(Database, Expander),
			(	benchmark(Interpreter, Statistic, N, Goal, Res0, Database)
			;	benchmark_failure(Interpreter, Statistic, N, Goal, Res0, Database),
				write('(failure) ')
			),
			write(Statistic), write(': '),
			Res is Res0/N,
			write(Res), nl.

	:- endif.	

	dispatch(benchmark(Interpreter, Goal, Database)) :-
		valid_interpreter(Interpreter, Expander),
		load_database(Database, Expander),
		current_output(Stream),
		write(Stream, Interpreter),
		write_benchmark(Stream, Interpreter, Goal, Database),
		nl.
	dispatch((Goal, Goals)) :-
		dispatch(Goal),
		dispatch(Goals).
	dispatch(Goal) :-
		prove(dfs_interpreter, Goal, demodb).

	:- if(predicate_property(statistics(_,_), built_in)).
		
		benchmark(_, _, 0, _, 0, _) :- !.
		benchmark(Interpreter, Statistic, N, Goal, Res, Database) :-
			N1 is N - 1,
			benchmark(Interpreter, Statistic, N1, Goal, Res0, Database),
			statistics(Statistic, Before),
			Interpreter::prove(Goal, 1000000, Database), !,
			statistics(Statistic, After),
			Res is Res0 + (After - Before).

		benchmark_failure(_, _, 0, _, 0, _) :- !.
		benchmark_failure(Interpreter, Statistic, N, Goal, Res, Database) :-
			N1 is N - 1,
			benchmark_failure(Interpreter, Statistic, N1, Goal, Res0, Database),
			statistics(Statistic, Before),
			\+ Interpreter::prove(Goal, 1000000, Database), 
			statistics(Statistic, After),
			Res is Res0 + (After - Before).
	:- endif.

	benchmark(Interpreter, Goal, Inferences, Database) :-
		counter::reset,
		Interpreter::prove(Goal, 1000000, Database), !,
		counter::value(Inferences).

	benchmark_failure(Interpreter, Goal, Inferences, Database) :-
		counter::reset,
		\+ Interpreter::prove(Goal, 1000000, Database), !,
		counter::value(Inferences).		

	prove(Interpreter, Goal, Database) :-
		Interpreter::prove(Goal, Database),
		write(Goal), nl.

	prove(Interpreter, Goal, Limit, Database) :-
		Interpreter::prove(Goal, Limit, Database),
		write(Goal), nl.

	valid_interpreter(Interpreter, Expander) :-
		this(shell(Interpreters)),
		functor(Interpreter, Functor, Arity),			% necessary for the parametric
		functor(InterpreterTemplate, Functor, Arity),	% interpreters
		list::member(InterpreterTemplate - Expander, Interpreters).

	load_database(Database, Expander) :-
		logtalk_load(Database, [hook(Expander), report(off), plredef(silent), unknown(silent), lgtredef(silent), startup_message(none)]). 

	write_statistics(Stream, Statistic, N, Res0) :-
		debug((write(Stream, Statistic), write(Stream, ': '))),
		Res1 is Res0/N,
		Res is floor(Res1),
		write(Stream, Res).

	write_help_message :-
		write('Available commands are:'), nl,
		(	command(Command, Description),
			write(Command), nl, write('  '), write(Description), nl,
			fail
		;	true
		).
		
	write_rule(rule(Head, Body)) :-
		write(Head),
		write(' '),
		write('<-'), nl,
		write_body(Body),
		nl.

	write_body([G]) :-
		!,
		write('	  '),
		write(G),
		write('.').
	write_body([]) :-
		!,
		write('	  '),
		write(true),
		write('.').
	write_body([G|Gs]) :-
		write('	  '),	
		write(G),
		write(' '),
		write('&'), nl,
		write_body(Gs).

	write_benchmark(Stream, Interpreter, Statistic, N, Goal, Database) :-
		write(Stream, ' '), 
		(	benchmark(Interpreter, Statistic, N, Goal, Res, Database), !
		;	benchmark_failure(Interpreter, Statistic, N, Goal, Res, Database),
			write(Stream, '(F) ')
		),
		write_statistics(Stream, Statistic, N, Res).
		
	write_benchmark(Stream, Interpreter, Goal, Database) :-
		write(Stream, ' '), 
		(	benchmark(Interpreter, Goal, Inferences, Database), !
		;	benchmark_failure(Interpreter,  Goal, Inferences, Database),
			write(Stream, '(F) ')
		),
		write('inferences: '),
		write(Stream, Inferences).

	writeln(X) :-
		write(X),
		nl.		

	writeln(Stream, X) :-
		write(Stream, X),
		nl(Stream).

:- end_object.
