
:- object(lgtunit).

	:- info([
		version is 0.4,
		author is 'Paulo Moura',
		date is 2009/11/14,
		comment is 'Logtalk unit test framework.']).

	:- uses(list, [member/2]).
	:- uses(term, [subsumes/2]).

	:- public(succeeds/3).
	:- mode(succeeds(+atom, +list(callable), @callable), zero_or_more).
	:- info(succeeds/3, [
		comment is 'Defines a test goal which is expected to succeed. The Context argument specifies optional setup and cleanup goals.',
		argnames is ['Identifier', 'Context', 'Goal']]).

	:- public(fails/3).
	:- mode(fails(+atom, +list(callable), @callable), zero_or_more).
	:- info(fails/3, [
		comment is 'Defines a test goal which is expected to fail. The Context argument specifies optional setup and cleanup goals.',
		argnames is ['Identifier', 'Context', 'Goal']]).

	:- public(throws/4).
	:- mode(throws(+atom, +list(callable), @callable, @nonvar), zero_or_more).
	:- info(throws/4, [
		comment is 'Defines a test goal which is expected to throw a specific error. The Context argument specifies optional setup and cleanup goals.',
		argnames is ['Identifier', 'Context', 'Goal', 'Error']]).

	:- public(run/2).
	:- mode(run(+atom, +atom), zero_or_one).
	:- info(run/2, [
		comment is 'Runs the unit tests, writing the results to the specified file. Mode can be either "write" (to create a new file) or "append" (to add results to an existing file).',
		argnames is ['File', 'Mode']]).

	:- public(run/0).
	:- mode(run, zero_or_one).
	:- info(run/0, [
		comment is 'Runs the unit tests, writing the results to the current output stream.']).

	:- protected(setup/0).
	:- mode(setup, zero_or_one).
	:- info(setup/0, [
		comment is 'Setup environment before running the test set. Defaults to the goal true.']).

	:- protected(cleanup/0).
	:- mode(cleanup, zero_or_one).
	:- info(cleanup/0, [
		comment is 'Cleanup environment after running the test set. Defaults to the goal true.']).

	:- private(passed_tests_/1).
	:- dynamic(passed_tests_/1).
	:- mode(passed_tests_(?integer), zero_or_one).
	:- info(passed_tests_/1, [
		comment is 'Counter for passed tests.']).

	:- private(failed_tests_/1).
	:- dynamic(failed_tests_/1).
	:- mode(failed_tests_(?integer), zero_or_one).
	:- info(failed_tests_/1, [
		comment is 'Counter for failed tests.']).

	run(File, Mode) :-
		open(File, Mode, Stream),
		current_output(Output),
		set_output(Stream),
		::run,
		set_output(Output),
		close(Stream).

	run :-
		retractall(passed_tests_(_)),
		asserta(passed_tests_(0)),
		retractall(failed_tests_(_)),
		asserta(failed_tests_(0)),
		date::today(Year1, Month1, Day1),
		time::now(Hours1, Minutes1, Seconds1),
		write('% tests started at '), write(Year1/Month1/Day1), write(', '), write(Hours1), write(':'), write(Minutes1), write(':'), write(Seconds1), nl,
		self(Self),
		write('% running tests from object '), writeq(Self), nl,
		(	object_property(Self, file(File, Directory)) ->
			write('% file: '), writeq(File), write(' ('), writeq(Directory), write(')'), nl
		;	true
		),
		(	catch(::setup, Error, (broken(setup, Error), fail)) ->
			(	catch(run_tests, Error, (broken(test, Error), Flag = error)) ->
				do_cleanup,
				(	var(Flag) ->
					passed_tests_(Passed),
					failed_tests_(Failed),
					Total is Passed + Failed,
					write('% '), write(Total), write(' tests: '), write(Passed), write(' passed, '), write(Failed), write(' failed'), nl,
					write('% completed tests from object '), writeq(Self), nl
				;	write('% test run failed'), nl
				)
			;	do_cleanup,
				write('! test run failed for object '), writeq(Self), nl,
				write('% test run failed'), nl
			)
		;	write('! test setup failed for object '), writeq(Self), nl
		),
		date::today(Year2, Month2, Day2),
		time::now(Hours2, Minutes2, Seconds2),
		write('% tests ended at '), write(Year2/Month2/Day2), write(', '), write(Hours2), write(':'), write(Minutes2), write(':'), write(Seconds2), nl, nl.

	% by default, no test setup is needed:
	setup.

	% by default, no test cleanup is needed:
	cleanup.

	run_tests :-
		forall(succeeds_test(Test, Setup, Goal, Cleanup), run_succeeds_test(Test, Setup, Goal, Cleanup)),
		forall(fails_test(Test, Setup, Goal, Cleanup), run_fails_test(Test, Setup, Goal, Cleanup)),
		forall(throws_test(Test, Setup, Goal, Error, Cleanup), run_throws_test(Test, Setup, Goal, Error, Cleanup)).

	succeeds_test(Test, Setup, Goal, Cleanup) :-
		::succeeds(Test, Context, Goal),
		context_setup_cleanup(Context, Setup, Cleanup).

	fails_test(Test, Setup, Goal, Cleanup) :-
		::fails(Test, Context, Goal),
		context_setup_cleanup(Context, Setup, Cleanup).

	throws_test(Test, Setup, Goal, Error, Cleanup) :-
		::throws(Test, Context, Goal, Error),
		context_setup_cleanup(Context, Setup, Cleanup).

	run_succeeds_test(Test, Setup, Goal, Cleanup) :-
		self(Self),
		(	catch(Self<<Setup, Error, (broken(setup(Test), Error), Flag = error)) ->
			(	var(Flag) ->
				(	catch({Goal}, _, fail) ->
					passed_test(Test)
				;	failed_test(Test)
				),
				(	catch(Self<<Cleanup, Error, broken(cleanup(Test), Error)) ->
					true
				;	failed(cleanup(Test))
				)
			;	true
			)
		;	failed(setup(Test))
		).

	run_fails_test(Test, Setup, Goal, Cleanup) :-
		self(Self),
		(	catch(Self<<Setup, Error, (broken(setup(Test), Error), Flag = error)) ->
			(	var(Flag) ->
				(	catch(\+ {Goal}, _, fail) ->
					passed_test(Test)
				;	failed_test(Test)
				),
				(	catch(Self<<Cleanup, Error, broken(cleanup(Test), Error)) ->
					true
				;	failed(cleanup(Test))
				)
			;	true
			)
		;	failed(setup(Test))
		).

	run_throws_test(Test, Setup, Goal, Error, Cleanup) :-
		self(Self),
		(	catch(Self<<Setup, Error, (broken(setup(Test), Error), Flag = error)) ->
			(	var(Flag) ->
				(	catch({Goal}, Ball, ((subsumes(Error, Ball) -> passed_test(Test); failed_test(Test)), Throw = true)) ->
					(	var(Throw) ->
						failed_test(Test)
					;	true
					)
				;	failed_test(Test)
				),
				(	catch(Self<<Cleanup, Error, broken(cleanup(Test), Error)) ->
					true
				;	failed(cleanup(Test))
				)
			;	true
			)
		;	failed(setup(Test))
		).

	context_setup_cleanup(Context, Setup, Cleanup) :-
		(	member(setup(Setup), Context) ->
			true
		;	Setup = true
		),
		(	member(cleanup(Cleanup), Context) ->
			true
		;	Cleanup = true
		).

	passed_test(Test) :-
		retract(passed_tests_(Old)) ->
		New is Old + 1,
		asserta(passed_tests_(New)),
		writeq(Test), write(': passed'), nl.

	failed_test(Test) :-
		retract(failed_tests_(Old)) ->
		New is Old + 1,
		asserta(failed_tests_(New)),
		writeq(Test), write(': failed'), nl.

	do_cleanup :-
		self(Self),
		(	catch(::cleanup, Error, (broken(cleanup, Error), fail)) ->
			true
		;	write('! test cleanup failed for object '), writeq(Self), nl
		).

	broken(Step, Error) :-
		self(Self),
		write('! broken '), write(Step), write(' for object '), writeq(Self), write(': '), write(Error), nl.

	failed(Step) :-
		self(Self),
		write('! failed '), write(Step), write(' for object '), writeq(Self), nl.

:- end_object.
