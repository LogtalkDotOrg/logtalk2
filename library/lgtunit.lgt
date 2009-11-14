
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
		comment is 'Defines a test goal which is expected to throw an error. The Context argument specifies optional setup and cleanup goals.',
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
		comment is 'Setup environment before running the test. Defaults to the goal true.']).

	:- protected(test/0).
	:- mode(test, zero_or_one).
	:- info(test/0, [
		comment is 'Executes the tests. By default, starts with the "succeeds" tests, followed by the "fails" tests, and than the "throws" tests.']).

	:- protected(cleanup/0).
	:- mode(cleanup, zero_or_one).
	:- info(cleanup/0, [
		comment is 'Cleanup environment after running the test. Defaults to the goal true.']).

	% by default, no test setup is needed:
	setup.

	% by default, run all "succeeds", "fails", and "throws" tests:
	test :-
		test_succeeds,
		test_fails,
		test_throws.

	context_setup_cleanup(Context, Setup, Cleanup) :-
		(	member(setup(Setup), Context) ->
			true
		;	Setup = true
		),
		(	member(cleanup(Cleanup), Context) ->
			true
		;	Cleanup = true
		).

	test_succeeds :-
		forall(
			(::succeeds(Test, Context, Goal), context_setup_cleanup(Context, Setup, Cleanup)),
			setup_call_cleanup(::Setup, test_succeeds(Test, Goal), ::Cleanup)).

	test_succeeds(Test, Goal) :-
		(	catch({Goal}, _, fail) ->
			passed_test(Test, Goal)
		;	failed_test(Test, Goal)
		).

	test_fails :-
		forall(
			(::fails(Test, Context, Goal), context_setup_cleanup(Context, Setup, Cleanup)),
			setup_call_cleanup(::Setup, test_fail(Test, Goal), ::Cleanup)).

	test_fail(Test, Goal) :-
		(	catch(\+ {Goal}, _, fail) ->
			passed_test(Test, Goal)
		;	failed_test(Test, Goal)
		).

	test_throws :-
		forall(
			(::throws(Test, Context, Goal, Error), context_setup_cleanup(Context, Setup, Cleanup)),
			setup_call_cleanup(::Setup, test_throws(Test, Goal, Error), ::Cleanup)).

	test_throws(Test, Goal, Error) :-
		(	catch({Goal}, Ball, ((subsumes(Error, Ball) -> passed_test(Test, Goal); failed_test(Test, Goal)), Flag = error)) ->
			(	var(Flag) ->
				failed_test(Test, Goal)
			;	true
			)
		;	failed_test(Test, Goal)
		).

	passed_test(Test, _Goal) :-
		self(Self),
		write('= passed test '), writeq(Test), write(' in object '), writeq(Self), nl.

	failed_test(Test, _Goal) :-
		self(Self),
		write('= failed test '), writeq(Test), write(' in object '), writeq(Self), nl.

	% by default, no test cleanup is needed:
	cleanup.

	run(File, Mode) :-
		open(File, Mode, Stream),
		current_output(Output),
		set_output(Stream),
		::run,
		set_output(Output),
		close(Stream).

	run :-
		self(Self),
		write('% running tests from object '), writeq(Self), nl, 
		(	catch(::setup, Error, (broken(setup, Error), fail)) ->
			(	catch(::test, Error, (broken(test, Error), Flag = error)) ->
				do_cleanup,
				(	var(Flag) ->
					write('% completed tests from object '), writeq(Self), nl
				;	write('% test run failed'), nl
				)
			;	do_cleanup,
				write('! test run failed for object '), writeq(Self), nl,
				write('% test run failed'), nl
			)
		;	write('! test setup failed for object '), writeq(Self), nl
		).

	do_cleanup :-
		self(Self),
		(	catch(::cleanup, Error, (broken(cleanup, Error), fail)) ->
			true
		;	write('! test cleanup failed for object '), writeq(Self), nl
		).

	broken(Step, Error) :-
		self(Self),
		write('! broken '), write(Step), write(' for object '), writeq(Self), write(': '), write(Error), nl.

:- end_object.
