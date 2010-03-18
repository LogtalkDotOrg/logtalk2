
:- object(lgtunit,
	implements(expanding)).		% built-in protocol for term and goal expansion methods

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2010/03/18,
		comment is 'A simple unit test framework.']).

	:- uses(list, [member/2]).
	:- uses(term, [subsumes/2]).

	:- public(run/2).
	:- mode(run(+atom, +atom), one).
	:- info(run/2, [
		comment is 'Runs the unit tests, writing the results to the specified file. Mode can be either "write" (to create a new file) or "append" (to add results to an existing file).',
		argnames is ['File', 'Mode']]).

	:- public(run/0).
	:- mode(run, one).
	:- info(run/0, [
		comment is 'Runs the unit tests, writing the results to the current output stream.']).

	:- protected(run_tests/0).
	:- mode(run, one).
	:- info(run/0, [
		comment is 'Runs all defined unit tests.']).

	:- protected(run_tests/1).
	:- mode(run_tests(+list(callable)), one).
	:- info(run_tests/1, [
		comment is 'Runs a list of defined tests.',
		argnames is ['Tests']]).

	:- protected(setup/0).
	:- mode(setup, zero_or_one).
	:- info(setup/0, [
		comment is 'Setup environment before running the test set. Defaults to the goal true.']).

	:- protected(cleanup/0).
	:- mode(cleanup, zero_or_one).
	:- info(cleanup/0, [
		comment is 'Cleanup environment after running the test set. Defaults to the goal true.']).

	:- private(test/2).
	:- mode(test(?atom, ?nonvar), zero_or_more).
	:- info(test/2, [
		comment is 'Specifies a unit test.',
		argnames is ['Identifier', 'Outcome']]).

	:- private(test_/1).
	:- dynamic(test_/1).
	:- mode(test_(?compound), zero_or_more).
	:- info(test_/1, [
		comment is 'Table of defined tests.']).

	:- private(passed_/1).
	:- dynamic(passed_/1).
	:- mode(passed_(?integer), zero_or_one).
	:- info(passed_/1, [
		comment is 'Counter for passed tests.']).

	:- private(failed_/1).
	:- dynamic(failed_/1).
	:- mode(failed_(?callable), zero_or_one).
	:- info(failed_/1, [
		comment is 'Counter for failed tests.']).

	run(File, Mode) :-
		open(File, Mode, Stream),
		current_output(Output),
		set_output(Stream),
		::run,
		set_output(Output),
		close(Stream).

	run :-
		retractall(passed_(_)),
		asserta(passed_(0)),
		retractall(failed_(_)),
		asserta(failed_(0)),
		tests_start_time,
		self(Self),
		write('% running tests from object '), writeq(Self), nl,
		(	object_property(Self, file(File, Directory)) ->
			write('% file: '), writeq(File), write(' ('), writeq(Directory), write(')'), nl
		;	true
		),
		(	catch(::setup, Error, (broken(setup, Error), fail)) ->
			::run_tests,
			(	catch(::cleanup, Error, (broken(cleanup, Error), fail)) ->
				true
			;	write('! test cleanup failed for object '), writeq(Self), nl
			),
			passed_(Passed),
			failed_(Failed),
			Total is Passed + Failed,
			write('% '), write(Total), write(' tests: '), write(Passed), write(' passed, '), write(Failed), write(' failed'), nl,
			write('% completed tests from object '), writeq(Self), nl
		;	write('! test setup failed for object '), writeq(Self), nl
		),
		tests_end_time.

	% by default, no test setup is needed:
	setup.

	% by default, no test cleanup is needed:
	cleanup.

	run_tests([]).
	run_tests([Test| Tests]) :-
		run_test(Test),
		run_tests(Tests).

	% by default, no tests are defined:
	run_tests :-
		run_tests([]).

	run_test(succeeds(Test)) :-
		(	catch(::test(Test, _), _, fail) ->
			passed_test(Test)
		;	failed_test(Test)
		).

	run_test(fails(Test)) :-
		(	catch(\+ ::test(Test, _), _, fail) ->
			passed_test(Test)
		;	failed_test(Test)
		).

	run_test(throws(Test)) :-
		(	catch(::test(Test, Error), Ball, ((subsumes(Error, Ball) -> passed_test(Test); failed_test(Test)), Throw = true)) ->
			(	var(Throw) ->
				failed_test(Test)
			;	true
			)
		;	failed_test(Test)
		).

	tests_start_time :-
		date::today(Year, Month, Day),
		time::now(Hours, Minutes, Seconds),
		write('% tests started at '), write(Year/Month/Day), write(', '),
		write(Hours), write(':'), write(Minutes), write(':'), write(Seconds),
		nl.

	tests_end_time :-
		date::today(Year, Month, Day),
		time::now(Hours, Minutes, Seconds),
		write('% tests ended at '), write(Year/Month/Day), write(', '),
		write(Hours), write(':'), write(Minutes), write(':'), write(Seconds),
		nl, nl.

	passed_test(Test) :-
		retract(passed_(Old)) ->
		New is Old + 1,
		asserta(passed_(New)),
		writeq(Test), write(': success'), nl.

	failed_test(Test) :-
		retract(failed_(Old)) ->
		New is Old + 1,
		asserta(failed_(New)),
		writeq(Test), write(': failure'), nl.

	broken(Step, Error) :-
		self(Self),
		write('! broken '), write(Step), write(' for object '), writeq(Self), write(': '), write(Error), nl.

	failed(Step) :-
		self(Self),
		write('! failed '), write(Step), write(' for object '), writeq(Self), nl.

	term_expansion((:- object(Test, Relations)), [(:- object(Test, Relations))]) :-
		retractall(test_(_)).

	term_expansion((:- discontiguous(Functor/Arity)), []) :-
		(	Functor/Arity == succeeds/1
		;	Functor/Arity == fails/1
		;	Functor/Arity == throws/2
		),
		!.

	term_expansion((test(Test, Outcome) :- Goal), [(test(Test, Outcome) :- Goal)]) :-
		(	Outcome == true ->
			assertz(test_(succeeds(Test)))
		;	Outcome == fail ->
			assertz(test_(fails(Test)))
		;	nonvar(Outcome) ->
			assertz(test_(throws(Test)))
		).

	term_expansion((test(Test) :- Goal), [(test(Test, true) :- Goal)]) :-
		assertz(test_(succeeds(Test))).

	term_expansion((succeeds(Test) :- Goal), [(test(Test, true) :- Goal)]) :-
		assertz(test_(succeeds(Test))).

	term_expansion((fails(Test) :- Goal), [(test(Test, fail) :- Goal)]) :-
		assertz(test_(fails(Test))).

	term_expansion((throws(Test, Error) :- Goal), [(test(Test, Error) :- Goal)]) :-
		assertz(test_(throws(Test))).

	term_expansion((:- end_object), [(run_tests :- ::run_tests(Tests)), (:- end_object)]) :-
		findall(Test, retract(test_(Test)), Tests).

:- end_object.
