
:- object(lgtunit,
	implements(expanding)).		% built-in protocol for term and goal expansion methods

	:- info([
		version is 1.5,
		author is 'Paulo Moura',
		date is 2012/05/01,
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

	:- public(op(700, xfx, ('=~='))).
	:- public(('=~=')/2).
	:- mode('=~='(+float, +float), zero_or_one).
	:- info(('=~=')/2, [
		comment is 'Compares two floats for approximate equality using 100*epsilon for the absolute error and, if that fails, 99.999% accuracy for the relative error. Handy when writing certain unit tests but the default precision values may not be adequate for all cases.',
		argnames is ['Float1', 'Float2']]).

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
		(	catch(::test(Test, _), Ball, (unexpected_error_expected_success(Test, Ball), Throw = true)) ->
			(	var(Throw) ->
				passed_test(Test)
			;	true
			)
		;	unexpected_failure_expected_success(Test)
		).
	run_test(fails(Test)) :-
		(	catch(::test(Test, _), Ball, (unexpected_error_expected_failure(Test, Ball), Throw = true)) ->
			(	var(Throw) ->
				unexpected_success_expected_failure(Test)
			;	true
			)
		;	passed_test(Test)
		).
	run_test(throws(Test, Error)) :-
		(	catch(::test(Test, Error), Ball, ((subsumes(Error, Ball) -> passed_test(Test); wrong_error(Test, Error, Ball)), Throw = true)) ->
			(	var(Throw) ->
				unexpected_success_expected_error(Test)
			;	true
			)
		;	unexpected_failure_expected_error(Test)
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

	unexpected_success_expected_failure(Test) :-
		retract(failed_(Old)) ->
		New is Old + 1,
		asserta(failed_(New)),
		writeq(Test), write(': failure (test goal succeeded but should have failed)'), nl.

	unexpected_success_expected_error(Test) :-
		retract(failed_(Old)) ->
		New is Old + 1,
		asserta(failed_(New)),
		writeq(Test), write(': failure (test goal succeeded but should have throw an error)'), nl.

	unexpected_failure_expected_success(Test) :-
		retract(failed_(Old)) ->
		New is Old + 1,
		asserta(failed_(New)),
		writeq(Test), write(': failure (test goal failed but should have succeeded)'), nl.

	unexpected_failure_expected_error(Test) :-
		retract(failed_(Old)) ->
		New is Old + 1,
		asserta(failed_(New)),
		writeq(Test), write(': failure (test goal failed but should have throw an error)'), nl.

	unexpected_error_expected_failure(Test, Error) :-
		retract(failed_(Old)) ->
		New is Old + 1,
		asserta(failed_(New)),
		writeq(Test), write(': failure (test goal throws an error but should have failed: '), writeq(Error), write(')'), nl.

	unexpected_error_expected_success(Test, Error) :-
		retract(failed_(Old)) ->
		New is Old + 1,
		asserta(failed_(New)),
		writeq(Test), write(': failure (test goal throws an error but should have succeeded: '), writeq(Error), write(')'), nl.

	wrong_error(Test, Error, Ball) :-
		retract(failed_(Old)) ->
		New is Old + 1,
		asserta(failed_(New)),
		writeq(Test), write(': failure (test goal throws the wrong error: got '), writeq(Ball), write(' instead of '),  writeq(Error), write(')'), nl.

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

	% unit test idiom test/2
	term_expansion((test(Test, Outcome) :- Goal), [(test(Test, Outcome) :- Goal)]) :-
		(	Outcome == true ->
			assertz(test_(succeeds(Test)))
		;	Outcome == fail ->
			assertz(test_(fails(Test)))
		;	nonvar(Outcome) ->
			assertz(test_(throws(Test, Outcome)))
		).

	% unit test idiom test/1
	term_expansion((test(Test) :- Goal), [(test(Test, true) :- Goal)]) :-
		assertz(test_(succeeds(Test))).

	% unit test idiom succeeds/1 + fails/1 + throws/2
	term_expansion((succeeds(Test) :- Goal), [(test(Test, true) :- Goal)]) :-
		assertz(test_(succeeds(Test))).
	term_expansion((fails(Test) :- Goal), [(test(Test, fail) :- Goal)]) :-
		assertz(test_(fails(Test))).
	term_expansion((throws(Test, Error) :- Goal), [(test(Test, Error) :- Goal)]) :-
		assertz(test_(throws(Test, Error))).

	term_expansion((:- end_object), [(run_tests :- ::run_tests(Tests)), (:- end_object)]) :-
		findall(Test, retract(test_(Test)), Tests).

	'=~='(Float1, Float2) :-
		(	% first test the absolute error, for meaningful results with numbers very close to zero:
			epsilon(Epsilon), abs(Float1 - Float2) < 100*Epsilon ->
			true
		;	% if that fails, test the relative error (protected by a catch/3 to avoid division errors)
		 	% by using as the divisor the larger float in order to make argument order irrelevant:
			abs(Float1) > abs(Float2) ->
			catch(abs((Float1 - Float2) / Float1) < 0.00001, _, fail)	% 99.999% accuracy
		;	catch(abs((Float1 - Float2) / Float2) < 0.00001, _, fail)
		).

	:- if((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == swi; Dialect == yap; Dialect == gnu; Dialect == b; Dialect == cx))).
		epsilon(Epsilon) :-
			Epsilon is epsilon.
	:- else.
		epsilon(0.000000000001).
	:- endif.

:- end_object.
