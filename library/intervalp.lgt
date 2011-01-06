
:- protocol(intervalp).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2011/01/06,
		comment is 'Basic temporal interval relations protocol (based on James F. Allen Interval Algebra work).']).

	:- public(valid/1).
	:- mode(valid(@interval), zero_or_one).
	:- info(valid/1, [
		comment is 'True if Interval is a valid interval.',
		argnames is ['Interval']]).

	:- public(before/2).
	:- mode(before(@interval, @interval), zero_or_one).
	:- info(before/2, [
		comment is 'True if Interval1 takes place before Interval2.',
		argnames is ['Interval1', 'Interval2']]).

	:- public(after/2).
	:- mode(after(@interval, @interval), zero_or_one).
	:- info(after/2, [
		comment is 'True if Interval1 takes place after Interval2.',
		argnames is ['Interval1', 'Interval2']]).

	:- public(meets/2).
	:- mode(meets(@interval, @interval), zero_or_one).
	:- info(meets/2, [
		comment is 'True if Interval1 meets Interval2.',
		argnames is ['Interval1', 'Interval2']]).

	:- public(met_by/2).
	:- mode(met_by(@interval, @interval), zero_or_one).
	:- info(met_by/2, [
		comment is 'True if Interval1 is met by Interval2.',
		argnames is ['Interval1', 'Interval2']]).

	:- public(overlaps/2).
	:- mode(overlaps(@interval, @interval), zero_or_one).
	:- info(overlaps/2, [
		comment is 'True if Interval1 overlaps with Interval2.',
		argnames is ['Interval1', 'Interval2']]).

	:- public(overlapped_by/2).
	:- mode(overlapped_by(@interval, @interval), zero_or_one).
	:- info(overlapped_by/2, [
		comment is 'True if Interval1 is overlapped by Interval2.',
		argnames is ['Interval1', 'Interval2']]).

	:- public(starts/2).
	:- mode(starts(@interval, @interval), zero_or_one).
	:- info(starts/2, [
		comment is 'True if Interval1 starts Interval2.',
		argnames is ['Interval1', 'Interval2']]).

	:- public(started_by/2).
	:- mode(started_by(@interval, @interval), zero_or_one).
	:- info(started_by/2, [
		comment is 'True if Interval1 is started by Interval2.',
		argnames is ['Interval1', 'Interval2']]).

	:- public(during/2).
	:- mode(during(@interval, @interval), zero_or_one).
	:- info(during/2, [
		comment is 'True if Interval1 occurs during Interval2.',
		argnames is ['Interval1', 'Interval2']]).

	:- public(contains/2).
	:- mode(contains(@interval, @interval), zero_or_one).
	:- info(contains/2, [
		comment is 'True if Interval1 contains Interval2.',
		argnames is ['Interval1', 'Interval2']]).

	:- public(finishes/2).
	:- mode(finishes(@interval, @interval), zero_or_one).
	:- info(finishes/2, [
		comment is 'True if Interval1 finishes Interval2.',
		argnames is ['Interval1', 'Interval2']]).

	:- public(finished_by/2).
	:- mode(finished_by(@interval, @interval), zero_or_one).
	:- info(finished_by/2, [
		comment is 'True if Interval1 is finished by Interval2.',
		argnames is ['Interval1', 'Interval2']]).

	:- public(equal/2).
	:- mode(equal(@interval, @interval), zero_or_one).
	:- info(equal/2, [
		comment is 'True if Interval1 is equal to Interval2.',
		argnames is ['Interval1', 'Interval2']]).

:- end_protocol.
