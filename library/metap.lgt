
:- protocol(metap).

	:- info([
		version is 3.0,
		date is 2008/7/16,
		author is 'Paulo Moura',
		comment is 'Useful meta-predicates protocol.']).

	:- public(callable/1).
	:- mode(callable(@term), zero_or_one).
	:- info(callable/1, [
		comment is 'True if the argument can be called as a goal.',
		argnames is ['Term']]).

	:- public(filter/3).
	:- meta_predicate(filter(1, *, *)).
	:- mode(filter(+callable, +list, -list), one).
	:- info(filter/3, [
		comment is 'Returns a list of all list elements that satisfy a predicate.',
		argnames is ['Predicate', 'In', 'Out']]).

	:- public(ignore/1).
	:- meta_predicate(ignore(::)).
	:- mode(ignore(@callable), one).
	:- info(ignore/1, [
		comment is 'Calls Goal once but always succeeds, even if Goal fails.',
		argnames is ['Goal']]).

	:- public(map/3).
	:- meta_predicate(map(2, *, *)).
	:- mode(map(+callable, ?list, ?list), zero_or_more).
	:- info(map/3, [
		comment is 'List mapping predicate taken arguments from two lists of elements.',
		argnames is ['Predicate', 'List1', 'List2']]).

	:- public(map/4).
	:- meta_predicate(map(3, *, *, *)).
	:- mode(map(+callable, ?list, ?list, ?list), zero_or_more).
	:- info(map/4, [
		comment is 'List mapping predicate taken arguments from three lists of elements.',
		argnames is ['Predicate', 'List1', 'List2', 'List3']]).

	:- public(map/5).
	:- meta_predicate(map(4, *, *, *, *)).
	:- mode(map(+callable, ?list, ?list, ?list, ?list), zero_or_more).
	:- info(map/5, [
		comment is 'List mapping predicate taken arguments from four lists of elements.',
		argnames is ['Predicate', 'List1', 'List2', 'List3', 'List4']]).

	:- public(succeeds/2).
	:- meta_predicate(succeeds(1, *)).
	:- mode(succeeds(+callable, +list), zero_or_more).
	:- info(succeeds/2, [
		comment is 'True if the predicate succeeds for each list element.',
		argnames is ['Predicate', 'List']]).

:- end_protocol.
