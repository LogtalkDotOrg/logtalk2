
:- protocol(metap).

	:- info([
		version is 3.2,
		date is 2008/11/18,
		author is 'Paulo Moura',
		comment is 'Useful meta-predicates protocol.']).

	:- public(callable/1).
	:- mode(callable(@term), zero_or_one).
	:- info(callable/1, [
		comment is 'True if the argument can be called as a goal.',
		argnames is ['Term']]).

	:- public(include/3).
	:- meta_predicate(include(1, *, *)).
	:- mode(include(+callable, +list, -list), one).
	:- info(include/3, [
		comment is 'Returns a list of all list elements that satisfy a predicate.',
		argnames is ['Closure', 'List', 'Included']]).

	:- public(exclude/3).
	:- meta_predicate(exclude(1, *, *)).
	:- mode(exclude(+callable, +list, -list), one).
	:- info(exclude/3, [
		comment is 'Returns a list of all list elements that fail to satisfy a predicate.',
		argnames is ['Closure', 'List', 'Excluded']]).

	:- public(partition/4).
	:- meta_predicate(partition(1, *, *, *)).
	:- mode(partition(+callable, +list, -list, -list), one).
	:- info(partition/4, [
		comment is 'Partition a list of elements in two lists using a predicate.',
		argnames is ['Closure', 'List', 'Included', 'Excluded']]).

	:- public(ignore/1).
	:- meta_predicate(ignore(::)).
	:- mode(ignore(@callable), one).
	:- info(ignore/1, [
		comment is 'Calls Goal once but always succeeds, even if Goal fails.',
		argnames is ['Goal']]).

	:- public(fold_left/4).
	:- meta_predicate(fold_left(3, *, *, *)).
	:- mode(fold_left(+callable, ?term, +list, ?term), zero_or_more).
	:- info(fold_left/4, [
		comment is 'List folding (left associative).',
		argnames is ['Closure', 'Accumulator', 'List', 'Result']]).

	:- public(fold_right/4).
	:- meta_predicate(fold_right(3, *, *, *)).
	:- mode(fold_right(+callable, ?term, +list, ?term), zero_or_more).
	:- info(fold_right/4, [
		comment is 'List folding (right associative).',
		argnames is ['Closure', 'Accumulator', 'List', 'Result']]).

	:- public(map/2).
	:- meta_predicate(map(1, *)).
	:- mode(map(+callable, ?list), zero_or_more).
	:- info(map/2, [
		comment is 'True if the predicate succeeds for each list element.',
		argnames is ['Closure', 'List']]).

	:- public(map/3).
	:- meta_predicate(map(2, *, *)).
	:- mode(map(+callable, ?list, ?list), zero_or_more).
	:- info(map/3, [
		comment is 'List mapping predicate taken arguments from two lists of elements.',
		argnames is ['Closure', 'List1', 'List2']]).

	:- public(map/4).
	:- meta_predicate(map(3, *, *, *)).
	:- mode(map(+callable, ?list, ?list, ?list), zero_or_more).
	:- info(map/4, [
		comment is 'List mapping predicate taken arguments from three lists of elements.',
		argnames is ['Closure', 'List1', 'List2', 'List3']]).

	:- public(map/5).
	:- meta_predicate(map(4, *, *, *, *)).
	:- mode(map(+callable, ?list, ?list, ?list, ?list), zero_or_more).
	:- info(map/5, [
		comment is 'List mapping predicate taken arguments from four lists of elements.',
		argnames is ['Closure', 'List1', 'List2', 'List3', 'List4']]).

:- end_protocol.
