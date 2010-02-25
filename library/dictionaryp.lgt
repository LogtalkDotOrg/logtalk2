
:- protocol(dictionaryp).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2010/02/25,
		comment is 'Dictionary protocol.']).

	:- public(as_dictionary/2).
	:- mode(as_dictionary(@list(pairs), -dictionary), one).
	:- info(as_dictionary/2, [
		comment is 'Converts a list of key-value pairs to a dictionary.',
		argnames is ['List', 'Dictionary']]).

	:- public(as_list/2).
	:- mode(as_list(@dictionary, -list(pairs)), one).
	:- info(as_list/2, [
		comment is 'Converts a dictionary to a list of key-value pairs.',
		argnames is ['Dictionary', 'List']]).

	:- public(delete/4).
	:- mode(delete(+dictionary, @ground, ?term, -dictionary), zero_or_one).
	:- info(delete/4, [
		comment is 'Deletes a matching Key-Value pair from a dictionary, returning the updated dictionary.',
		argnames is ['OldDictionary', 'Key', 'Value', 'NewDictionary']]).

	:- public(empty/1).
	:- mode(empty(@dictionary), zero_or_one).
	:- info(empty/1, [
		comment is 'True if the dictionary is empty.',
		argnames is ['Dictionary']]).

	:- public(insert/4).
	:- mode(insert(+ground, @term, +dictionary, -dictionary), one).
	:- info(insert/4, [
		comment is 'Inserts a Key-Value pair into a dictionary, returning the updated dictionary.',
		argnames is ['Key', 'Value', 'OldDictionary', 'NewDictionary']]).

	:- public(insert_all/3).
	:- mode(insert_all(@list(pairs), +dictionary, -dictionary), one).
	:- info(insert_all/3, [
		comment is 'Inserts a list of Key-Value pairs into a dictionary, returning the updated dictionary.',
		argnames is ['List', 'OldDictionary', 'NewDictionary']]).

	:- public(lookup/3).
	:- mode(lookup(+ground, ?term, @dictionary), zero_or_one).
	:- mode(lookup(-ground, ?term, @dictionary), zero_or_more).
	:- info(lookup/3, [
		comment is 'Get a matching Key-Value pair from a dictionary.',
		argnames is ['Key', 'Value', 'Dictionary']]).

	:- public(keys/2).
	:- mode(keys(@dictionary, -list), one).
	:- info(keys/2, [
		comment is 'Returns a list with all dictionary keys.',
		argnames is ['Dictionary', 'List']]).

	:- public(map/3).
	:- meta_predicate(map(2, *, *)).
	:- mode(map(@callable, +dictionary, -dictionary), zero_or_more).
	:- info(map/3, [
		comment is 'Maps a closure over each dictionary key-value pair returning the new dictionary. The predicate fails if the mapped closure atempts to modify the keys.',
		argnames is ['Closure', 'OldDictionary', 'NewDictionary']]).

	:- public(size/2).
	:- mode(size(@dictionary, ?integer), zero_or_one).
	:- info(size/2, [
		comment is 'Number of dictionary entries.',
		argnames is ['Dictionary', 'Size']]).

:- end_protocol.
