
:- object(pairs).

	:- info([
		version is 1.1,
		date is 2011/01/07,
		author is 'Paulo Moura',
		comment is 'Useful predicates over lists of pairs (key-value terms).']).

    :- public(keys_values/3).
	:- mode(keys_values(+list(pair), -list, -list), one).
	:- mode(keys_values(-list(pair), +list, +list), one).
	:- info(keys_values/3, [
		comment is 'Converts between a list of pairs and lists of keys and values.',
		argnames is ['Pairs', 'Keys', 'Values']]).

    :- public(keys/2).
	:- mode(keys(+list(pair), -list), one).
	:- info(keys/2, [
		comment is 'Extracts a list of keys from a list of pairs.',
		argnames is ['Pairs', 'Keys']]).

    :- public(values/2).
	:- mode(values(+list(pair), -list), one).
	:- info(values/2, [
		comment is 'Extracts a list of values from a list of pairs.',
		argnames is ['Pairs', 'Values']]).

    :- public(transpose/2).
	:- mode(transpose(+list(pair), -list(pair)), one).
	:- info(transpose/2, [
		comment is 'Transposes a list of pairs by swapping each pair key and value. The relative order of the list elements is kept.',
		argnames is ['Pairs', 'TransposedPairs']]).

	keys_values(Pairs, Keys, Values) :-
		(	nonvar(Pairs) ->
			pairs_to_keys_values(Pairs, Keys, Values)
		;	nonvar(Keys), nonvar(Values) ->
			keys_values_to_pairs(Keys, Values, Pairs)
		).

	pairs_to_keys_values([], [], []).
	pairs_to_keys_values([Key-Value| Pairs], [Key| Keys], [Value| Values]) :-
		pairs_to_keys_values(Pairs, Keys, Values).

	keys_values_to_pairs([], [], []).
	keys_values_to_pairs([Key| Keys], [Value| Values], [Key-Value| Pairs]) :-
		keys_values_to_pairs(Keys, Values, Pairs).

	keys([], []).
	keys([Key-_| Pairs], [Key| Keys]) :-
		keys(Pairs, Keys).

	values([], []).
	values([_-Value| Pairs], [Value| Values]) :-
		values(Pairs, Values).

	transpose([], []).
	transpose([Key-Value| Pairs], [Value-Key| TransposedPairs]) :-
		transpose(Pairs, TransposedPairs).

:- end_object.
