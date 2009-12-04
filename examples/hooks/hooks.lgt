
:- object(hook,
	implements(expanding)).		% built-in protocol for term and goal expansion methods

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2009/6/12,
		comment is 'Example of an object defining compiler hook predicates.']).

	% the term_expansion/2 predicate is called for every term in the source file
	% being compiled using this hook object:
	term_expansion((:- info(Original)), [(:- info(New))]) :-
		expand_key_values(Original, New).

	% the goal_expansion/2 predicate is called for every goal in predicate clause
	% bodies in the source file being compiled using this hook object:
	goal_expansion(
		write(Term),
		(numbervars(Term, 0, _), write_term(Term, [quoted(true), numbervars(true)]))).

	expand_key_values([], []).
	expand_key_values([Info| Infos], [ExpInfo| ExpInfos]) :-
		(	Info = (Key is Value), key_value(Key, Value, ExpValue) ->
			ExpInfo = (Key is ExpValue)
		;	ExpInfo = Info
		),
		expand_key_values(Infos, ExpInfos).

	key_value(author, pm, 'Paulo Moura, pmoura@logtalk.org').
	key_value(license, artistic2, 'Artistic License 2.0').

:- end_object.
