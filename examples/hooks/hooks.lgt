
:- object(hook,
	implements(expanding)).		% built-in protocol for term and goal expansion methods

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2007/12/3,
		comment is 'Example of an object defining compiler hook predicates.']).

	% the term_expansion/2 predicate is called for every term in the source file:
	term_expansion((:- info(Original)), [(:- info(New))]) :-
		expand_author(Original, New).

	% the goal_expansion/2 predicate is called for every goal in predicate clause
	% bodies in the source file:
	goal_expansion(
		write(Term),
		(numbervars(Term, 0, _), write_term(Term, [quoted(true), numbervars(true)]))).

	expand_author([], []).
	expand_author([Info| Infos], [ExpInfo| ExpInfos]) :-
		(	Info = (author is Abbreviation) ->
			author(Abbreviation, FullName),
			ExpInfo = (author is FullName),
			ExpInfos = Infos
		;	ExpInfo = Info,
			expand_author(Infos, ExpInfos)
		).

	author(pm, 'Paulo Moura, pmoura@logtalk.org').

:- end_object.
