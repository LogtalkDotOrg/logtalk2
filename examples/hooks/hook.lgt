
:- object(hook,
	implements(expanding)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2007/12/3,
		comment is 'Example of an object defining compiler hook predicates.']).

	term_expansion((:- info(Original)), [(:- info(New))]) :-
		expand_author(Original, New).

	goal_expansion(write(Term), (write_term(Term, []), nl)).

	expand_author([], []).
	expand_author([Info| Infos], [Info2| Infos2]) :-
		(	Info = (author is Abbreviation) ->
			author(Abbreviation, FullName),
			Info2 = (author is FullName)
		;	Info = Info2
		),
		expand_author(Infos, Infos2).

	author(pm, 'Paulo Moura, pmoura@logtalk.org').

:- end_object.
