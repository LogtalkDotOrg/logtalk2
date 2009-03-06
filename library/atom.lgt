
:- object(atom,
	extends(atomic)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2009/3/6,
		comment is 'Atom data type predicates.']).

	valid(Atom) :-
		atom(Atom).

	check(Term) :-
		this(This),
		sender(Sender),
		(	atom(Term) ->
			true
		;	var(Term) ->
			throw(error(instantiation_error, This::check(Term), Sender))
		;	throw(error(type_error(This, Term), This::check(Term), Sender))
		).

:- end_object.
