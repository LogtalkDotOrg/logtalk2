
:- object(atomic,
	extends(term)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2009/3/6,
		comment is 'Atomic data type predicates.']).

	valid(Atomic) :-
		atomic(Atomic).

	check(Term) :-
		this(This),
		sender(Sender),
		(	atomic(Term) ->
			true
		;	var(Term) ->
			throw(error(instantiation_error, This::check(Term), Sender))
		;	throw(error(type_error(This, Term), This::check(Term), Sender))
		).

:- end_object.
