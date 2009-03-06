
:- object(callable,
	extends(term)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2009/3/6,
		comment is 'Callable term type predicates.']).

	valid(Callable) :-
		(	atom(Callable) ->
			true
		;	compound(Callable)
		).

	check(Term) :-
		this(This),
		sender(Sender),
		(	atom(Term) ->
			true
		;	compound(Term) ->
			true
		;	var(Term) ->
			throw(error(instantiation_error, This::check(Term), Sender))
		;	throw(error(type_error(This, Term), This::check(Term), Sender))
		).

:- end_object.
