
:- object(float,
	extends(number)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2009/3/6,
		comment is 'Floating point numbers data type predicates.']).

	valid(Float) :-
		float(Float).

	check(Term) :-
		this(This),
		sender(Sender),
		(	float(Term) ->
			true
		;	var(Term) ->
			throw(error(instantiation_error, This::check(Term), Sender))
		;	throw(error(type_error(This, Term), This::check(Term), Sender))
		).

:- end_object.
