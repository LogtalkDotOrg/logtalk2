
:- object(natural,
	extends(integer)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2009/3/6,
		comment is 'Natural numbers data type predicates.']).

	between(Lower, Upper, Integer) :-
		integer(Lower),
		Lower > 0,
		^^between(Lower, Upper, Integer).

	valid(Natural) :-
		integer(Natural),
		Natural > 0.

	check(Term) :-
		this(This),
		sender(Sender),
		(	integer(Term), Term > 0 ->
			true
		;	var(Term) ->
			throw(error(instantiation_error, This::check(Term), Sender))
		;	throw(error(type_error(This, Term), This::check(Term), Sender))
		).

:- end_object.
