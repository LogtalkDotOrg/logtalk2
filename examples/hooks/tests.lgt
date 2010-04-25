
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2010/04/25,
		comment is 'Unit tests for the "hooks" example.']).

	throws(hooks_1, error(permission_error(private_predicate, item/1), object::item(_), This)) :-
		this(This),
		object::item(_).

	succeeds(hooks_2) :-
		object::items(Items),
		Items == [alpha, omega, zeta].

:- end_object.
