
:- object(prototype).


	:- public(public_assert/0).
	:- public(protected_assert/0).
	:- public(private_assert/0).

	:- public(dynamic_predicates/0).


	public_assert :-
		self(Self),
		Self::assertz(public_predicate).


	protected_assert :-
		::assertz(protected_predicate).


	private_assert :-
		assertz(private_predicate).


	dynamic_predicates :-
		current_predicate(Functor/Arity),
		functor(Predicate, Functor, Arity),
		predicate_property(Predicate, (dynamic)),
		predicate_property(Predicate, Scope),
		scope(Scope),
		writeq(Functor/Arity), write(' - '), writeq(Scope), nl,
		fail.

	dynamic_predicates.


	scope(private).
	scope(protected).
	scope((public)).


:- end_object.
