
:- initialization(catch(client_b_3_variant::test(_), Error, writeq(Error))).

:- object(library_b_3_variant).

	:- public(m/2).
	:- meta_predicate(m(2, *)).
	m(Closure, Arg) :-
		call(Closure, Arg, _).

:- end_object.


:- object(client_b_3_variant).

	:- public(test/1).
	test(X) :-
		library_b_3_variant::m(a, X).

	a(1). a(2).

	a(3, one). a(4, two).

:- end_object.
