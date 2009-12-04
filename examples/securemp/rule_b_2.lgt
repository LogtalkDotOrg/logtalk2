
:- initialization(catch(client_b_2::test, Error, writeq(Error))).


:- object(library_b_2).

	:- meta_predicate(meta(::)).
	meta(Goal) :-
		call(Goal).

	:- public(pred/1).
	pred(Arg) :-
		meta(Arg).

:- end_object.


:- object(client_b_2).

	:- public(test/0).
	test :-
		library_b_2::pred(term).

	term :-
		write('Some local, private predicate.').

:- end_object.
