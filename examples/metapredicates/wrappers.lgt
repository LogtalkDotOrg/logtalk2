
% example of defining wrappers for the bagof/3 and setof/3 built-in methods
% where the meta-argument may contain existentially qualified variables

:- object(wrappers).

	:- public(my_setof/3).
	:- meta_predicate(my_setof(*, ^, *)).

	my_setof(Term, Goal, List) :-
		setof(Term, Goal, List).

:- end_object.


:- object(object).

	:- public(p/1).
	p(L) :-
		wrappers::my_setof(X, Y^p(X, Y), L).

	p(1, one).
	p(2, two).
	p(3, three).

:- end_object.
