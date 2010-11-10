
:- object(dom).

	:- public(dom/2).

	:- use_module(library(lists), [intersection/3, memberchk/2]).

	:- chr_type list(T) ---> [] ; [T| list(T)].

	:- chr_constraint(dom(?int, +list(int))).

	dom(_, []) <=> fail.
	dom(X, [Y]) <=> X = Y.
	dom(X, L) <=> nonvar(X) | memberchk(X, L).
	dom(X, L1), dom(X, L2) <=> intersection(L1, L2, L3), dom(X, L3).

:- end_object.
