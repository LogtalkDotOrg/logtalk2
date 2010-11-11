%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  simple finite domain constraint solver
%%  from the SWI-Prolog 5.11.8 Reference Manual
%% 
%% ported to Logtalk by Paulo Moura

:- object(dom).

	:- public(dom/2).
	:- chr_constraint(dom/2).

	dom(_, []) <=> fail.
	dom(X, [Y]) <=> X = Y.
	dom(X, L) <=> nonvar(X) | memberchk(X, L).
	dom(X, L1), dom(X, L2) <=> intersection(L1, L2, L3), dom(X, L3).

	% use local definitions of the list predicates in order to avoid
	% loading Prolog or Logtalk libraries just for this example
	memberchk(H, [H| _]) :-
		!.
	memberchk(H, [_| T]) :-
		memberchk(H, T).

	intersection([], _, []) :-
		!.
	intersection([H| T], L, I) :-
		memberchk(H, L),
		!,
		I = [H| R],
		intersection(T, L, R).
	intersection([_| T], L, R) :-
		intersection(T, L, R).

:- end_object.
