
% you may need to update the module directive that follows for 
% compatibility with your Prolog compiler module system

:- module(module, [mod_nrev/2, mod_length/2]).


mod_append([], X, X).
mod_append([X| Xs], Y, [X| Z]) :-
	mod_append(Xs, Y, Z).

mod_nrev([], []).
mod_nrev([X| Xs], Zs) :-
	mod_nrev(Xs, Ys),
	mod_append(Ys, [X], Zs).

mod_length(List, Length) :-
	(	integer(Length) ->
		Length >= 0,
		mod_make_list(Length, List)
	;	mod_length(List, 0, Length)
	).

mod_make_list(0, []):-
	!.
mod_make_list(N, [_| Tail]):-
	M is N-1,
	mod_make_list(M, Tail).

mod_length([], Length, Length).
mod_length([_| Tail], Acc, Length) :-
	Acc2 is Acc + 1,
	mod_length(Tail, Acc2, Length).
