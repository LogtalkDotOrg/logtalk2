
% code adapted to Logtalk by Paulo Moura from one of the CLP(FD) examples
% written by Markus Triska (November 2009)


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Closed Knight's Tour.

   Written by Markus Triska (triska@gmx.at) Nov. 2nd 2009
   Tested with SWI-Prolog 5.9.0
   Public domain code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


:- object(knight).

	:- use_module(clpfd, [
					op(700, xfx, #=), op(700, xfx, in), op(700, xfx, ins),
					(#=)/2, circuit/1, (in)/2, (ins)/2, label/1]).

	:- uses(list, [append/2, length/2, nth1/3]).
	:- uses(meta, [map/2::maplist/2]).

	:- public(n_tour/2).

	/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	Constraints.
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	n_tour(N, Ts) :-
		length(Ts, N),
		maplist(length_(N), Ts),
		append(Ts, Vs),
		circuit(Vs),
		successors(Vs, N, 1).

	successors([], _, _).
	successors([V| Vs], N, K0) :-
		findall(Num, n_k_next(N, K0, Num), [Next| Nexts]),
		nums_to_dom(Nexts, Next, Dom),
		V in Dom,
		K1 #= K0 + 1,
		successors(Vs, N, K1).

	nums_to_dom([], D, D).
	nums_to_dom([N| Ns], D0, D) :-
		nums_to_dom(Ns, D0 \/ N, D).

	length_(L, Ls) :-
		length(Ls, L).

	n_x_y_k(N, X, Y, K) :-
		[X,Y] ins 1..N,
		K #= N*(Y-1) + X.

	n_k_next(N, K, Next) :-
		n_x_y_k(N, X0, Y0, K),
		[DX,DY] ins -2 \/ -1 \/ 1 \/ 2,
		abs(DX) + abs(DY) #= 3,
		[X,Y] ins 1..N,
		X #= X0 + DX,
		Y #= Y0 + DY,
		n_x_y_k(N, X, Y, Next),
		label([DX, DY]).

	/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   	Display.
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	:- public(tour_enumeration/2).

	tour_enumeration(Ts, Es) :-
		length(Ts, N),
		length(Es, N),
		maplist(length_(N), Es),
		append(Ts, Vs),
		append(Es, Ls),
		vs_enumeration(Vs, Vs, 1, 1, Ls).

	vs_enumeration([], _, _, _, _).
	vs_enumeration([_| Rest], Vs, V0, E0, Ls) :-
		nth1(V0, Ls, E0),
		nth1(V0, Vs, V1),
		E1 #= E0 + 1,
		vs_enumeration(Rest, Vs, V1, E1, Ls).

	/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	Examples:
	See the "SCRIPT.txt" file for usage examples.
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- end_object.
