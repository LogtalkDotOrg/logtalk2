/* This example's code is adapted from the following paper:

@incollection{ apt93modular,
    author = "Krzysztof R. Apt and Dino Pedreschi",
    title = "Modular Termination Proofs for Logic and Pure Prolog Programs.",
    booktitle = "116",
    month = "31",
    publisher = "Centrum voor Wiskunde en Informatica (CWI)",
    address = "ISSN 0169-118X",
    pages = "35",
    year = "1993",
    url = "citeseer.ist.psu.edu/apt93modular.html" }
*/

:- object(msort).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2007/03/22,
		comment is 'Single-threaded and multi-threaded versions of the merge sort algorithm.']).

	:- threaded.

	:- public(st_msort/2).

	st_msort(List, Sorted) :-
		st_msort(List, Sorted, List).

	st_msort([], [], _).
	st_msort([X], [X], _).
	st_msort([X, Y| Xs], Ys, [H| Ls]) :-
		split([X, Y| Xs], X1s, X2s, [H| Ls]),
		st_msort(X1s, Y1s, Ls),
		st_msort(X2s, Y2s, Ls),
		merge(Y1s, Y2s, Ys, [H| Ls]).

	:- public(mt_msort/2).

	mt_msort(List, Sorted) :-
		mt_msort(List, Sorted, List).

	mt_msort([], [], _).
	mt_msort([X], [X], _).
	mt_msort([X, Y| Xs], Ys, [H| Ls]) :-
		split([X, Y| Xs], X1s, X2s, [H| Ls]),
		threaded_call(st_msort(X1s, Y1s, Ls)),
		threaded_call(st_msort(X2s, Y2s, Ls)),
		threaded_exit(st_msort(X1s, Y1s, Ls)),
		threaded_exit(st_msort(X2s, Y2s, Ls)),
		merge(Y1s, Y2s, Ys, [H| Ls]).

	split([], [], [], _).
	split([X| Xs], [X| Ys], Zs, [_| Ls]) :-
		split(Xs, Zs, Ys, Ls).

	merge([], Xs, Xs, _) :- !.
	merge(Xs, [], Xs, _) :- !.
	merge([X| Xs], [Y| Ys], [X| Zs], [_| Ls]) :-
		X @=< Y, !,
		merge(Xs, [Y| Ys], Zs, Ls).
	merge([X| Xs], [Y| Ys], [Y| Zs], [_| Ls]) :-
		X @> Y,
		merge([X | Xs], Ys, Zs, Ls).

:- end_object.

