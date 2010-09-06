
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2010/09/06,
		comment is 'Unit tests for the "coinduction" example.']).

	:- discontiguous(succeeds/1).
	:- discontiguous(fails/1).

	succeeds(coinduction_simple_1) :-
		simple::p.

	succeeds(coinduction_simple_2) :-
    	simple::p(hoho).

	succeeds(coinduction_simple_3) :-
		simple::p(hoho, X),
		X == hoho.

	succeeds(coinduction_binary_1) :-
		L = [1| L], binary::p(L).

	succeeds(coinduction_binary_2) :-
		L = [0| L], binary::p(L).

	succeeds(coinduction_binary_3) :-
		L = [1,0,1| L], binary::p(L).

	succeeds(coinduction_streams_1) :-
		bagof(T, streams::nat_stream([0, s(0), s(s(0))| T]), [T1, T2, T3]),
		T1 == [s(s(0))| T1],
		T2 == [s(0), s(s(0))| T2],
		T3 == [0, s(0), s(s(0))| T3].

	succeeds(coinduction_streams_2) :-
		X = [0, 1, 1, 0| X], streams::bit_stream(X).

	succeeds(coinduction_lists_1) :-
		X = [1, 2, 3| X], lists::comember(2, X).

	fails(coinduction_lists_2) :-
		X = [1, 2, 3, 1, 2, 3], lists::comember(2, X).

	fails(coinduction_lists_3) :-
		X = [0, s(0), s(s(0))], lists::comember(s(0), X).

	succeeds(coinduction_lists_4) :-
		X = [0, s(0), s(s(0))| X], lists::comember(s(0), X).

	succeeds(coinduction_automata_1) :-
		bagof(X, automata::automata(X, s0), [X1, X2]),
		X1 == [a, b, c, d| X1],
		X2 == [a, b, e| X2].

:- end_object.
