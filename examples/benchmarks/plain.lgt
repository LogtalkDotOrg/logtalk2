
my_length(List, Length) :-
	(	integer(Length) ->
		Length >= 0,
		my_make_list(Length, List)
	;	my_length(List, 0, Length)
	).

my_make_list(0, []):-
	!.
my_make_list(N, [_| Tail]):-
	M is N-1,
	my_make_list(M, Tail).

my_length([], Length, Length).
my_length([_| Tail], Acc, Length) :-
	Acc2 is Acc + 1,
	my_length(Tail, Acc2, Length).


:- dynamic(pred_plain/0).

db_test_plain :-
	repeat(100),
		assertz(pred_plain),
	fail.
db_test_plain :-
	retract(pred_plain),
	fail.
db_test_plain.

% some Prolog compilers define the predicate repeat/1 as a built-in predicate;
% if that's the case of the Prolog compiler you are using, then comment out 
% the definition that follows

repeat(_).
repeat(N) :-
	N > 1,
	N2 is N - 1,
	repeat(N2).

% generate a list containing the first N non-negative integers

generate_list(N, List) :-
	generate_list(0, N, List).

generate_list(N, N, []) :-
	!.
generate_list(M, N, [M| Ms]) :-
	M < N,
	M2 is M + 1,
	generate_list(M2, N, Ms).
