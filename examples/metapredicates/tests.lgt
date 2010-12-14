
:- object(library).

	:- public(my_call/1).
	:- meta_predicate(my_call(0)).

	my_call(Goal) :-
		call(Goal).

	a(library).

:- end_object.



:- object(client).

	:- public(test1/1).

	test1(L) :-
		library::my_call(setof(E, a(E), L)).

	:- public(test2/1).

	test2(L) :-
		library::my_call(setof(E, call(a, E), L)).

	:- public(test3/1).

	test3(L) :-
		library::my_call(call(setof, E, call(a, E), L)).

	a(1). a(2). a(3).

:- end_object.



:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.11,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/12/14,
		comment is 'Unit tests for the "metapredicates" example.']).

	% This example defines a plain Prolog predicate even_integer/1. As the
	% definition is in the pseudo-object "user", moving the call to another object
	% switches the calling context to that object, so even_integer/1 is no longer
	% found.  To solve, copy the definition of even_integer/1 and sum_squares/3 are
	% copied the testing object from "predicates.lgt".

	% some simple predicates to use with library meta-predicates (e.g. fold_left/4
	% and partition/4) compiled as plain Prolog code and thus defined in the "user"
	% pseudo-object:

	sum_squares(X, Y, Z) :-
		Z is X*X + Y*Y.

	even_integer(I) :-
		I mod 2 =:= 0.

	test(metapredicates_1) :-
		sort(user)::sort([3,1,4,2,9], Sorted),
		Sorted == [1, 2, 3, 4, 9].

	test(metapredicates_2) :-
		meta::partition(even_integer, [1,2,3,4,5], Included, Excluded),
		Included == [2, 4], Excluded == [1, 3, 5].

	test(metapredicates_3) :-
		meta::fold_left(sum_squares, 0, [1,2,3], Result),
		Result == 34.

	test(metapredicates_4) :-
		meta::fold_left(atom_concat, 'PREFIX', [abc,def,ghi], Result),
		Result=='PREFIXabcdefghi'.

	test(metapredicates_5) :-
		meta::fold_right(atom_concat, 'SUFIX', [abc,def,ghi], Result),
		Result==abcdefghiSUFIX.

	test(metapredicates_6) :-
		meta::fold_left(predicates::sum, 0, [1,2,3,4,5], Result),
		Result == 15.

	test(metapredicates_7) :-
		meta::fold_left(predicates::product, 1, [1,2,3,4,5], Result),
		Result == 120.

	test(metapredicates_8) :-
		meta::fold_left(predicates::tuple, (0,0), [(1,2),(3,4),(6,4)], Result),
		Result == (10, 10).

	test(metapredicates_9) :-
		meta::scan_left(sum_squares, 0, [1,2,3], Result),
		Result == [0, 1, 5, 34].

	test(metapredicates_10) :-
		meta::scan_right(predicates::sum, 5, [1,2,3,4], Result),
		Result == [15, 14, 12, 9, 5].

	test(metapredicates_11) :-
		meta::map(integer, [1,2,3,4,5]).

	test(metapredicates_12) :-
		meta::map(char_code, [a,b,c,d,e], Codes),
		Codes == [97, 98, 99, 100, 101].

	% tests for calling meta-predicates with other meta-predicates as meta-arguments

	test(metapredicates_13) :-
		client::test1(L),
		L == [1, 2, 3].

	test(metapredicates_14) :-
		client::test2(L),
		L == [1, 2, 3].

	test(metapredicates_15) :-
		client::test3(L),
		L == [1, 2, 3].

:- end_object.
