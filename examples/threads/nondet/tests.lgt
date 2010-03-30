
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "threads/nondet" example.']).

	:- threaded.

	test(nondet_1) :-
		threaded_call(lists::member(_, [1,2,3])).

	test(nondet_2) :-
		findall(X, threaded_exit(lists::member(X, [1,2,3])), Xs),
		Xs == [1, 2, 3].

	test(nondet_3) :-
		threaded_once(lists::member(_, [1,2,3])).

	test(nondet_4) :-
		findall(X, threaded_exit(lists::member(X, [1,2,3])), Xs),
		Xs == [1].

	test(nondet_5) :-
		threaded_call(lists::member(_, [1,2,3])),
		threaded_call(lists::member(_, [1,2,3])).

	test(nondet_6) :-
		findall(X, threaded_exit(lists::member(X, [1,2,3])), Xs),
		Xs == [1, 2, 3].

	test(nondet_7) :-
		findall(X, threaded_exit(lists::member(X, [1,2,3])), Xs),
		Xs == [1, 2, 3].

	test(nondet_8) :-
		threaded_call(lists::member(_, [1,2,3]), _),
		threaded_call(lists::member(_, [1,2,3]), Tag),
		findall(X, threaded_exit(lists::member(X, [1,2,3]), Tag), Xs),
		Xs == [1, 2, 3].

	test(nondet_9) :-
		threaded_call(lists::member(_, [1,2,3,2])).

	test(nondet_10) :-
		findall(1, threaded_exit(lists::member(2, [1,2,3,2])), L),
		L == [1, 1].

:- end_object.
