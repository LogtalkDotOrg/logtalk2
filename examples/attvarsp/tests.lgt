
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2011/03/09,
		comment is 'Unit tests for the "attvars" example.']).

	:- if(current_object(domain)).
	test(attvars_1) :-
		\+ (domain::domain(X, [a,b]), X = c).

	test(attvars_2) :-
		domain::domain(X, [a,b]), domain::domain(X, [a,c]),
		X == a.

	test(attvars_3) :-
		domain::domain(X, [a,b,c]), domain::domain(X, [a,c]),
		domain::domain(X, List), List == [a,c].

	test(attvars_4) :-
		domain::domain(X, [a,b,c]), domain::domain(X, [a,c]),
		get_attr(X, domain, List), List == [a,c].
	:- endif.

	:- if(current_object(domain(_))).
	test(attvars_5) :-
		\+ (domain(atom)::domain(X, [a,b]), X = c).

	test(attvars_6) :-
		domain(integer)::domain(X, [1,2]), domain(integer)::domain(X, [1,3]),
		X == 1.

	test(attvars_7) :-
		domain(integer)::domain(X, [1,2,3]), domain(integer)::domain(X, [1,3]),
		domain(integer)::domain(X, List), List == [1,3].

	test(attvars_8) :-
		domain(integer)::domain(X, [1,2,3]), domain(integer)::domain(X, [1,3]),
		get_attr(X, domain(integer), List), List == [1,3].
	:- endif.

:- end_object.
