
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "benchmarks" example.']).

	test(s11) :-
		benchmarks::run(s11, 10000).
	:- if(current_logtalk_flag(modules, supported)).
	test(s12) :-
		benchmarks::run(s12, 10000).
	:- endif.
	test(s13) :-
		benchmarks::run(s13, 10000).

	test(s21) :-
		benchmarks::run(s21, 10000).
	:- if(current_logtalk_flag(modules, supported)).
	test(s22) :-
		benchmarks::run(s22, 10000).
	:- endif.
	test(s23) :-
		benchmarks::run(s23, 10000).

	test(c1) :-
		benchmarks::run(c1, 10000).
	test(c2) :-
		benchmarks::run(c2, 10000).
	test(c3) :-
		benchmarks::run(c3, 10000).

	test(d1) :-
		benchmarks::run(d1, 10000).
	test(d2) :-
		benchmarks::run(d2, 10000).
	test(d3) :-
		benchmarks::run(d3, 10000).
	test(d4) :-
		benchmarks::run(d4, 10000).
	test(d5) :-
		benchmarks::run(d5, 10000).

:- end_object.
