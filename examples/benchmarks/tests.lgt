
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.2,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/11/15,
		comment is 'Unit tests for the "benchmarks" example.']).

	test(s11) :-
		benchmarks::run(s11, 20000).
	:- if(current_logtalk_flag(modules, supported)).
	test(s12) :-
		benchmarks::run(s12, 20000).
	:- endif.
	test(s13) :-
		benchmarks::run(s13, 20000).

	test(s21) :-
		benchmarks::run(s21, 20000).
	:- if(current_logtalk_flag(modules, supported)).
	test(s22) :-
		benchmarks::run(s22, 20000).
	:- endif.
	test(s23) :-
		benchmarks::run(s23, 20000).

	test(s31) :-
		benchmarks::run(s31, 20000).
	:- if(current_logtalk_flag(modules, supported)).
	test(s32) :-
		benchmarks::run(s32, 20000).
	:- endif.
	test(s33) :-
		benchmarks::run(s33, 20000).

	test(s41) :-
		benchmarks::run(s41, 20000).
	:- if(current_logtalk_flag(modules, supported)).
	test(s42) :-
		benchmarks::run(s42, 20000).
	:- endif.
	test(s43) :-
		benchmarks::run(s43, 20000).

	test(c1) :-
		benchmarks::run(c1, 20000).
	test(c2) :-
		benchmarks::run(c2, 20000).
	test(c3) :-
		benchmarks::run(c3, 20000).

	test(d1) :-
		benchmarks::run(d1, 20000).
	test(d2) :-
		benchmarks::run(d2, 20000).
	test(d3) :-
		benchmarks::run(d3, 20000).
	test(d4) :-
		benchmarks::run(d4, 20000).
	test(d5) :-
		benchmarks::run(d5, 20000).

:- end_object.
