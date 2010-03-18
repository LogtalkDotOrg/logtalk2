
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "metainterpreters" example.']).

	test(metainterpreters_1) :-
		findall(X,database::p(X),Solutions),
		Solutions == [1,2].

	test(metainterpreters_2) :-
		findall(X,database::solve(p(X)),Solutions),
		Solutions == [1,2].

	test(metainterpreters_3) :-
		findall(X-Tree,database::proof_tree(p(X), Tree),Solutions),
		Solutions == [1-(p(1):- (q(1, a):- (s(1):-true), (t(1, a):-true)), (r(a):-true)),2-(p(2):- (q(2, b):- (s(2):-true), (t(2, b):-true)), (r(b):-true))].

	test(metainterpreters_4) :-
		findall(Weather, rules::prove(weather(Weather)),Solutions),
		Solutions == [raining].

	test(metainterpreters_5) :-
		findall(Where,rules::prove(goto(Where)),Solutions),
		Solutions == [cinema].

	test(metainterpreters_6) :-
		lists::steps(reverse([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30],_), Steps),
		Steps == 496.

:- end_object.
