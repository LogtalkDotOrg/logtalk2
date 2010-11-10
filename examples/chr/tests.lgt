
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2010/11/09,
		comment is 'Unit tests for the "chr" example.']).

	test(chr_dom_1) :-
		dom::dom(A, [1,2,3]), dom::dom(A, [3,4,5]),
		A =:= 3.

:- end_object.
