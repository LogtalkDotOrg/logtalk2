
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "multifile" example.']).

	test(multifile_1) :-
		findall(X, main::a(X), Solutions),
		Solutions == [1,2,3,4,5].

:- end_object.
