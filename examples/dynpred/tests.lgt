
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "dynpred" example.']).

	:- discontiguous(succeeds/1).
	:- discontiguous(throws/2).

	succeeds(dynpred_1) :-
		findall(Value, descendant::p(Value), Solutions),
		Solutions == [root].

	succeeds(dynpred_2) :-
		descendant::assertz(p(descendant)),
		findall(Value, descendant::p(Value), Solutions),
		Solutions == [descendant].

	succeeds(dynpred_3) :-
		descendant::retractall(p(_)),
		findall(Value, descendant::p(Value), Solutions),
		Solutions == [root].

	throws(dynpred_4, error(existence_error(_,_),_,_)) :-
		class::p1(_).

	succeeds(dynpred_5) :-
		findall(X, instance::p1(X), Solutions),
		Solutions == [class].

	succeeds(dynpred_6) :-
		class::assertz(p2(class)).

	throws(dynpred_7, error(existence_error(_,_),_,_)) :-
		class::p2(_).

	succeeds(dynpred_8) :-
		findall(X, instance::p2(X), Solutions),
		Solutions == [class].

	succeeds(dynpred_9) :-
		prototype::(object_assert, self_assert, this_assert).

:- end_object.
