
:- object(database).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2007/04/17,
		comment is 'Dynamic database benchmark utility predicates.']).
	
	:- public([db_test_this/1, db_test_self/1, db_test_obj/1]).
	
	:- private([pred_this/1, pred_self/1, pred_obj/1]).
	:- dynamic([pred_this/1, pred_self/1, pred_obj/1]).

	% direct calls to assertz/1 and retract/1:
	db_test_this(N) :-
		assertz(pred_this(N)),
		retract(pred_this(N)).

	% calls to assertz/1 and retract/1 using ::/1:
	db_test_self(N) :-
		::assertz(pred_self(N)),
		::retract(pred_self(N)).

	% calls to assertz/1 and retract/1 using ::/2:
	db_test_obj(N) :-
		this(This),
		This::assertz(pred_obj(N)),
		This::retract(pred_obj(N)).

:- end_object.
