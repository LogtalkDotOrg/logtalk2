
:- object(database).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2007/04/17,
		comment is 'Dynamic database benchmark utility predicates.']).

	:- public([db_test_this/0, db_test_self/0, db_test_obj/0]).

	:- private([pred_this/0, pred_self/0, pred_obj/0]).
	:- dynamic([pred_this/0, pred_self/0, pred_obj/0]).

	% direct calls to assertz/1 and retract/1:
	db_test_this :-
		{repeat(100)},
			assertz(pred_this),
		fail.
	db_test_this :-
		retract(pred_this),
		fail.
	db_test_this.

	% calls to assertz/1 and retract/1 using ::/1:
	db_test_self :-
		{repeat(100)},
			::assertz(pred_self),
		fail.
	db_test_self :-
		::retract(pred_self),
		fail.
	db_test_self.

	% calls to assertz/1 and retract/1 using ::/2:
	db_test_obj :-
		this(This),
		{repeat(100)},
			This::assertz(pred_obj),
		fail.
	db_test_obj :-
		this(This),
		This::retract(pred_obj),
		fail.
	db_test_obj.

:- end_object.
