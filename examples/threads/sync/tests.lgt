
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "threads/sync" example.']).

	:- threaded.

	test(sync_1) :-
		threaded_call(nasty2::update_db(_)),
		threaded_call(nasty2::update_db(_)),
		threaded_call(nasty2::update_db(_)).

	test(sync_2) :-
		threaded_exit(nasty2::update_db(X)),
		threaded_exit(nasty2::update_db(Y)), X \== Y,
		threaded_exit(nasty2::update_db(Z)), X \== Z, Y \== Z.

:- end_object.
