
:- object(beatles).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2007/10/23,
		comment is 'Simple example of using a barrier to synchronize a set of threads.']).

	:- threaded.

	:- public(sing_along/0).
	:- mode(sing_along, one).
	:- info(sing_along/0, [
		comment is 'Wait for all threads to say "hello" and then proceed with the threads saying "goodbye".']).

	sing(Thread) :-
		write(hello(Thread)), flush_output,
		threaded_notify(ready(Thread)),
		threaded_wait(go(Thread)),
		write(goodbye(Thread)), flush_output.

	sing_along :-
		threaded_ignore(sing(1)),
		threaded_ignore(sing(2)),
		threaded_ignore(sing(3)),
		threaded_ignore(sing(4)),
		threaded_wait([ready(1), ready(2), ready(3), ready(4)]),
		nl, write('Enough of hellos! Time for goodbyes!'), nl,
		threaded_notify([go(1), go(2), go(3), go(4)]).

:- end_object.
