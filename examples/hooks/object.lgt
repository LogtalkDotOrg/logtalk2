
:- object(object).

	:- info([
		version is 1.0,
		author is pm,
		date is 2006/01/29,
		comment is 'Example object for illustrating the use of compiler hooks.']).

	:- public(out/0).

	out :-
		write(1),
		write(2),
		write(3).

:- end_object.
