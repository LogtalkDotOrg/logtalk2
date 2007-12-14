
:- object(object).

	:- info([
		version is 1.1,
		author is pm,
		date is 2007/12/3,
		comment is 'Example object for illustrating the use of compiler hooks.']).

	:- public(out/0).

	out :-
		write('A'), nl,
		write(x(A, A)), nl,
		write(3), nl.

:- end_object.
