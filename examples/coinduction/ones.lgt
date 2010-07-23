
:- object(ones).

	:- info([
		version is 0.1,
		author is 'Gopal Gupta et al. Adapted to Logtalk by Paulo Moura.',
		date is 2010/07/23,
		comment is 'Coinduction example.']).

	:- public(p/1).
	:- coinductive(p/1).

	p([1|T]) :-
		p(T).

:- end_object.
