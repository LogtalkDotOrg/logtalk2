
:- object(binary).

	:- info([
		version is 0.2,
		author is 'Gopal Gupta et al. Adapted to Logtalk by Paulo Moura.',
		date is 2010/07/23,
		comment is 'Coinduction example.']).

	:- public(p/1).
	:- coinductive(p/1).

	p([0| T]) :- p(T).
	p([1| T]) :- p(T).

:- end_object.
