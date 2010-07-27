
:- object(simple).

	:- info([
		version is 0.1,
		author is 'Gopal Gupta et al. Adapted to Logtalk by Paulo Moura.',
		date is 2010/07/23,
		comment is 'Elementary coinduction predicate example.']).

	:- public(p/0).
	:- coinductive(p/0).

	p :-
		p.

:- end_object.
