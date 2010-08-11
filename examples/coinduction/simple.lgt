
:- object(simple).

	:- info([
		version is 0.1,
		author is 'Gopal Gupta et al. Adapted to Logtalk by Paulo Moura.',
		date is 2010/07/23,
		comment is 'Elementary coinduction predicate example.']).

	:- public(p/0).
	:- coinductive1(p/0).

	p :- p.

	:- public(p/1).
	:- coinductive1(p/1).

	p(X) :- q(X).

	q(X) :- r(X).

	r(X) :- p(X).

:- end_object.
