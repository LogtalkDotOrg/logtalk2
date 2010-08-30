
:- object(simple).

	:- info([
		version is 0.2,
		author is 'Gopal Gupta et al. Adapted to Logtalk by Paulo Moura.',
		date is 2010/08/31,
		comment is 'Elementary coinduction predicate example.']).

	:- public(p/0).
	:- coinductive(p/0).

	p :- p.

	:- public(p/1).
	:- coinductive(p/1).

	p(X) :- q(X).

	q(X) :- r(X).

	r(X) :- p(X).

	:- public(p/2).
	:- coinductive(p/2).

	p(_, Y) :- 
		findall(T, s(T), Bag),
		member(Y, Bag).

	s(X) :- t(X).

	t(X) :- p(X, _).

	member(H, [H| _]).
	member(H, [_| T]) :-
		member(H, T).

:- end_object.
