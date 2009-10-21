
:- object(database,
	imports(solver, proof_tree, tracer)).

	:- public(p/1).
	:- private([q/2, r/1, s/1, t/2]).
	:- dynamic([p/1, q/2, r/1, s/1, t/2]).

	p(X) :- q(X, Y), r(Y).

	q(X, Y) :- s(X), t(X, Y).

	r(a).
	r(b).

	s(1).
	s(2).
	s(3).

	t(1, a).
	t(2, b).

:- end_object.
