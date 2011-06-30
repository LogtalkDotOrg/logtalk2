
:- object(tangle).

	:- info([
		version is 1.0,
		author is 'Feliks Kluzniak. Adapted to Logtalk by Paulo Moura.',
		date is 2011/06/28,
		comment is 'Coinduction example of a predicate with two starting points and no common solution prefix.']).

	:- public(p/1).
	:- coinductive(p/1).
	p([a| X]) :- q(X).
	p([c| X]) :- r(X).

	:- coinductive(q/1).
	q([b| X]) :- p(X).

	:- coinductive(r/1).
	r([d| X]) :- p(X).

:- end_object.
