
:- object(coeficients).

	:- info([
		version is 1.0,
		author is 'Paul Crocker',
		date is 2008/03/19,
		comment is 'Orthogonal Polinomials']).

	% static data for quadrature methods

	:- public(w/3).
	:- public(c/3).

	w(1,1, 1.0).
	w(2,1, 0.5).
	w(2,2, 0.5).
	w(3,1, 0.2777777777).
	w(3,2, 0.4444444444).
	w(3,3, 0.2777777777).
	w(4,1, 0.17392742256872692868).
	w(4,2, 0.32607257743127307131).
	w(4,3, 0.32607257743127307131).
	w(4,4, 0.17392742256872692868).

	c(1,1, 0.5).
	c(2,1, 0.211324865405187117745).
	c(2,2, 1.0-0.211324865405187117745).
	c(3,1, 0.1127016653792583114811).
	c(3,2, 0.5).
	c(3,3, 1.0-0.1127016653792583114811).
	c(4,1, 0.06943184420973712388).
	c(4,2, 0.330009478207571867598).
	c(4,3, 1-0.330009478207571867598).
	c(4,4, 1-0.06943184420973712388).

	:- public(legendre/3).
	:- mode(  legendre(+float,+integer,-float), one).
	:- info(  legendre/3, [
		comment is 'Calculate Pnx where Pn is the Nth Legendre Polynomial',
		argnames is ['X', 'N', 'Pnx']]).

	legendre(_, 0, 1.0) :- !.
	legendre(X, 1,   X) :- !.
	legendre(X, N,  PX) :-
		N > 1,
		!,
		legendre(X, N, 2, X, 1.0, PX).
	legendre(X, N, N, PN1, PN2, PX) :-
		!,
		PX is ((2.0*N-1.0)*X*PN1-(N-1.0)*PN2)/N.
	legendre(X, N, Ncurrent, PN1, PN2, PX) :-
		PN is ((2.0*Ncurrent-1.0)*X*PN1-(Ncurrent-1.0)*PN2)/Ncurrent,	
		PN2_NEW is PN1,
		Ncurr is Ncurrent + 1,	
		legendre(X, N, Ncurr, PN, PN2_NEW, PX).

:- end_object.
