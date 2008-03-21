
:- category(areas).

	:- info([
		version is 1.0,
		author is 'Paul Crocker',
		date is 2008/03/19,
		comment is 'Interval and trapezium area predicates for quadrature methods.']).

	interval_area(_, Left, Right, 0, _, Acc, Area) :-
		Area is (Right-Left)*Acc,
		!.
	interval_area(Function, Left, Right, N, NP, Acc, Area) :-
		c(NP, N, C),
		w(NP, N, W),
		XK is Left + (Right-Left)*C,
		Function::eval(XK, Y),
		N2 is N - 1,
		Acc2 is Acc + W*Y,
		interval_area(Function, Left, Right, N2, NP, Acc2, Area).

	trapezium_area(Left, Right, Fleft, Fright, Area) :-
		Area is 0.5*(Right-Left)*(Fright+Fleft).

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

:- end_category.
