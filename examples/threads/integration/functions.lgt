
:- protocol(evaluating).

	:- info([
		version is 1.0,
		author is 'Paul Crocker',
		date is 2006/11/26,
		comment is 'Default protocol for evaluating real functions of a single real variable.']).

	:- public(eval/2).
	:- mode(eval(+float, -float), one).
	:- info(eval/2, [
		comment is 'Calculates the function value.',
		argnames is ['X', 'Fx']]).

:- end_protocol.



:- object(const,
	implements(evaluating)).

	% exp(x) [0,4] integral = 8.0

	eval(_, 2).

:- end_object.



:- object(exp,
	implements(evaluating)).

	% exp(x) [0,4] integral = 53.59815

	eval(X, Y) :-
		Y is exp(X).

:- end_object.



:- object(log,
	implements(evaluating)).

	% log(x) [1,4] integral = 2.54518

	eval(X, Y) :-
		Y is log(X).

:- end_object.



:- object(sin,
	implements(evaluating)).

	% sin(x) [0.000,6.283] integral = 1.71694e-08

	eval(X, Y) :-
		Y is sin(X).

:- end_object.



:- object(quiver,
	implements(evaluating)).

	% [0.001, 0.999] integral ~= 0.0

	eval(X, Y) :-
		S is 1-X,
		Y is sin(1.0/X)/X - sin(1.0/S)/S.

:- end_object.



:- object(oscillate,
	implements(evaluating)).

	% cos((w+1)x) [-1,1] integral = 2 sin(w+1)/(w+1)
	% for w = 99  integral = -0.01012731282

	eval(X, Y) :-
		W is 99.0,
		Y is cos((W+1.0)*X).

:- end_object.



% the six functions that follows, test01..test06, are taken from the paper
% http://www.osti.gov/energycitations/servlets/purl/860346-LpvoMN/860346.PDF

:- object(test01,
	implements(evaluating)).

	% [0, 1] integral = 0.25

	eval(X, Y) :-
		Y is X*log(1+X).

:- end_object.



:- object(test02,
	implements(evaluating)).

	% [0, 1] integral = (pi - 2 + 2log2)/12 = 0,210657251226

	eval(X, Y) :-
		Y is X*X*atan(X).

:- end_object.



:- object(test03,
	implements(evaluating)).

	% [0, pi/2] integral = (e^(pi/2) - 1)/2 = 1,905238690483

	eval(X, Y) :-
		Y is exp(X)*cos(X).

:- end_object.



:- object(test04,
	implements(evaluating)).

 	% [0, 1] integral = (5*pi^2)/96 = 0.51404189589

	eval(X, Y) :-
		W is sqrt(2.0+X*X),
		Y is atan(W)/(W*(1.0+X*X)).

:- end_object.



:- object(test05,
	implements(evaluating)).

	% [0, 1] integral = -4/9 = 0,44444444444(4)

	eval(X, Y) :-
		Y is sqrt(X)*log(X).

:- end_object.



:- object(test06,
	implements(evaluating)).

	% [0, 1] integral = pi/9 = 0,349065850399

	eval(X, Y) :-
		Y is sqrt(1-X*X).

:- end_object.



:- object(f1,
	implements(evaluating)).

	% exp(-x*x) [1,1.5] integral = 0.1093643

	eval(X, Y) :-
		Y is exp(-X*X).

:- end_object.



:- object(f2,
	implements(evaluating)).

	% 5/1+4x*x [-2,2 ] integral = 5 atan 4 = 6.629088

	eval(X, Y) :-
		Y is 5.0/(1.0 + 4.0*X*X).

:- end_object.



:- object(f3,
	implements(evaluating)).

	% (4*x-x*x*x)exp(x*x) [0,2] integral = (e**4-5)/2 = 24.79907 

	eval(X, Y) :-
		Y is (4.0*X-X*X*X)*exp(X*X).

:- end_object.



:- object(f4,
	implements(evaluating)).

	% 1/xsin(1/x) [0.001 1] integral = 0.62415

	eval(X, Y) :-
		Y is sin(1.0/X)/X.

:- end_object.
