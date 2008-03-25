
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

	% 1/xsin(1/x) - 1/(1-x)sin(1/(1-x)) [0.001 0.999] integral = 0.099109

	eval(X, Y) :-
		S is 1-X,
		Y is sin(1.0/X)/X - sin(1.0/S)/S.

:- end_object.



:- object(f1,
	implements(evaluating)).

	% exp(-x*x) [1,1.5] integral = 0.1093643

	eval(X, Y) :-
		Y is exp(-(X*X)).

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
		Y is (4.0*X-X*X*X))*exp(X*X).

:- end_object.



:- object(f4,
	implements(evaluating)).

	% 1/xsin(1/x) [0.001 1] integral = 0.62415

	eval(X, Y) :-
		Y is sin(1.0/X)/X.

:- end_object.
