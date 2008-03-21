
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



:- object(f1,
	implements(evaluating)).

	% exp(x) [0,4] integral = 53.59815

	eval(X, Y) :-
		Y is exp(X).

:- end_object.



:- object(f2,
	implements(evaluating)).

	% exp(-x*x) [1,1.5] integral = 0.1093643

	eval(X, Y) :-
		Y is exp(-(X*X)).

:- end_object.



:- object(f3,
	implements(evaluating)).

	% 5/1+4x*x [-2,2 ] integral = 5 atan 4 = 6.6.29088

	eval(X, Y) :-
		Y is 5.0/(1.0 + 4.0*X*X).

:- end_object.



:- object(f4,
	implements(evaluating)).

	% (4*x-x+x*x)exp(x*x) [0,2] integral = (e**4-5)/2 = 24.79907 

	eval(X, Y) :-
		Y is (4.0*X-(X*X*X))*exp(X*X).

:- end_object.



:- object(f5,
	implements(evaluating)).

	% 1/xsin(1/x) [0.001 1]

	eval(X, Y) :-
		Y is (1.0/X)*sin(1.0/X).

:- end_object.



:- object(f6,
	implements(evaluating)).

	% 1/xsin(1/x) - 1/(1-x)sin(1/(1-x)) [0.001 0.999]

	eval(X, Y) :-
		S is 1-X,
		Y is ((1.0/X)*sin(1.0/X) * (1.0/S)*sin(1.0/S)).

:- end_object.
