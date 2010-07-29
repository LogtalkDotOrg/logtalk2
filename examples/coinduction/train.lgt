
:- if(current_logtalk_flag(prolog_dialect, gnu)).

	:- object(train).
	
		:- info([
			version is 0.1,
			author is 'Gopal Gupta et al. Adapted to Logtalk by Paulo Moura.',
			date is 2010/07/23,
			comment is 'Timed automata example.']).

		:- public(driver/9).
		:- coinductive(driver/9).
	
		driver(S0, S1, S2, T, T0, T1, T2, [X| Rest], [(X,T)| R]) :-
			train(S0, X, S00, T, T0, T00),
			controller(S1, X, S10, T, T1, T10),
			gate(S2, X, S20, T, T2, T20),
			'#>#'(TA, T),
			driver(S00, S10, S20, TA, T00, T10, T20, Rest, R).
	
		train(X, up, X, _T1, T2, T2).
		train(s0, approach, s1, T1, _T2, T3) :-
			'#=#'(T3, T1).
		train(s1, in, s2, T1, T2, T3) :-
			'#>#'(T1 - T2, 2), '#=#'(T3, T2).
		train(s2, out, s3, _T1, T2, T2).
		train(s3, exit, s0, T1, T2, T3) :-
			'#=#'(T3, T2), '#<#'(T1 - T2, 5).
		train(X, lower, X, _T1, T2, T2).
		train(X, down, X, _T1, T2, T2).
		train(X, raise, X, _T1, T2, T2).
	
		controller(s0, approach, s1, T1, _T2, T1).
		controller(s1, lower, s2, T1, T2, T3) :-
			'#=#'(T3, T2), '#=#'(T1 - T2, 1).
		controller(s2, exit, s3, T1, _T2, T1).
		controller(s3, raise, s0, T1, T2, T2) :-
			'#<#'(T1 - T2, 1).
		controller(X, in, X, _T1, T2, T2).
		controller(X, up, X, _T1, T2, T2).
		controller(X, out, X, _T1, T2, T2).
		controller(X, down, X, _T1, T2, T2).
	
		gate(s0, lower, s1, T1, _T2, T3) :-
			'#=#'(T3, T1).
		gate(s1, down, s2, T1, T2, T3) :-
			'#=#'(T3, T2), '#<#'(T1 - T2, 1).
		gate(s2, raise, s3, T1, _T2, T3) :-
			'#=#'(T3, T1).
		gate(s3, up, s0, T1, T2, T3) :-
			'#=#'(T3, T2), '#>#'(T1 - T2, 1), '#<#'(T1 - T2, 2).
		gate(X, approach, X, _T1, T2, T2).
		gate(X, in, X, _T1, T2, T2).
		gate(X, out, X, _T1, T2, T2).
		gate(X, exit, X, _T1, T2, T2).
	
	:- end_object.

:- elif(current_logtalk_flag(prolog_dialect, eclipse)).

	:- lib(ic).

	:- object(train).

		:- info([
			version is 0.1,
			author is 'Gopal Gupta et al. Adapted to Logtalk by Paulo Moura.',
			date is 2010/07/23,
			comment is 'Timed automata example.']).

		:- public(driver/9).
		:- coinductive(driver/9).

		driver(S0, S1, S2, T, T0, T1, T2, [X| Rest], [(X,T)| R]) :-
			train(S0, X, S00, T, T0, T00),
			controller(S1, X, S10, T, T1, T10),
			gate(S2, X, S20, T, T2, T20),
			ic:(TA > T),
			driver(S00, S10, S20, TA, T00, T10, T20, Rest, R).

		train(X, up, X, _T1, T2, T2).
		train(s0, approach, s1, T1, _T2, T3) :-
			ic:(T3 =:= T1).
		train(s1, in, s2, T1, T2, T3) :-
			ic:(T1 - T2 > 2), ic:(T3 =:= T2).
		train(s2, out, s3, _T1, T2, T2).
		train(s3, exit, s0, T1, T2, T3) :-
			ic:(T3 =:= T2), ic:(T1 - T2 < 5).
		train(X, lower, X, _T1, T2, T2).
		train(X, down, X, _T1, T2, T2).
		train(X, raise, X, _T1, T2, T2).
	
		controller(s0, approach, s1, T1, _T2, T1).
		controller(s1, lower, s2, T1, T2, T3) :-
			ic:(T3 =:= T2), ic:(T1 - T2 =:= 1).
		controller(s2, exit, s3, T1, _T2, T1).
		controller(s3, raise, s0, T1, T2, T2) :-
			ic:(T1 - T2 < 1).
		controller(X, in, X, _T1, T2, T2).
		controller(X, up, X, _T1, T2, T2).
		controller(X, out, X, _T1, T2, T2).
		controller(X, down, X, _T1, T2, T2).
	
		gate(s0, lower, s1, T1, _T2, T3) :-
			ic:(T3 =:= T1).
		gate(s1, down, s2, T1, T2, T3) :-
			ic:(T3 =:= T2), ic:(T1 - T2 < 1).
		gate(s2, raise, s3, T1, _T2, T3) :-
			ic:(T3 =:= T1).
		gate(s3, up, s0, T1, T2, T3) :-
			ic:(T3 =:= T2), ic:(T1 - T2 > 1), ic:(T1 - T2 < 2).
		gate(X, approach, X, _T1, T2, T2).
		gate(X, in, X, _T1, T2, T2).
		gate(X, out, X, _T1, T2, T2).
		gate(X, exit, X, _T1, T2, T2).

	:- end_object.

:- elif(current_logtalk_flag(prolog_dialect, cx)).

	% CxProlog doesn't support constraints

:- else.	% assume either SWI-Prolog or YAP

	:- use_module(library(clpr), []).
	
	:- object(train).
	
		:- info([
			version is 0.1,
			author is 'Gopal Gupta et al. Adapted to Logtalk by Paulo Moura.',
			date is 2010/07/23,
			comment is 'Timed automata example.']).
	
		:- public(driver/9).
		:- coinductive(driver/9).
	
		driver(S0, S1, S2, T, T0, T1, T2, [X| Rest], [(X,T)| R]) :-
			train(S0, X, S00, T, T0, T00),
			controller(S1, X, S10, T, T1, T10),
			gate(S2, X, S20, T, T2, T20),
			clpr:{TA > T},
			driver(S00, S10, S20, TA, T00, T10, T20, Rest, R).
	
		train(X, up, X, _T1, T2, T2).
		train(s0, approach, s1, T1, _T2, T3) :-
			clpr:{T3 = T1}.
		train(s1, in, s2, T1, T2, T3) :-
			clpr:{T1 - T2 > 2, T3 = T2}.
		train(s2, out, s3, _T1, T2, T2).
		train(s3, exit, s0, T1, T2, T3) :-
			clpr:{T3 = T2, T1 - T2 < 5}.
		train(X, lower, X, _T1, T2, T2).
		train(X, down, X, _T1, T2, T2).
		train(X, raise, X, _T1, T2, T2).
	
		controller(s0, approach, s1, T1, _T2, T1).
		controller(s1, lower, s2, T1, T2, T3) :-
			clpr:{T3 = T2, T1 - T2 = 1}.
		controller(s2, exit, s3, T1, _T2, T1).
		controller(s3, raise, s0, T1, T2, T2) :-
			clpr:{T1 - T2 < 1}.
		controller(X, in, X, _T1, T2, T2).
		controller(X, up, X, _T1, T2, T2).
		controller(X, out, X, _T1, T2, T2).
		controller(X, down, X, _T1, T2, T2).
	
		gate(s0, lower, s1, T1, _T2, T3) :-
			clpr:{T3 = T1}.
		gate(s1, down, s2, T1, T2, T3) :-
			clpr:{T3 = T2, T1 - T2 < 1}.
		gate(s2, raise, s3, T1, _T2, T3) :-
			clpr:{T3 = T1}.
		gate(s3, up, s0, T1, T2, T3) :-
			clpr:{T3 = T2, T1 - T2 > 1, T1 - T2 < 2}.
		gate(X, approach, X, _T1, T2, T2).
		gate(X, in, X, _T1, T2, T2).
		gate(X, out, X, _T1, T2, T2).
		gate(X, exit, X, _T1, T2, T2).
	
	:- end_object.

:- endif.
