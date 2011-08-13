
:- if(current_logtalk_flag(prolog_dialect, eclipse)).

	:- lib(ic).

	:- object(train).

		:- info([
			version is 0.3,
			author is 'Neda Saeedloei and Gopal Gupta. Adapted to Logtalk by Paulo Moura.',
			date is 2011/08/12,
			comment is 'Timed automata example.']).

		:- initialization(init_clocks).

		init_clocks :-
			setval(wall_clock, 0),
			setval(train_clock, 0),
     		setval(gate_clock, 0),
			setval(controller_clock, 0).

		:- public(driver/5).
		:- coinductive(driver/5).
	
		driver(S0, S1, S2, [X| Rest], [(X,T)| R]) :-
			getval(wall_clock, T),
			getval(train_clock, T0),
			getval(controller_clock, T1),
			getval(gate_clock, T2),
			train(S0, X, S00, T, T0, T00),
			gate(S1, X, S10, T, T1, T10),
			controller(S2, X, S20, T, T2, T20),
			ic:(TA > T),
			setval(wall_clock, TA),
			setval(train_clock, T00),
			setval(controller_clock, T10),
    		setval(gate_clock, T20),
			driver(S00, S10, S20, Rest, R).

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
		train(X, up, X, _T1, T2, T2).
	
		controller(s0, approach, s1, T1, _T2, T3) :-
			ic:(T3 =:= T1).
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

:- elif((current_logtalk_flag(prolog_dialect, swi); current_logtalk_flag(prolog_dialect, yap))).

	:- use_module(library(clpr), []).
	
	:- object(train).
	
		:- info([
			version is 0.3,
			author is 'Neda Saeedloei and Gopal Gupta. Adapted to Logtalk by Paulo Moura.',
			date is 2011/08/12,
			comment is 'Timed automata example.']).

		:- initialization(init_clocks).

		init_clocks :-
			nb_setval(wall_clock, 0),
			nb_setval(train_clock, 0),
     		nb_setval(gate_clock, 0),
			nb_setval(controller_clock, 0).

		:- public(driver/5).
		:- coinductive(driver/5).
	
		driver(S0, S1, S2, [X| Rest], [(X,T)| R]) :-
			b_getval(wall_clock, T),
			b_getval(train_clock, T0),
			b_getval(controller_clock, T1),
			b_getval(gate_clock, T2),
			train(S0, X, S00, T, T0, T00),
			gate(S1, X, S10, T, T1, T10),
			controller(S2, X, S20, T, T2, T20),
			clpr:{TA > T},
			b_setval(wall_clock, TA),
			b_setval(train_clock, T00),
			b_setval(controller_clock, T10),
    		b_setval(gate_clock, T20),
			driver(S00, S10, S20, Rest, R).
	
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
		train(X, up, X, _T1, T2, T2).
	
		controller(s0, approach, s1, T1, _T2, T3) :-
			clpr:{T3 = T1}.
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
