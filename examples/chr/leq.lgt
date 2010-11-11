%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% simple constraint solver for inequalities between variables
%% thom fruehwirth ECRC 950519, LMU 980207, 980311
%%
%% ported to hProlog by Tom Schrijvers 
%% 
%% ported to Logtalk by Paulo Moura

:- object(leq).

	:- public(leq/2).
	:- chr_constraint(leq/2).

	reflexivity  @ leq(X,X) <=> true.
	antisymmetry @ leq(X,Y), leq(Y,X) <=> X = Y.
	idempotence  @ leq(X,Y) \ leq(X,Y) <=> true.
	transitivity @ leq(X,Y), leq(Y,Z) ==> leq(X,Z).

:- end_object.
