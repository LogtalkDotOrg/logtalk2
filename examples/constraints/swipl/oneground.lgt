
% code adapted to Logtalk by Paulo Moura from the CLP(FD)
% documentation by Markus Triska


:- object(oneground).

	:- public(oneground/3).

	oneground(X, Y, Z) :-
		clpfd:make_propagator(oneground(X, Y, Z), Prop),
		clpfd:init_propagator(X, Prop),
		clpfd:init_propagator(Y, Prop),
		clpfd:trigger_once(Prop).

	:- multifile(clpfd:run_propagator/2).
	clpfd:run_propagator(oneground(X, Y, Z), MState) :-
		(	integer(X) -> clpfd:kill(MState), Z = 1
		;	integer(Y) -> clpfd:kill(MState), Z = 1
		;	true
		).

:- end_object.
