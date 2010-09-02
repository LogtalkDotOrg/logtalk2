
:- if(current_logtalk_flag(coinduction, supported)).

	:- if(current_logtalk_flag(prolog_dialect, cx)).
		:- write_depth(10, 10).
	:- endif.

	:- if((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == eclipse; Dialect == swi; Dialect == yap))).
		:- initialization(logtalk_load([pta, train])).
	:- endif.

	:- initialization(
		logtalk_load([
			simple,
			binary,
			streams,
			lists,
			automata,
			counter,
			nested
		])
	).

:- else.

	:- initialization((write('ERROR: coinduction not supported!'), nl)).

:- endif.
