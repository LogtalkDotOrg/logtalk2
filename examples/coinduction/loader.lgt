
:- if(current_logtalk_flag(coinduction, supported)).

	:- if(current_logtalk_flag(prolog_dialect, cx)).
		:- write_depth(10, 10).
	:- endif.

	:- if((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == eclipse; Dialect == swi; Dialect == yap))).
		:- initialization((
			logtalk_load(coinduction, [reload(skip)]),
			logtalk_load([simple, pta, train], [hook(coinduction)])
		)).
	:- endif.

	:- initialization(
		logtalk_load([
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
