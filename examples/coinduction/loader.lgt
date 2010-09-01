
:- if(current_logtalk_flag(coinduction, supported)).

	:- initialization((
		logtalk_load(coinduction, [reload(skip)]),
		logtalk_load(simple, [hook(coinduction)]),
		logtalk_load([
			binary,
			streams,
			lists,
			automata,
			counter,
			nested,
			pta,
			train
		])
	)).

:- else.

	:- initialization((write('ERROR: coinduction not supported!'), nl)).

:- endif.
