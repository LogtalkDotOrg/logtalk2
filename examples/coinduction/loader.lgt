
:- if(current_logtalk_flag(coinduction, supported)).

	:- if(current_logtalk_flag(prolog_dialect, yap)).
		:- initialization((
			current_prolog_flag(toplevel_print_options, Options),
			set_prolog_flag(toplevel_print_options, [max_depth(10)| Options])
		)).
	:- endif.

	:- if((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == eclipse; Dialect == sicstus; Dialect == swi; Dialect == yap))).
		:- initialization((
			logtalk_load(library(streamvars), [reload(skip)]),
			logtalk_load([pta, train, cotrain])
		)).
	:- endif.

	:- initialization(
		logtalk_load([
			simple,
			binary,
			streams,
			filter,
			sieve,
			lists,
			sorting,
			automata,
			counter,
			nested,
			cyclic_paths,
			shared_paths,
			tangle
		])
	).

:- else.

	:- initialization((write('ERROR: coinduction not supported!'), nl)).

:- endif.
