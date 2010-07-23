
:- if(current_logtalk_flag(prolog_dialect, swi)).

	:- use_module(library(coinduction)).

	:- initialization((
		logtalk_load(coinduction),
		logtalk_load(ones, [hook(coinduction)]),
		logtalk_load(streams, [hook(coinduction)]),
		logtalk_load(lists, [hook(coinduction)]),
		logtalk_load(sieve, [hook(coinduction)])
	)).

:- endif.
