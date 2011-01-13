
:- if(current_logtalk_flag(prolog_dialect, swi)).
	:- use_module(library(statistics)).
:- endif.

:- initialization((
	logtalk_load(library(meta_compiler_loader)),
	set_logtalk_flag(hook, meta_compiler),
	logtalk_load(lambdas(lambdas))
)).
