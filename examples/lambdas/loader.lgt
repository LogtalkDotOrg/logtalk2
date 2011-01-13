
:- if(current_logtalk_flag(prolog_dialect, swi)).
	:- use_module(library(statistics)).
:- endif.

:- initialization((
	logtalk_load(library(metapredicates_loader)),
	logtalk_load(library(types_loader)),
	logtalk_load(lambdas)
)).
