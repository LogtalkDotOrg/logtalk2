
:- if((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == swi; Dialect == yap))).

	:- initialization((
		logtalk_load(library(types_loader)),
		logtalk_load(attvars_hook, [reload(skip)]),		% allow for static binding
		logtalk_load(domain, [hook(attvars_hook)])
	)).

:- else.

	:- initialization((
		write('WARNING: example not supported on this back-end Prolog compiler!'), nl
	)).

:- endif.
