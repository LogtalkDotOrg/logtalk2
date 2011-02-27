
:- if((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == swi; Dialect == yap))).

	:- initialization((
		logtalk_load(library(types_loader)),
		logtalk_load(attvars_hook, [reload(skip)]),		% allow for static binding
		logtalk_load(domain, [hook(attvars_hook)])
	)).

:- elif(current_logtalk_flag(prolog_dialect, xsb)).

	:- import(from(/(put_attr,3), machine)).
	:- import(from(/(get_attr,3), machine)).
	:- import(from(/(del_attr,2), machine)).
	:- import(from(/(install_verify_attribute_handler,4), machine)).
	:- import(from(/(install_attribute_portray_hook,3), machine)).

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
