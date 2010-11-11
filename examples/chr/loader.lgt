
:- if(current_logtalk_flag(prolog_dialect, swi)).
	:- set_prolog_flag(iso, false).
	:- ensure_loaded(library(chr)).
:- elif(current_logtalk_flag(prolog_dialect, yap)).
	:- ensure_loaded(library(chr)).
	:- op(200, fy, ?).						% workaround a YAP-CHR operator bug
:- elif(current_logtalk_flag(prolog_dialect, qp)).
	:- chr_init.
:- endif.

:- if((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == swi; Dialect == yap))).
	:- initialization((
		logtalk_load(chr_hook, [reload(skip)]),	% allow for static binding
		logtalk_load(dom, [hook(chr_hook)])		% only a single object or category containing
	%	logtalk_load(leq, [hook(chr_hook)])		% CHR code can be loaded at a time
	)).
:- elif(current_logtalk_flag(prolog_dialect, qp)).
	:- initialization((
		logtalk_load(chr_hook, [reload(skip)]),	% allow for static binding
		logtalk_load(dom, [hook(chr_hook)]),
		logtalk_load(leq, [hook(chr_hook)])
	)).
:- endif.
