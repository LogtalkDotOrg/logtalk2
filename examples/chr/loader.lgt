
:- if(current_logtalk_flag(prolog_dialect, swi)).
	:- set_prolog_flag(iso, false).
	:- ensure_loaded(library(chr)).
:- elif(current_logtalk_flag(prolog_dialect, yap)).
	:- ensure_loaded(library(chr)).
	:- op(200, fy, ?).						% workaround a YAP-CHR operator bug
:- endif.

:- ensure_loaded(library(lists)).

:- initialization((
	logtalk_load(chr_hook, [reload(skip)]),	% allow for static binding
	logtalk_load(dom, [hook(chr_hook)])
)).
