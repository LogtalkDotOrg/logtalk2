
:- if((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == swi; Dialect == yap))).

	% SWI-Prolog and YAP don't support UTF-32
	:- initialization(logtalk_load([asian, babel, latin])). 

:- elif((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == cx; Dialect == sicstus))).

	:- initialization(logtalk_load([asian, babel, latin, mythology])). 

:- else.

	:- initialization((write('WARNING: example not supported on this back-end Prolog compiler!'), nl)).

:- endif.
