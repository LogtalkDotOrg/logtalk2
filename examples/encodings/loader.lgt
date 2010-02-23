
:- if((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == swi; Dialect == yap))).

	:- initialization(logtalk_load([asian, babel, latin])). 

:- else.

	:- initialization(logtalk_load([asian, babel, latin, mythology])). 

:- endif.
