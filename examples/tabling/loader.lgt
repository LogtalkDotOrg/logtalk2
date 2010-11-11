
:- if(current_logtalk_flag(tabling, supported)).

	:- initialization(logtalk_load(tabling)). 

:- else.

	:- initialization((write('WARNING: example not supported on this back-end Prolog compiler!'), nl)).

:- endif.
