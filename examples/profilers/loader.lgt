
:- initialization(logtalk_load(profilerp, [reload(skip)])).


:- if(current_logtalk_flag(prolog_dialect, yap)).

	:- initialization(logtalk_load(yap_profiler, [reload(skip)])).

:- elif(current_logtalk_flag(prolog_dialect, sicstus)).

	:- initialization(logtalk_load(sicstus_profiler, [reload(skip)])).

:- elif(current_logtalk_flag(prolog_dialect, swi)).

	:- initialization((
		nl,
		write('Logtalk natively supports the SWI-Prolog XPCE profiler.'),
		nl
	)).

:- else.

	:- initialization((
		nl,
		write('Your back-end Prolog compiler does not support a suitable profiler.'),
		nl
	)).

:- endif.
