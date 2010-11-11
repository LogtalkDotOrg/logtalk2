
:- if(\+ (current_logtalk_flag(prolog_dialect, Dialect), (Dialect == qp; Dialect == xsb))).

	:- initialization(logtalk_load([main, other, more])).

:- else.

	:- initialization((write('WARNING: example not supported on this back-end Prolog compiler!'), nl)).

:- endif.
