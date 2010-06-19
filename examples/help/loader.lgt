
:- if((
	current_logtalk_flag(prolog_dialect, Dialect),
	(Dialect == swi; Dialect == yap; Dialect == xsb; Dialect == gnu; Dialect == cx;
	 Dialect == b; Dialect == sicstus; Dialect == eclipse; Dialect == ciao)
)).

	:- initialization((
		logtalk_load(cc(loader), [report(off)]),
		logtalk_load(help, [report(off)]),
		write('For help on Logtalk, type help::help.'), nl
	)).

:- endif.
