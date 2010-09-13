
:- if(current_logtalk_flag(prolog_dialect, xsb)).

	% XSB doesn't support static multifile predicates
	:- initialization((
		write('(not applicable)'), nl
	)).

:- elif(current_logtalk_flag(prolog_dialect, qp)).

	% Qu-Prolog doesn't support static multifile predicates
	:- initialization((
		write('(not applicable)'), nl
	)).

:- else.

	:- initialization((
		set_logtalk_flag(report, warnings),
		logtalk_load(library(lgtunit_loader)),
		logtalk_load(loader),
		logtalk_load(tests, [hook(lgtunit)]),
		tests::run
	)).

:- endif.