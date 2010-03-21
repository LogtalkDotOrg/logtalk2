
% XSB 3.2 doesn't support static multifile predicates
:- if(\+ current_logtalk_flag(prolog_dialect, xsb)).

	:- initialization((
		set_logtalk_flag(report, warnings),
		logtalk_load(library(lgtunit_loader)),
		logtalk_load(loader),
		logtalk_load(tests, [hook(lgtunit)]),
		tests::run
	)).

:- endif.