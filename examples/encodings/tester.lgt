
:- if(\+ current_logtalk_flag(encoding_directive, unsupported)).

	:- initialization((
		set_logtalk_flag(report, warnings),
		logtalk_load(library(lgtunit_loader)),
		logtalk_load(loader),
		logtalk_load(tests, [hook(lgtunit)]),
		tests::run
	)).

:- endif.
