
:- if(current_logtalk_flag(tabling, supported)).

	:- initialization((
		logtalk_load(library(lgtunit_loader)),
		logtalk_load(loader),
		logtalk_load(tests, [hook(lgtunit)]),
		tests::run
	)).

:- endif.
