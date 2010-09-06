
:- if(current_logtalk_flag(coinduction, supported)).

	:- initialization((
		logtalk_load(library(lgtunit_loader)),
		logtalk_load(loader),
		logtalk_load(tests, [hook(lgtunit)]),
		tests::run
	)).

:- else.

	:- initialization((
		write('(not applicable)'), nl
	)).

:- endif.
