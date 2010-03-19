
:- initialization((
	set_logtalk_flag(report, warnings),
	logtalk_load(library(lgtunit_loader)),
	logtalk_load(loader_debug),
%	logtalk_load(loader_production),
	logtalk_load(tests, [hook(lgtunit)]),
	tests::run
)).
