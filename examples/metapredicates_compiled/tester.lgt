
:- initialization((
	set_logtalk_flag(report, warnings),
	logtalk_load(library(lgtunit_loader)),
	logtalk_load(loader),
	logtalk_load(metapredicates(tests), [hook(lgtunit)]),
	tests::run
)).
