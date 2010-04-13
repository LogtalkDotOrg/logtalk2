
:- initialization((
	set_logtalk_flag(report, warnings),
	logtalk_load(library(lgtunit_loader)),
	logtalk_load(loader),
	catch(logtalk_load(client), _,	true),	% also test parsing of use_module/1 directives
	logtalk_load(tests, [hook(lgtunit)]),
	tests::run
)).
