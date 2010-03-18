
:- initialization((
	logtalk_load(library(lgtunit_loader)),
%	logtalk_load(errors_loader),
%	logtalk_load(warnings_loader),
	logtalk_load(tests, [hook(lgtunit)]),
	tests::run
)).
