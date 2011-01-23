
:- initialization((
	logtalk_load(library(types_loader)),
	logtalk_load([expansion, hooks]),
	logtalk_load(raw, [hook(hh)])
)).
