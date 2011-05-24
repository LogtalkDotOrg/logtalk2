
:- initialization((
	logtalk_load(library(types_loader)),
	logtalk_load(library(pairs), [reload(skip)]),
	logtalk_load(cc(loader), [reload(skip)]),
	logtalk_load([diagrams])
)).
