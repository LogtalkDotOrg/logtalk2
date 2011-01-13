
:- initialization((
	logtalk_load([types_loader, gensym], [reload(skip)]),
	logtalk_load([meta_compiler], [reload(skip)])
)).
