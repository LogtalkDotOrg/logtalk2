
:- initialization((
	logtalk_load([types_loader, gensym], [reload(skip)]),
	logtalk_load([metap, meta, meta_compiler], [reload(skip)])
)).
