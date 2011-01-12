
:- initialization((
	logtalk_load([types_loader, gensym]),
	logtalk_load([metap, meta, loopp, loop, meta_compiler], [reload(skip)])
)).
