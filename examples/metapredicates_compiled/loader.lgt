
:- initialization((
	logtalk_load(library(meta_compiler_loader)),
	set_logtalk_flag(hook, meta_compiler),
	logtalk_load([
		metapredicates(predicates),
		metapredicates(closures),
		metapredicates(metapredicates),
		metapredicates(fibonacci),
		metapredicates(company),
		metapredicates(wrappers)
	])
)).
