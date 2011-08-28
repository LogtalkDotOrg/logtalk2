
:- initialization((
	logtalk_load([library(types_loader), library(metapredicates_loader)], [reload(skip)]),	% allow for static binding
	logtalk_load([predicates, closures, metapredicates, fibonacci, company, wrappers]))).
