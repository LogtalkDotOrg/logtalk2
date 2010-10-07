
:- initialization((
	logtalk_load(library(types_loader)),
	logtalk_load([problog, hook], [reload(skip)]),		% allow for static binding
	logtalk_load([
		graph,
%		graph_tabled,
		office,
		learn_graph,
		viralmarketing
%		viralmarketing_tabled
	], [hook(hook)])
)).
