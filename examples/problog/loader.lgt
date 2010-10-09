
:- initialization((
	logtalk_load(library(types_loader)),
	logtalk_load([problog, hook], [reload(skip)]),		% allow for static binding
	logtalk_load([
		graph,
		office,
		learn_graph,
		viralmarketing,
		graph_tabled,
		viralmarketing_tabled
	], [hook(hook), misspelt(silent)])
)).
