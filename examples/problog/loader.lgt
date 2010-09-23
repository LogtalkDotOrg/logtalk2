
:- initialization((
	logtalk_load([problog, hook], [reload(skip)]),		% allow for static binding
	logtalk_load([graph, office, viralmarketing], [hook(hook)])
)).
