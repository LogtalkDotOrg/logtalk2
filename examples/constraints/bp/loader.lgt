
:- initialization((
	logtalk_load([clique, magic, puzzle, steiner]),
	logtalk_load(action_rules_hook, [reload(skip)]),
	logtalk_load([queens3, srq], [hook(action_rules_hook)])
)).
