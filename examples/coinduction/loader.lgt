
:- initialization((
	logtalk_load(coinduction, [reload(skip)]),
	logtalk_load(coinduction_with_constraints, [reload(skip)]),
	logtalk_load(simple, [hook(coinduction)]),
	logtalk_load(binary, [hook(coinduction)]),
	logtalk_load(streams, [hook(coinduction)]),
	logtalk_load(lists, [hook(coinduction)]),
	logtalk_load(automata, [hook(coinduction)]),
	logtalk_load(counter, [hook(coinduction)]),
	logtalk_load(nested, [hook(coinduction)]),
	logtalk_load(pta, [hook(coinduction_with_constraints)]),
	logtalk_load(train, [hook(coinduction_with_constraints)])
)).
