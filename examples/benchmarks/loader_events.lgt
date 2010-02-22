
:- initialization(
	logtalk_load(
		[category, objects, database, plain, benchmarks],
		[events(allow)])).	% compile messages with event support in order
							% to measure the implicit overhead
