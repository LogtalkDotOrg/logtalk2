
:- initialization((
	logtalk_load([category], [events(deny), reload(skip)]),
	logtalk_load([objects, database], [events(deny), reload(skip)]),
	logtalk_load([plain, benchmarks], [events(deny)]))).
