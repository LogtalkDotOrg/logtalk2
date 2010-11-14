
:- initialization((
	logtalk_load([category], [events(deny), reload(skip)]),
	logtalk_load([objects, database, maze, graph], [events(deny), reload(skip)]),
	logtalk_load([plain, benchmarks], [events(deny)])
)).

:- if(current_logtalk_flag(modules, supported)).
	:- ensure_loaded(module).
:- endif.
