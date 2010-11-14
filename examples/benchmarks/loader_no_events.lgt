
:- initialization((
	logtalk_load([category, objects, database, plain, maze, graph, benchmarks], [events(deny)])
)).

:- if(current_logtalk_flag(modules, supported)).
	:- ensure_loaded(module).
:- endif.
