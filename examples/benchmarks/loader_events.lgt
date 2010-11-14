
:- initialization((
	% compile messages with event support in order to measure the implicit overhead
	logtalk_load([category, objects, database, plain, maze, graph, benchmarks], [events(allow)])
)).

:- if(current_logtalk_flag(modules, supported)).
	:- ensure_loaded(module).
:- endif.
