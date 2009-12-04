
% uncomment the next line only if your Prolog compiler supports modules
%:- ensure_loaded(module).

:- initialization(
	logtalk_load(
		[category, objects, database, plain, benchmarks],
		[events(allow)])).	% compile messages with event support in order
							% to measure the implicit overhead
