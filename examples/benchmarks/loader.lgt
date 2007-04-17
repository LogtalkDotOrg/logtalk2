
% uncomment the next line if your Prolog compiler supports modules
%:- ensure_loaded(module).

:- initialization((
	logtalk_load([object, database], [events(off), reload(skip)]),
	logtalk_load([plain, benchmarks], [events(off)]))).
