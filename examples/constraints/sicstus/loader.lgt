
:- ensure_loaded(library(clpfd)).
:- ensure_loaded(library(lists)).

:- initialization((
	logtalk_load(sicstus_clpfd_hook, [reload(skip)]),
	logtalk_load([cars_ix, smm, torpedo, squares], [hook(sicstus_clpfd_hook)])
)).
