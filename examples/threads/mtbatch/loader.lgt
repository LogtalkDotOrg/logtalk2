
:- initialization((
	logtalk_load(library(types_loader), [reload(skip)]),
	logtalk_load(library(metapredicates_loader), [reload(skip)]),
	logtalk_load(primes(loader)),
	logtalk_load(sorting(loader)),
	logtalk_load(fibonacci(loader)),
	logtalk_load(hanoi(loader)),
	logtalk_load(tak(loader)),
	logtalk_load(mtbatch, [misspelt(silent)])
	)). 
