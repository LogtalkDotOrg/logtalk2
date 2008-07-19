
:- initialization((
	logtalk_load(library(types_loader), [reload(skip)]),
	logtalk_load(library(metapredicates_loader), [reload(skip)]),
	logtalk_load(primes(loader)),
	logtalk_load(sorting(loader)),
	logtalk_load(fibonacci(loader)),
	logtalk_load(hanoi(loader)),
	logtalk_load(tak(loader)),
	logtalk_load(fft(loader)),
	logtalk_load(integration(loader)),
	logtalk_load(integration2d(loader)),
	logtalk_load(roots(loader), [reload(skip)]),
	logtalk_load([
		searching(state_space),
		searching(heuristic_state_space),
		searching(salt3),
		searching(search_strategy),
		searching(blind_search1),
		searching(breadth_first1),
		searching(depth_first1),
		searching(heuristic_search1),
		searching(best_first1),
		searching(hill_climbing1)], [reload(skip)]),
	logtalk_load(mtbatch, [misspelt(silent)])
	)). 
