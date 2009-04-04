
:- initialization((
	logtalk_load(library(all_loader), [reload(skip)]),	% allow for static binding
	logtalk_load(roots(loader), [reload(skip)]),		% allow for static binding
	logtalk_load([
		state_space,
		water_jug,
		farmer,
		heuristic_state_space,
		bridge,
		eight_puzzle,
		miss_cann,
		salt3,
		search_strategy,
		blind_search1,
		heuristic_search1,
		performance]),
	logtalk_load([			% the actual search methods are compiled with
		breadth_first1,		% the option events(allow) to allow the use of
		depth_first1,		% the "performance" monitor
		best_first1,
		hill_climbing1],
		[events(allow)]))).
