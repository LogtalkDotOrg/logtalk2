
:- initialization((
	use_module(library(clpfd)),
	use_module(library(lists)),
	logtalk_load(library(types_loader)),
	logtalk_load(library(metapredicates_loader)),
	logtalk_load([hexagon, queens, puzzle, sudoku]))).
