
:- initialization((
	ensure_loaded(library(clpfd)),
	ensure_loaded(library(lists)),
	logtalk_load(library(types_loader)),
	logtalk_load(library(metapredicates_loader)),
	logtalk_load([hexagon, queens, puzzle, sudoku]))).