
:- ensure_loaded(library(clpfd)).

:- initialization((
	logtalk_load(library(types_loader)),
	logtalk_load(library(metapredicates_loader)),
	logtalk_load([hexagon, queens, puzzle, sudoku, oneground])
)).
