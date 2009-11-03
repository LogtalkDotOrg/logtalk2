
:- initialization((
	ensure_loaded(library(clpfd)),
	ensure_loaded(library(lists)),
	logtalk_load(library(types_loader)),
	logtalk_load(library(metapredicates_loader)),
	logtalk_load([hexagon, queens, puzzle, sudoku, oneground]))).


:- if((current_prolog_flag(version_data, swi(Major, Minor, _, _)), Major >= 5, Minor >= 8)).

	:- initialization(logtalk_load(knight)).

:- endif.
