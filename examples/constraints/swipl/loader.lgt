
:- if(current_prolog_flag(iso, true)).
	:- set_prolog_flag(iso, false).
	:- ensure_loaded(library(clpfd)).
	:- set_prolog_flag(iso, true).
:- else.
	:- ensure_loaded(library(clpfd)).
:- endif.

:- initialization((
	logtalk_load(library(types_loader)),
	logtalk_load(library(metapredicates_loader)),
	logtalk_load([hexagon, queens, puzzle, sudoku, oneground])
)).

:- if((current_prolog_flag(version_data, swi(Major, Minor, _, _)), Major >= 5, Minor >= 8)).
	:- initialization(logtalk_load(knight)).
:- endif.
