
main(Args) :-
	current_prolog_flag(version, Version),
	(	process_symbol(_), \+ member('dumb', Args)
		->
			global_state_set('$gui_state', gui),
			start_thread_gui
		;
			global_state_set('$gui_state', dumb)
	),
	write_term_list([wa('Qu-Prolog '), w(Version), nl]),
	interpreter.
