
main(Args) :-
	current_prolog_flag(version, Version),
	current_logtalk_flag(version, version(Major, Minor, Patch)),
	(	process_symbol(_), \+ member('dumb', Args)
		->
			global_state_set('$gui_state', gui),
			start_thread_gui
		;
			global_state_set('$gui_state', dumb)
	),
	write_term_list([
		wa('Logtalk '), w(Major), wa('.'), w(Minor), wa('.'), w(Patch),
		wa(' ++ Qu-Prolog '), w(Version), nl]),
	interpreter.
