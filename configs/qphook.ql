%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Object oriented extension to Prolog
%  Release 2.16.1
%
%  integration code for Qu-Prolog 6.4 and later versions
%  used when generating a new interpreter that embeds Logtalk
%
%  last updated: February 27, 2004
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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
