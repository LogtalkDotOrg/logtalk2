%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.39.3
%
%  Copyright (c) 1998-2010 Paulo Moura.        All Rights Reserved.
%  Logtalk is free software.  You can redistribute it and/or modify
%  it under the terms of the "Artistic License 2.0" as published by 
%  The Perl Foundation. Consult the "LICENSE.txt" file for details.
%
%
%  integration code for Qu-Prolog 7.1 and later versions
%  used when generating a new interpreter that embeds Logtalk
%
%  last updated: February 20, 2006
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


main(Args) :-
	current_prolog_flag(version, Version),
	(	process_symbol(_), member('gui', Args) ->
		global_state_set('$gui_state', gui),
		start_thread_gui
	;	global_state_set('$gui_state', dumb)
	),
	write_term_list([wa('Qu-Prolog '), w(Version), nl]),
	interpreter.
