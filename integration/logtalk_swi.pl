
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.36.0
%  
%  Copyright (c) 1998-2009 Paulo Moura.        All Rights Reserved.
%  Logtalk is free software.  You can redistribute it and/or modify
%  it under the terms of the "Artistic License 2.0" as published by 
%  The Perl Foundation. Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- (	(	current_prolog_flag(windows, true) ->
			Base = 'pl.ini'
		;	Base = '.plrc'
		),
		(	absolute_file_name(user_profile(Base), [access(read), file_errors(fail)], InitFile) ->
			ensure_loaded(user:InitFile)
		;	true
		)
).

:- consult('$LOGTALKHOME/configs/swi.pl').
:- consult('$LOGTALKHOME/integration/logtalk_comp_swi.pl').
:- consult('$LOGTALKUSER/libpaths/libpaths.pl').
:- consult('$LOGTALKHOME/configs/swihook.pl').
:- (	absolute_file_name(library(pce), _, [file_type(prolog), access(read), file_errors(fail)]) ->
		consult('$LOGTALKHOME/configs/xpcehook.pl')
	;	true
	).
