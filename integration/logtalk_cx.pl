
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.35.2
%  
%  Copyright (c) 1998-2009 Paulo Moura.        All Rights Reserved.
%  Logtalk is free software.  You can redistribute it and/or modify
%  it under the terms of the "Artistic License 2.0" as published by 
%  The Perl Foundation. Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- set_prolog_flag(file_name_variables, true).
:- silent_consult('$LOGTALKUSER/configs/cx.pl').
:- (	fs_exists_file('settings.pl') ->
		silent_consult('settings.pl')
	;	fs_exists_file('$LOGTALKUSER/settings.pl') ->
		silent_consult('$LOGTALKUSER/settings.pl')
	;	true
	).
:- silent_consult('$LOGTALKHOME/compiler/logtalk.pl').
:- silent_consult('$LOGTALKUSER/libpaths/libpaths.pl').
