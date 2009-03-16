
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


:- set_prolog_flag(redefined, off).
:- (	file_exists('settings.pl') ->
		cl('settings.pl')
	;	expand_environment('$LOGTALKUSER/settings.pl', File), file_exists(File) ->
		cl('$LOGTALKUSER/settings.pl')
	;	true
	).
:- cl('$LOGTALKUSER/configs/b.pl').
:- cl('$LOGTALKHOME/compiler/logtalk.pl').
:- cl('$LOGTALKUSER/libpaths/libpaths.pl').
