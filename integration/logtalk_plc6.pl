
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


:- consult('$LOGTALKUSER/configs/k6.pl').
:- (	dir('.', Files, _),
		'$lgt_member'(Name, Files),
		fname(Name, 'settings.pl') ->
		consult('settings.pl')
	;	fname("$LOGTALKUSER", Directory),
		dir(Directory, Files, _),
		'$lgt_member'(Name, Files),
		fname(Name, 'settings.pl') ->
		consult('$LOGTALKUSER/settings.pl')
	;	true
	).
:- consult('$LOGTALKHOME/compiler/logtalk.pl').
:- consult('$LOGTALKUSER/libpaths/libpaths.pl').
