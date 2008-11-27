
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.34.1
%  
%  Copyright (c) 1998-2008 Paulo Moura.        All Rights Reserved.
%  Logtalk is free software.  You can redistribute it and/or modify
%  it under the terms of the "Artistic License 2.0" as published by 
%  The Perl Foundation. Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- compile('$LOGTALKUSER/configs/sicstus4.pl').
:- asserta((
	user:goal_expansion(CallWitArgs, Layout, _, Call, Layout) :-
		CallWitArgs =.. [call_with_args| Args],
		Call =.. [call| Args])).
:- compile('$LOGTALKHOME/compiler/logtalk.pl').
:- retract((
	user:goal_expansion(CallWitArgs, Layout, _, Call, Layout) :-
		CallWitArgs =.. [call_with_args| Args],
		Call =.. [call| Args])).
:- compile('$LOGTALKUSER/libpaths/libpaths.pl').
