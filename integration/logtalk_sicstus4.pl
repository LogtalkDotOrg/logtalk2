
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.32.1
%
%  Copyright (c) 1998-2008 Paulo Moura. All Rights Reserved.
%  
%  Logtalk is free software. You can redistribute it and/or modify
%  it under the terms of the Artistic License 2.0 as published by 
%  the The Perl Foundation.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%  Artistic License 2.0 for more details. A copy of the license is 
%  provided in the "LICENSE.txt" file.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- compile('$LOGTALKUSER/configs/sicstus4.config').
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
