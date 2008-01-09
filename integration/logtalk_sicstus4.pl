
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.31.2
%
%  Copyright (c) 1998-2008 Paulo Moura.  All Rights Reserved.
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
