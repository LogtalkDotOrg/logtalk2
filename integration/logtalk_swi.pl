
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.32.0
%
%  Copyright (c) 1998-2008 Paulo Moura.  All Rights Reserved.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- consult('$LOGTALKUSER/configs/swi.config').
:- consult('$LOGTALKHOME/integration/logtalk_comp_swi.pl').
:- consult('$LOGTALKUSER/libpaths/libpaths.pl').
:- consult('$LOGTALKUSER/configs/swihook.pl').
:- absolute_file_name(library(pce), _, [file_type(prolog), access(read), file_errors(fail)]) -> consult('$LOGTALKUSER/configs/xpcehook.pl'); true.
