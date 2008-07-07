
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


:- ensure_loaded('$LOGTALKUSER/configs/ciao_aux.config').
:- set_prolog_flag(multi_arity_warnings, off).
:- ensure_loaded('$LOGTALKHOME/compiler/logtalk.pl').
:- ensure_loaded('$LOGTALKUSER/libpaths/libpaths.pl').
:- op(600, xfy, ::).
:- op(600, fy, ::).
:- op(600, fy, ^^).
:- op(200, fy, +).
:- op(200, fy, ?).
:- op(200, fy, @).
:- op(200, fy, -).
