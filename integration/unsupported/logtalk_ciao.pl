
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.44.1
%  
%  Copyright (c) 1998-2012 Paulo Moura.        All Rights Reserved.
%  Logtalk is free software.  You can redistribute it and/or modify
%  it under the terms of the "Artistic License 2.0" as published by 
%  The Perl Foundation. Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- ensure_loaded('$LOGTALKHOME/configs/ciao.pl').
:- ensure_loaded('$LOGTALKUSER/libpaths/libpaths.pl').
:- set_prolog_flag(multi_arity_warnings, off).
:- ensure_loaded('$LOGTALKHOME/compiler/logtalk.pl').
:- op(600, xfy, ::).
:- op(600,  fy, ::).
:- op(600,  fy, ^^).
:- op(200,  fy,  +).
:- op(200,  fy,  ?).
:- op(200,  fy,  @).
:- op(200,  fy,  -).
:- op(400, yfx, <<).
:- op(600,  fy,  :).
:- op(400, yfx, >>).
