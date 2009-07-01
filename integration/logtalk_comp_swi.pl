
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.37.3
%  
%  Copyright (c) 1998-2009 Paulo Moura.        All Rights Reserved.
%  Logtalk is free software.  You can redistribute it and/or modify
%  it under the terms of the "Artistic License 2.0" as published by 
%  The Perl Foundation. Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- set_prolog_flag(generate_debug_info, false).
:- system_module.
:- op(600, xfy, ::).
:- op(600,  fy, ::).
:- op(600,  fy, ^^).
:- op(200,  fy,  +).
:- op(200,  fy,  ?).
:- op(200,  fy,  @).
:- op(200,  fy,  -).
:- op(400, yfx, <<).
:- op(600,  fy,  :).
:- include('../compiler/logtalk.pl').
