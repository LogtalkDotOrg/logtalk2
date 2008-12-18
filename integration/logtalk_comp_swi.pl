
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.35.0
%  
%  Copyright (c) 1998-2008 Paulo Moura.        All Rights Reserved.
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
:- if(current_prolog_flag(threads, true)).
    :- thread_local('$lgt_obj_lookup_cache_'/4).
    :- thread_local('$lgt_self_lookup_cache_'/4).
    :- thread_local('$lgt_super_lookup_cache_'/5).
    :- thread_local('$lgt_super_lookup_cache_'/6).
    :- thread_local('$lgt_db_lookup_cache_'/5).
:- endif.
:- include('../compiler/logtalk.pl').
