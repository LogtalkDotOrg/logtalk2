
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


:- import stat_set_flag/2 from machine.	% workaround for compiling/loading source files
:- stat_set_flag(79, 1).				% when more than one thread is active

:- compiler_options([xpp_on]).

#include ../compiler/logtalk.pl

% tables of defined events and monitors
:- thread_shared('$lgt_before_'(_, _, _, _, _)).
:- thread_shared('$lgt_after_'(_, _, _, _, _)).

% tables of loaded entities and respective relationships
:- thread_shared('$lgt_current_protocol_'(_, _, _, _, _)).
:- thread_shared('$lgt_current_category_'(_, _, _, _, _, _, _)).
:- thread_shared('$lgt_current_object_'(_, _, _, _, _, _, _, _, _, _, _, _, _)).

:- thread_shared('$lgt_implements_protocol_'(_, _, _)).
:- thread_shared('$lgt_imports_category_'(_, _, _)).
:- thread_shared('$lgt_instantiates_class_'(_, _, _)).
:- thread_shared('$lgt_specializes_class_'(_, _, _)).
:- thread_shared('$lgt_extends_protocol_'(_, _, _)).
:- thread_shared('$lgt_extends_object_'(_, _, _)).

% table of loaded files
:- thread_shared('$lgt_loaded_file_'(_, _)).

% debugger status and tables
:- thread_shared('$lgt_debugging_'(_)).

:- thread_shared('$lgt_dbg_debugging_').
:- thread_shared('$lgt_dbg_tracing_').
:- thread_shared('$lgt_dbg_skipping_').
:- thread_shared('$lgt_dbg_spying_'(_, _)).
:- thread_shared('$lgt_dbg_spying_'(_, _, _, _)).
:- thread_shared('$lgt_dbg_leashing_'(_)).

% runtime flags
:- thread_shared('$lgt_current_flag_'(_, _)).

% static binding caches
:- thread_shared('$lgt_static_binding_entity_'(_)).
:- thread_shared('$lgt_obj_static_binding_cache_'(_, _, _, _)).
:- thread_shared('$lgt_ctg_static_binding_cache_'(_, _, _, _, _, _)).

% lookup caches for messages to an object, messages to self, and super calls
:- thread_shared('$lgt_obj_lookup_cache_'(_, _, _, _)).
:- thread_shared('$lgt_self_lookup_cache_'(_, _, _, _)).
:- thread_shared('$lgt_super_lookup_cache_'(_, _, _, _, _)).

% lookup cache for asserting and retracting dynamic facts
:- thread_shared('$lgt_db_lookup_cache_'(_, _, _, _, _)).

% table of library paths
:- thread_shared(logtalk_library_path(_, _)).

% compiler hook term and goal expansion:
:- thread_shared('$lgt_hook_term_expansion_'(_, _)).
:- thread_shared('$lgt_hook_goal_expansion_'(_, _)).

% multi-threading tags
:- thread_shared('$lgt_threaded_tag_counter'(_)).
