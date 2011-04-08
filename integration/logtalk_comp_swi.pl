
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.43.0
%  
%  Copyright (c) 1998-2011 Paulo Moura.        All Rights Reserved.
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
:- op(400, yfx, >>).

:- '$hide'((::)/2).
:- '$hide'((<<)/2).

:- noprofile((
	'$lgt_before_event_'/5, '$lgt_after_event_'/5,
	'$lgt_current_protocol_'/5, '$lgt_current_category_'/6, '$lgt_current_object_'/11,
	'$lgt_entity_property_'/2,
	'$lgt_implements_protocol_'/3, '$lgt_imports_category_'/3, '$lgt_instantiates_class_'/3, 
	'$lgt_specializes_class_'/3, '$lgt_extends_category_'/3, '$lgt_extends_object_'/3,
	'$lgt_extends_protocol_'/3, '$lgt_complemented_object_'/5,
	'$lgt_loaded_file_'/2,
	'$lgt_compiler_flag'/2, '$lgt_default_flag'/2, '$lgt_current_flag_'/2, '$lgt_pp_compiler_flag_'/2,
	'$lgt_prolog_feature'/2,
	'$lgt_debugger.debugging_'/0,
	'$lgt_exec_ctx'/6, '$lgt_pred_meta_vars'/3,
	'$lgt_send_to_self_nv'/3,
	'$lgt_send_to_self'/3, '$lgt_send_to_self_'/3,
	'$lgt_send_to_obj'/3, '$lgt_send_to_obj_'/3,
	'$lgt_send_to_obj_nv'/3,
	'$lgt_send_to_obj_ne_nv'/3,
	'$lgt_send_to_obj_ne'/3, '$lgt_send_to_obj_ne_'/3,
	'$lgt_obj_super_call_same'/3, '$lgt_obj_super_call_same_'/3,
	'$lgt_obj_super_call_other'/3, '$lgt_obj_super_call_other_'/3,
	'$lgt_ctg_super_call_same'/3, '$lgt_ctg_super_call_same_'/3,
	'$lgt_ctg_super_call_other'/3, '$lgt_ctg_super_call_other_'/3,
	'$lgt_ctg_call'/3, '$lgt_ctg_call_'/3,
	'$lgt_db_lookup_cache_'/7,
	'$lgt_hook_term_expansion_'/2, '$lgt_hook_goal_expansion_'/2,
	'$lgt_threaded_tag_counter_'/1,
	'$lgt_metacall'/5, '$lgt_metacall'/6
)).

% the following index/1 directives may or may not improve performance
% depending on your application; you can comment out them if necessary
:- index('$lgt_send_to_self_'(1, 1, 0)).
:- index('$lgt_send_to_obj_'(1, 1, 0)).
:- index('$lgt_send_to_obj_ne_'(1, 1, 0)).
:- index('$lgt_obj_super_call_same_'(1, 1, 0)).
:- index('$lgt_obj_super_call_other_'(1, 1, 0)).
:- index('$lgt_ctg_super_call_same_'(1, 1, 0)).
:- index('$lgt_ctg_super_call_other_'(1, 1, 0)).
:- index('$lgt_ctg_call_'(1, 1, 0)).
:- index('$lgt_db_lookup_cache_'(1, 1, 0, 0, 0, 0, 0)).

:- include('../compiler/logtalk.pl').
