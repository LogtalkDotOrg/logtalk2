
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




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  operator declarations
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% message sending operators

:- op(600, xfy, ::).	% send to object
:- op(600,  fy, ::).	% send to self

:- op(600,  fy, ^^).	% "super" call (calls an overriden, inherited method definition)


% mode operators

:- op(200, fy, +).		% input argument (instantiated)
:- op(200, fy, ?).		% input/output argument
:- op(200, fy, @).		% input argument (not modified by the call)
:- op(200, fy, -).		% output argument (not instantiated)


% bitwise left-shift operator (used for context-switching calls)

:- op(400, yfx, <<).	% some Prolog compilers don't declare this operator


% imported category predicate call operator

:- op(600,  fy,  :).


% bitwise right-shift operator (used for lambda expressions)

:- op(400, yfx, >>).	% some Prolog compilers don't declare this operator




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  runtime directives (bookkeeping tables)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% tables of defined events and monitors

:- dynamic('$lgt_before_event_'/5).					% '$lgt_before_event_'(Obj, Msg, Sender, Monitor, Call)
:- dynamic('$lgt_after_event_'/5).					% '$lgt_after_event_'(Obj, Msg, Sender, Monitor, Call)


% tables of loaded entities, entity properties, and entity relations

:- multifile('$lgt_current_protocol_'/5).			% '$lgt_current_protocol_'(Ptc, Prefix, Dcl, Rnm, Flags)
:- dynamic('$lgt_current_protocol_'/5).

:- multifile('$lgt_current_category_'/6).			% '$lgt_current_category_'(Ctg, Prefix, Dcl, Def, Rnm, Flags)
:- dynamic('$lgt_current_category_'/6).

:- multifile('$lgt_current_object_'/11).			% '$lgt_current_object_'(Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm, Flags)
:- dynamic('$lgt_current_object_'/11).

:- multifile('$lgt_entity_property_'/2).			% '$lgt_entity_property_'(Entity, Property)
:- dynamic('$lgt_entity_property_'/2).
	
:- multifile('$lgt_implements_protocol_'/3).		% '$lgt_implements_protocol_'(ObjOrCtg, Ptc, Scope)
:- dynamic('$lgt_implements_protocol_'/3).

:- multifile('$lgt_imports_category_'/3).			% '$lgt_imports_category_'(Obj, Ctg, Scope)
:- dynamic('$lgt_imports_category_'/3).

:- multifile('$lgt_instantiates_class_'/3).			% '$lgt_instantiates_class_'(Instance, Class, Scope)
:- dynamic('$lgt_instantiates_class_'/3).

:- multifile('$lgt_specializes_class_'/3).			% '$lgt_specializes_class_'(Class, Superclass, Scope)
:- dynamic('$lgt_specializes_class_'/3).

:- multifile('$lgt_extends_category_'/3).			% '$lgt_extends_category_'(Ctg1, Ctg2, Scope)
:- dynamic('$lgt_extends_category_'/3).

:- multifile('$lgt_extends_object_'/3).				% '$lgt_extends_object_'(Prototype, Parent, Scope)
:- dynamic('$lgt_extends_object_'/3).

:- multifile('$lgt_extends_protocol_'/3).			% '$lgt_extends_protocol_'(Ptc1, Ptc2, Scope)
:- dynamic('$lgt_extends_protocol_'/3).

:- multifile('$lgt_complemented_object_'/5).		% '$lgt_complemented_object_'(Obj, Ctg, Dcl, Def, Rnm)
:- dynamic('$lgt_complemented_object_'/5).


% table of loaded files

:- dynamic('$lgt_loaded_file_'/2).					% '$lgt_loaded_file_'(File, Directory)


% debugger status and tables

:- multifile('$lgt_debugging_entity_'/1).			% '$lgt_debugging_entity_'(Entity)
:- dynamic('$lgt_debugging_entity_'/1).

:- dynamic('$lgt_debugger.debugging_'/0).			% '$lgt_debugger.debugging_'
:- dynamic('$lgt_debugger.tracing_'/0).				% '$lgt_debugger.tracing_'
:- dynamic('$lgt_debugger.skipping_'/0).			% '$lgt_debugger.skipping_'
:- dynamic('$lgt_debugger.spying_'/2).				% '$lgt_debugger.spying_'(Functor, Arity)
:- dynamic('$lgt_debugger.spying_'/4).				% '$lgt_debugger.spying_'(Sender, This, Self, Goal)
:- dynamic('$lgt_debugger.leashing_'/1).			% '$lgt_debugger.leashing_'(Port)
:- dynamic('$lgt_debugger.invocation_number_'/1).	% '$lgt_debugger.invocation_number_'(N)


% runtime flags

:- dynamic('$lgt_current_flag_'/2).					% '$lgt_current_flag_'(Option, Value)


% static binding caches

:- multifile('$lgt_static_binding_entity_'/1).		% '$lgt_static_binding_entity_'(Entity)
:- dynamic('$lgt_static_binding_entity_'/1).

:- dynamic('$lgt_obj_static_binding_cache_'/4).		% '$lgt_obj_static_binding_cache_'(Obj, Pred, Sender, Call)
:- dynamic('$lgt_ctg_static_binding_cache_'/4).		% '$lgt_ctg_static_binding_cache_'(Ctg, Pred, ExCtx, Call)


% lookup caches for messages to an object, messages to self, and super calls

:- dynamic('$lgt_send_to_obj_'/3).					% '$lgt_send_to_obj_'(Obj, Pred, Sender)
:- dynamic('$lgt_send_to_obj_ne_'/3).				% '$lgt_send_to_obj_ne_'(Obj, Pred, Sender)
:- dynamic('$lgt_send_to_self_'/3).					% '$lgt_send_to_self_'(Obj, Pred, Sender)
:- dynamic('$lgt_obj_super_call_same_'/3).			% '$lgt_obj_super_call_same_'(Obj, Pred, ExCtx)
:- dynamic('$lgt_obj_super_call_other_'/3).			% '$lgt_obj_super_call_other_'(Obj, Pred, ExCtx)
:- dynamic('$lgt_ctg_super_call_same_'/3).			% '$lgt_ctg_super_call_same_'(Ctg, Pred, ExCtx)
:- dynamic('$lgt_ctg_super_call_other_'/3).			% '$lgt_ctg_super_call_other_'(Ctg, Pred, ExCtx)
:- dynamic('$lgt_ctg_call_'/3).						% '$lgt_ctg_call_'(Dcl, Pred, ExCtx)


% lookup cache for asserting and retracting dynamic facts

:- dynamic('$lgt_db_lookup_cache_'/5).				% '$lgt_db_lookup_cache_'(Obj, Fact, Sender, TFact, UClause)


% table of library paths

:- multifile(logtalk_library_path/2).				% logtalk_library_path(Library, Path)
:- dynamic(logtalk_library_path/2).


% term and goal expansion compiler hooks:

:- dynamic('$lgt_hook_term_expansion_'/2).			% '$lgt_hook_term_expansion_'(Term, ExpandedTerms)
:- dynamic('$lgt_hook_goal_expansion_'/2).			% '$lgt_hook_goal_expansion_'(Goal, ExpandedGoal)


% multi-threading tags

:- dynamic('$lgt_threaded_tag_counter_'/1).			% '$lgt_threaded_tag_counter_'(Tag)


% flag for recording loading of settings file

:- dynamic('$lgt_settings_file_loaded_'/1).			% '$lgt_settings_file_loaded_'(Path)




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  pre-processor directives
%
% (used for source file compilation and runtime creation of new entities)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- dynamic('$lgt_pp_file_compiler_flag_'/2).				% '$lgt_pp_file_compiler_flag_'(Option, Value)
:- dynamic('$lgt_pp_entity_compiler_flag_'/2).				% '$lgt_pp_entity_compiler_flag_'(Option, Value)

:- dynamic('$lgt_pp_dcl_'/1).								% '$lgt_pp_dcl_'(Clause)
:- dynamic('$lgt_pp_def_'/1).								% '$lgt_pp_def_'(Clause)
:- dynamic('$lgt_pp_final_def_'/1).							% '$lgt_pp_final_def_'(Clause)
:- dynamic('$lgt_pp_ddef_'/1).								% '$lgt_pp_ddef_'(Clause)
:- dynamic('$lgt_pp_final_ddef_'/1).						% '$lgt_pp_final_ddef_'(Clause)
:- dynamic('$lgt_pp_super_'/1).								% '$lgt_pp_super_'(Clause)

:- dynamic('$lgt_pp_synchronized_'/2).						% '$lgt_pp_synchronized_'(Pred, Mutex)
:- dynamic('$lgt_pp_predicate_mutex_counter_'/1).			% '$lgt_pp_predicate_mutex_counter_'(Count)
:- dynamic('$lgt_pp_dynamic_'/2).							% '$lgt_pp_dynamic_'(Functor, Arity)
:- dynamic('$lgt_pp_discontiguous_'/2).						% '$lgt_pp_discontiguous_'(Functor, Arity)
:- dynamic('$lgt_pp_mode_'/2).								% '$lgt_pp_mode_'(Mode, Determinism)
:- dynamic('$lgt_pp_public_'/2).							% '$lgt_pp_public_'(Functor, Arity)
:- dynamic('$lgt_pp_protected_'/2).							% '$lgt_pp_protected_'(Functor, Arity)
:- dynamic('$lgt_pp_private_'/2).							% '$lgt_pp_private_'(Functor, Arity)
:- dynamic('$lgt_pp_meta_predicate_'/1).					% '$lgt_pp_meta_predicate_'(Pred)
:- dynamic('$lgt_pp_value_annotation_'/4).					% '$lgt_pp_value_annotation_'(Annotation, Functor, Value, Goal)
:- dynamic('$lgt_pp_goal_annotation_'/4).					% '$lgt_pp_goal_annotation_'(Annotation, Functor, LeftGoal, RightGoal)
:- dynamic('$lgt_pp_predicate_alias_'/3).					% '$lgt_pp_predicate_alias_'(Entity, Pred, Alias)
:- dynamic('$lgt_pp_non_terminal_'/3).						% '$lgt_pp_non_terminal_'(Functor, Arity, ExtArity)
:- dynamic('$lgt_pp_multifile_'/2).							% '$lgt_pp_multifile_'(Functor, Arity)
:- dynamic('$lgt_pp_coinductive_'/2).						% '$lgt_pp_coinductive_'(Pred, CoinductivePred)
:- dynamic('$lgt_pp_coinductive_flag_'/1).					% '$lgt_pp_coinductive_flag_'(Pred)

:- dynamic('$lgt_pp_object_'/11).							% '$lgt_pp_object_'(Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm, Flags)
:- dynamic('$lgt_pp_category_'/6).							% '$lgt_pp_category_'(Ctg, Prefix, Dcl, Def, Rnm, Flags)
:- dynamic('$lgt_pp_protocol_'/5).							% '$lgt_pp_protocol_'(Ptc, Prefix, Dcl, Rnm, Flags)

:- dynamic('$lgt_pp_module_'/1).							% '$lgt_pp_module_'(Module)

:- dynamic('$lgt_pp_uses_'/1).								% '$lgt_pp_uses_'(Obj)
:- dynamic('$lgt_pp_uses_predicate_'/3).					% '$lgt_pp_uses_predicate_'(Obj, Predicate, Alias)
:- dynamic('$lgt_pp_uses_non_terminal_'/3).					% '$lgt_pp_uses_non_terminal_'(Obj, NonTerminal, Alias)
:- dynamic('$lgt_pp_use_module_predicate_'/3).				% '$lgt_pp_use_module_predicate_'(Module, Predicate, Alias)
:- dynamic('$lgt_pp_use_module_non_terminal_'/3).			% '$lgt_pp_use_module_non_terminal_'(Module, NonTerminal, Alias)
:- dynamic('$lgt_pp_calls_'/1).								% '$lgt_pp_calls_'(Entity)
:- dynamic('$lgt_pp_info_'/1).								% '$lgt_pp_info_'(List)
:- dynamic('$lgt_pp_info_'/2).								% '$lgt_pp_info_'(Functor/Arity, List) or '$lgt_pp_info_'(Functor//Args, List)

:- dynamic('$lgt_pp_implemented_protocol_'/4).				% '$lgt_pp_implemented_protocol_'(Ptc, Prefix, Dcl, Scope)
:- dynamic('$lgt_pp_imported_category_'/5).					% '$lgt_pp_imported_category_'(Ctg, Prefix, Dcl, Def, Scope)
:- dynamic('$lgt_pp_extended_object_'/10).					% '$lgt_pp_extended_object_'(Parent, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Scope)
:- dynamic('$lgt_pp_instantiated_class_'/10).				% '$lgt_pp_instantiated_class_'(Class, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Scope)
:- dynamic('$lgt_pp_specialized_class_'/10).				% '$lgt_pp_specialized_class_'(Superclass, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Scope)
:- dynamic('$lgt_pp_extended_protocol_'/4).					% '$lgt_pp_extended_protocol_'(Ptc, Prefix, Dcl, Scope)
:- dynamic('$lgt_pp_extended_category_'/5).					% '$lgt_pp_extended_category_'(Ctg, Prefix, Dcl, Def, Scope)
:- dynamic('$lgt_pp_complemented_object_'/1).				% '$lgt_pp_complemented_object_'(Obj)

:- dynamic('$lgt_pp_file_init_'/1).							% '$lgt_pp_file_init_'(Goal)	
:- dynamic('$lgt_pp_entity_init_'/3).						% '$lgt_pp_entity_init_'(Type, Entity, Goal)

:- dynamic('$lgt_pp_entity_init_'/1).						% '$lgt_pp_entity_init_'(Goal)
:- dynamic('$lgt_pp_final_entity_init_'/1).					% '$lgt_pp_final_entity_init_'(Goal)

:- dynamic('$lgt_pp_redefined_built_in_'/3).				% '$lgt_pp_redefined_built_in_'(Head, ExCtx, THead)

:- dynamic('$lgt_pp_directive_'/1).							% '$lgt_pp_directive_'(Dir)
:- dynamic('$lgt_pp_prolog_term_'/2).						% '$lgt_pp_prolog_term_'(Clause, Location)
:- dynamic('$lgt_pp_relation_clause_'/1).					% '$lgt_pp_relation_clause_'(Clause)
:- dynamic('$lgt_pp_entity_clause_'/2).						% '$lgt_pp_entity_clause_'(Clause, Location)
:- dynamic('$lgt_pp_final_entity_clause_'/2).				% '$lgt_pp_final_entity_clause_'(Clause, Location)
:- dynamic('$lgt_pp_entity_aux_clause_'/1).					% '$lgt_pp_entity_aux_clause_'(Clause)
:- dynamic('$lgt_pp_final_entity_aux_clause_'/1).			% '$lgt_pp_final_entity_aux_clause_'(Clause)

:- dynamic('$lgt_pp_defines_predicate_'/2).					% '$lgt_pp_defines_predicate_'(Functor, Arity)
:- dynamic('$lgt_pp_calls_predicate_'/4).					% '$lgt_pp_calls_predicate_'(Functor, Arity, TFunctor, TArity)
:- dynamic('$lgt_pp_non_portable_call_'/2).					% '$lgt_pp_non_portable_call_'(Functor, Arity)
:- dynamic('$lgt_pp_non_portable_function_'/2).				% '$lgt_pp_non_portable_function_'(Functor, Arity)
:- dynamic('$lgt_pp_missing_dynamic_directive_'/2).			% '$lgt_pp_missing_dynamic_directive_'(Functor, Arity)
:- dynamic('$lgt_pp_missing_discontiguous_directive_'/2).	% '$lgt_pp_missing_discontiguous_directive_'(Functor, Arity)
:- dynamic('$lgt_pp_previous_predicate_'/2).				% '$lgt_pp_previous_predicate_'(Functor, Arity)

:- dynamic('$lgt_pp_defines_non_terminal_'/2).				% '$lgt_pp_defines_non_terminal_'(Functor, Arity)
:- dynamic('$lgt_pp_calls_non_terminal_'/2).				% '$lgt_pp_calls_non_terminal_'(Functor, Arity)

:- dynamic('$lgt_pp_referenced_object_'/1).					% '$lgt_pp_referenced_object_'(Object)
:- dynamic('$lgt_pp_referenced_protocol_'/1).				% '$lgt_pp_referenced_protocol_'(Protocol)
:- dynamic('$lgt_pp_referenced_category_'/1).				% '$lgt_pp_referenced_category_'(Category)

:- dynamic('$lgt_pp_global_op_'/3).							% '$lgt_pp_global_op_'(Priority, Specifier, Operator)
:- dynamic('$lgt_pp_file_op_'/3).							% '$lgt_pp_file_op_'(Priority, Specifier, Operator)
:- dynamic('$lgt_pp_entity_op_'/4).							% '$lgt_pp_entity_op_'(Priority, Specifier, Operator, Scope)

:- dynamic('$lgt_pp_warnings_top_argument_'/1).				% '$lgt_pp_warnings_top_argument_'(Term)
:- dynamic('$lgt_pp_comp_warnings_counter_'/1).				% '$lgt_pp_comp_warnings_counter_'(Counter)
:- dynamic('$lgt_pp_load_warnings_counter_'/1).				% '$lgt_pp_load_warnings_counter_'(Counter)
:- dynamic('$lgt_pp_entity_warnings_flag_'/0).				% '$lgt_pp_entity_warnings_flag_'
:- dynamic('$lgt_pp_load_warnings_flag_'/0).				% '$lgt_pp_load_warnings_flag_'

:- dynamic('$lgt_pp_hook_term_expansion_'/2).				% '$lgt_pp_hook_term_expansion_'(Term, Terms)
:- dynamic('$lgt_pp_hook_goal_expansion_'/2).				% '$lgt_pp_hook_goal_expansion_'(Goal, EGoal)

:- dynamic('$lgt_pp_threaded_'/0).							% '$lgt_pp_threaded_'
:- dynamic('$lgt_pp_synchronized_'/0).						% '$lgt_pp_synchronized_'

:- dynamic('$lgt_pp_file_encoding_'/2).						% '$lgt_pp_file_encoding_'(LogtalkEncoding, PrologEncoding)
:- dynamic('$lgt_pp_file_bom_'/1).							% '$lgt_pp_file_bom_'(BOM)
:- dynamic('$lgt_pp_file_path_'/2).							% '$lgt_pp_file_path_'(File, Path)

:- dynamic('$lgt_pp_file_relation_clause_'/1).				% '$lgt_pp_file_relation_clause_'(Clause)

:- dynamic('$lgt_pp_cc_if_found_'/1).						% '$lgt_pp_cc_if_found_'(Goal)
:- dynamic('$lgt_pp_cc_skipping_'/0).						% '$lgt_pp_cc_skipping_'
:- dynamic('$lgt_pp_cc_mode_'/1).							% '$lgt_pp_cc_mode_'(Action)

:- dynamic('$lgt_pp_term_position_'/1).						% '$lgt_pp_term_position_'(Position)

:- dynamic('$lgt_pp_aux_predicate_counter_'/1).				% '$lgt_pp_aux_predicate_counter_'(Counter)




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  top-level predicates for message sending and context switching calls
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



Obj::Pred :-
	catch('$lgt_tr_msg'(Pred, Obj, Call, user), Error, '$lgt_runtime_error_handler'(error(Error, Obj::Pred, user))),
	(	'$lgt_debugger.debugging_', '$lgt_debugging_entity_'(Obj) ->
		'$lgt_debugger.reset_invocation_number',
		'$lgt_exec_ctx'(ExCtx, user, user, Obj, [], []),
		catch('$lgt_debugger.goal'(Obj::Pred, Call, ExCtx), Error, '$lgt_runtime_error_handler'(Error))
	;	catch(Call, Error, '$lgt_runtime_error_handler'(Error))
	).



Obj<<Goal :-
	catch('$lgt_tr_ctx_call'(Obj, Goal, Call, user), Error, '$lgt_runtime_error_handler'(error(Error, Obj<<Goal, user))),
	(	'$lgt_debugger.debugging_', '$lgt_debugging_entity_'(Obj) ->
		'$lgt_debugger.reset_invocation_number',
		'$lgt_exec_ctx'(ExCtx, user, user, Obj, [], []),
		catch('$lgt_debugger.goal'(Obj<<Goal, Call, ExCtx), Error, '$lgt_runtime_error_handler'(Error))
	;	catch(Call, Error, '$lgt_runtime_error_handler'(Error))
	).



% '$lgt_runtime_error_handler'(@term)
%
% top-level runtime error handler; an ugly mess due to the lack of Prolog standardization

'$lgt_runtime_error_handler'(Variable) :-
	var(Variable),
	throw(error(instantiation_error, throw(_))).

'$lgt_runtime_error_handler'(error(Variable)) :-
	var(Variable),
	throw(error(instantiation_error, throw(_))).

'$lgt_runtime_error_handler'(error(Variable, Sender)) :-
	var(Variable),
	throw(error(instantiation_error, throw(_), Sender)).

'$lgt_runtime_error_handler'(error(existence_error(goal_thread, '$lgt_send_to_obj_ne_nv'(Self, Goal, Sender)), _, _)) :-
	(	Self == user ->
		throw(error(existence_error(goal_thread, Goal), Sender))
	;	throw(error(existence_error(goal_thread, Self::Goal), Sender))
	).

'$lgt_runtime_error_handler'(error(existence_error(goal_thread, '$lgt_send_to_obj_nv'(Self, Goal, Sender)), _, _)) :-
	(	Self == user ->
		throw(error(existence_error(goal_thread, Goal), Sender))
	;	throw(error(existence_error(goal_thread, Self::Goal), Sender))
	).

'$lgt_runtime_error_handler'(error(existence_error(goal_thread, TGoal), _, Sender)) :-
	functor(TGoal, TFunctor, TArity),
	'$lgt_decompile_predicate_indicator'(TFunctor/TArity, _, _, Functor/Arity),
	functor(Goal, Functor, Arity),
	'$lgt_unify_head_thead_args'(Arity, Goal, TGoal),
	arg(TArity, TGoal, ExCtx),
	'$lgt_exec_ctx'(ExCtx, _, _, Self, _, _),
	(	Self == user ->
		throw(error(existence_error(goal_thread, Goal), Sender))
	;	throw(error(existence_error(goal_thread, Self::Goal), Sender))
	).

'$lgt_runtime_error_handler'(error(existence_error(procedure, TFunctor/6), _)) :-
	once((  atom_concat(Prefix, '_idcl', TFunctor)
		;   atom_concat(Prefix, '_dcl', TFunctor)
	)),
	'$lgt_reverse_entity_prefix'(Prefix, Obj),
	(	'$lgt_instantiates_class_'(_, Obj, _)
	;	'$lgt_specializes_class_'(_, Obj, _)
	;	'$lgt_extends_object_'(_, Obj, _)
	;	'$lgt_complemented_object_'(Obj, _, _, _, _)
	),
	\+ '$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, _),
	throw(error(existence_error(object, Obj), _, _)).

'$lgt_runtime_error_handler'(error(existence_error(procedure, TFunctor/5), _)) :-
	atom_concat(Prefix, '_dcl', TFunctor),
	'$lgt_reverse_entity_prefix'(Prefix, CtgOrPtc),
	(	'$lgt_implements_protocol_'(_, CtgOrPtc, _), \+ '$lgt_current_protocol_'(CtgOrPtc, _, _, _, _),
		throw(error(existence_error(protocol, CtgOrPtc), _, _))
	;	'$lgt_extends_protocol_'(_, CtgOrPtc, _), \+ '$lgt_current_protocol_'(CtgOrPtc, _, _, _, _),
		throw(error(existence_error(protocol, CtgOrPtc), _, _))
	;	'$lgt_imports_category_'(_, CtgOrPtc, _), \+ '$lgt_current_category_'(CtgOrPtc, _, _, _, _, _),
		throw(error(existence_error(category, CtgOrPtc), _, _))
	;	'$lgt_extends_category_'(_, CtgOrPtc, _), \+ '$lgt_current_category_'(CtgOrPtc, _, _, _, _, _),
		throw(error(existence_error(category, CtgOrPtc), _, _))
	).

'$lgt_runtime_error_handler'(error(existence_error(procedure, TFunctor1/TArity1), context(':'(_, TFunctor2/TArity2), _))) :-
	!,
	'$lgt_runtime_error_handler'(error(existence_error(procedure, TFunctor1/TArity1), context(TFunctor2/TArity2, _))).

'$lgt_runtime_error_handler'(error(existence_error(procedure, TFunctor1/TArity1), context(TFunctor2/TArity2, _))) :-	% SWI-Prolog
	'$lgt_decompile_predicate_indicator'(TFunctor1/TArity1, Entity, Type, Functor1/Arity1),
	'$lgt_decompile_predicate_indicator'(TFunctor2/TArity2, Entity, Type, Functor2/Arity2),
	throw(error(existence_error(procedure, Functor1/Arity1), context(Type, Entity, Functor2/Arity2))).

'$lgt_runtime_error_handler'(error(existence_error(procedure, TFunctor1/TArity1), TFunctor2/TArity2)) :-				% GNU Prolog and B-Prolog
	'$lgt_decompile_predicate_indicator'(TFunctor1/TArity1, Entity, Type, Functor1/Arity1),
	'$lgt_decompile_predicate_indicator'(TFunctor2/TArity2, Entity, Type, Functor2/Arity2),
	throw(error(existence_error(procedure, Functor1/Arity1), context(Type, Entity, Functor2/Arity2))).

'$lgt_runtime_error_handler'(error(existence_error(procedure, ModTFunctor/TArity), _)) :-								% Ciao
	atom_concat('user:', TFunctor, ModTFunctor),
	'$lgt_decompile_predicate_indicator'(TFunctor/TArity, Entity, Type, Functor/Arity),
	throw(error(existence_error(procedure, Functor/Arity), context(Type, Entity, _))).

'$lgt_runtime_error_handler'(error(existence_error(procedure, TFunctor/TArity), _)) :-									% K-Prolog and YAP 5.1 or later
	'$lgt_decompile_predicate_indicator'(TFunctor/TArity, Entity, Type, Functor/Arity),
	throw(error(existence_error(procedure, Functor/Arity), context(Type, Entity, _))).

'$lgt_runtime_error_handler'(error(existence_error(procedure, ':'(_, TFunctor/TArity)), _)) :-							% SICStus Prolog 4.x
	'$lgt_decompile_predicate_indicator'(TFunctor/TArity, Entity, Type, Functor/Arity),
	throw(error(existence_error(procedure, Functor/Arity), context(Type, Entity, _))).

'$lgt_runtime_error_handler'(error(existence_error(_, _, procedure, ':'(_, TFunctor/TArity), _), _)) :-					% Quintus, SICStus Prolog 3.x
	'$lgt_decompile_predicate_indicator'(TFunctor/TArity, Entity, Type, Functor/Arity),
	throw(error(existence_error(procedure, Functor/Arity), context(Type, Entity, _))).

'$lgt_runtime_error_handler'(error(existence_error(procedure, ':'(_, TFunctor/TArity)), _, _)) :-						% XSB
	'$lgt_decompile_predicate_indicator'(TFunctor/TArity, Entity, Type, Functor/Arity),
	throw(error(existence_error(procedure, Functor/Arity), context(Type, Entity, _))).

'$lgt_runtime_error_handler'(error(Variable, _, Sender)) :-
	var(Variable),
	throw(error(instantiation_error, throw(_), Sender)).

'$lgt_runtime_error_handler'(error(Error, Object::Goal, user)) :-
	Object == user,
	throw(error(Error, Goal)).

'$lgt_runtime_error_handler'(error(Error, Context)) :-																	% SWI-Prolog
	nonvar(Context),
	Context = context(TFunctor/TArity, _),
	'$lgt_decompile_predicate_indicator'(TFunctor/TArity, Entity, Type, Functor/Arity),
	throw(error(Error, context(Type, Entity, Functor/Arity))).

'$lgt_runtime_error_handler'(logtalk_debugger_aborted) :-
	!,
	write('Debugging session aborted by user. Debugger still on.'), nl,
	abort.

'$lgt_runtime_error_handler'(Error) :-
	throw(Error).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  built-in predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% current_object(?object_identifier)

current_object(Obj) :-
	'$lgt_must_be'(var_or_object_identifier, Obj, current_object(Obj)),
	'$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, _).



% current_protocol(?protocol_identifier)

current_protocol(Ptc) :-
	'$lgt_must_be'(var_or_protocol_identifier, Ptc, current_protocol(Ptc)),
	'$lgt_current_protocol_'(Ptc, _, _, _, _).



% current_category(?category_identifier)

current_category(Ctg) :-
	'$lgt_must_be'(var_or_category_identifier, Ctg, current_category(Ctg)),
	'$lgt_current_category_'(Ctg, _, _, _, _, _).



% object_property(?object_identifier, ?object_property)

object_property(Obj, Prop) :-
	nonvar(Obj),
	\+ callable(Obj),
	throw(error(type_error(object_identifier, Obj), object_property(Obj, Prop))).

object_property(Obj, Prop) :-
	nonvar(Prop),
	\+ '$lgt_valid_object_property'(Prop),
	throw(error(domain_error(object_property, Prop), object_property(Obj, Prop))).

object_property(Obj, Prop) :-
	'$lgt_current_object_'(Obj, _, Dcl, _, _, _, _, DDcl, _, _, Flags),
	(	'$lgt_object_flags'(Prop, Flags)
	;	'$lgt_entity_property_'(Obj, Internal),
		(	Internal = file(Base, Start, End, Path) ->
			(	Prop = file(Base, Path)
			;	Start =\= -1, End =\= -1, Prop = lines(Start, End)
			)
		;	Prop = Internal
		)
	;	(	Flags /\ 64 =:= 64 ->
			findall(
				Functor/Arity,
				((call(Dcl, Predicate, p(p(p)), _, _); call(DDcl, Predicate, p(p(p)))),
				 functor(Predicate, Functor, Arity)),
				Predicates)
		;	findall(
				Functor/Arity,
				(call(Dcl, Predicate, p(p(p)), _, _),
				 functor(Predicate, Functor, Arity)),
				Predicates)
		),
		Prop = public(Predicates)
	).



% category_property(?category_identifier, ?category_property)

category_property(Ctg, Prop) :-
	nonvar(Ctg),
	\+ callable(Ctg),
	throw(error(type_error(category_identifier, Ctg), category_property(Ctg, Prop))).

category_property(Ctg, Prop) :-
	nonvar(Prop),
	\+ '$lgt_valid_category_property'(Prop),
	throw(error(domain_error(category_property, Prop), category_property(Ctg, Prop))).

category_property(Ctg, Prop) :-
	'$lgt_current_category_'(Ctg, _, Dcl, _, _, Flags),
	(	'$lgt_category_flags'(Prop, Flags)
	;	'$lgt_entity_property_'(Ctg, Internal),
		(	Internal = file(Base, Start, End, Path) ->
			(	Prop = file(Base, Path)
			;	Start =\= -1, End =\= -1, Prop = lines(Start, End)
			)
		;	Prop = Internal
		)
	;	findall(
			Functor/Arity,
			(call(Dcl, Predicate, p(p(p)), _, _, Ctg), functor(Predicate, Functor, Arity)),
			Predicates),
		Prop = public(Predicates)
	).



% protocol_property(?protocol_identifier, ?protocol_property)

protocol_property(Ptc, Prop) :-
	nonvar(Ptc),
	\+ atom(Ptc),
	throw(error(type_error(protocol_identifier, Ptc), protocol_property(Ptc, Prop))).

protocol_property(Ptc, Prop) :-
	nonvar(Prop),
	\+ '$lgt_valid_protocol_property'(Prop),
	throw(error(domain_error(protocol_property, Prop), protocol_property(Ptc, Prop))).

protocol_property(Ptc, Prop) :-
	'$lgt_current_protocol_'(Ptc, _, Dcl, _, Flags),
	(	'$lgt_protocol_flags'(Prop, Flags)
	;	'$lgt_entity_property_'(Ptc, Internal),
		(	Internal = file(Base, Start, End, Path) ->
			(	Prop = file(Base, Path)
			;	Start =\= -1, End =\= -1, Prop = lines(Start, End)
			)
		;	Prop = Internal
		)
	;	findall(
			Functor/Arity,
			(call(Dcl, Predicate, p(p(p)), _, _, Ptc), functor(Predicate, Functor, Arity)),
			Predicates),
		Prop = public(Predicates)
	).



% create_object(?object_identifier, +list, +list, +list)

create_object(Obj, Rels, Dirs, Clauses) :-
	nonvar(Obj),
	(	\+ callable(Obj),
		throw(error(type_error(object_identifier, Obj), create_object(Obj, Rels, Dirs, Clauses)))
	;	'$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, _),
		throw(error(permission_error(modify, object, Obj), create_object(Obj, Rels, Dirs, Clauses)))
	;	'$lgt_current_category_'(Obj, _, _, _, _, _),
		throw(error(permission_error(modify, category, Obj), create_object(Obj, Rels, Dirs, Clauses)))
	;	'$lgt_current_protocol_'(Obj, _, _, _, _),
		throw(error(permission_error(modify, protocol, Obj), create_object(Obj, Rels, Dirs, Clauses)))
	).

create_object(Obj, Rels, Dirs, Clauses) :-
	'$lgt_must_be'(list, Rels, create_object(Obj, Rels, Dirs, Clauses)),
	'$lgt_must_be'(list, Dirs, create_object(Obj, Rels, Dirs, Clauses)),
	'$lgt_must_be'(list, Clauses, create_object(Obj, Rels, Dirs, Clauses)),
	catch(
		'$lgt_create_object'(Obj, Rels, Dirs, Clauses),
		Error,
		('$lgt_clean_pp_clauses', throw(error(Error, create_object(Obj, Rels, Dirs, Clauses))))).


'$lgt_create_object'(Obj, Rels, Dirs, Clauses) :-
	(	var(Obj) ->
		'$lgt_gen_entity_identifier'(0'o, Obj)
	;	true
	),
	'$lgt_clean_pp_clauses',
	'$lgt_tr_object_identifier'(Obj, (dynamic)),
	'$lgt_tr_object_relations'(Rels, Obj),
	'$lgt_comp_ctx_mode'(Ctx, runtime),	% set the initial compilation context for compiling
	'$lgt_tr_directives'(Dirs, Ctx),	% the object directives and clauses
	'$lgt_tr_terms'(Clauses, Ctx),		% the list of clauses may also include grammar rules
	'$lgt_gen_local_def_clauses',
	'$lgt_fix_synchronized_predicates',
	'$lgt_fix_predicate_calls',
	'$lgt_gen_object_clauses',
	'$lgt_gen_object_directives',
	'$lgt_assert_tr_entity',
	'$lgt_clean_pp_clauses'.



% create_category(?category_identifier, +list, +list, +list)

create_category(Ctg, Rels, Dirs, Clauses) :-
	nonvar(Ctg),
	(	\+ callable(Ctg),
		throw(error(type_error(category_identifier, Ctg), create_category(Ctg, Rels, Dirs, Clauses)))
	;	'$lgt_current_category_'(Ctg, _, _, _, _, _),
		throw(error(permission_error(modify, category, Ctg), create_category(Ctg, Rels, Dirs, Clauses)))
	;	'$lgt_current_object_'(Ctg, _, _, _, _, _, _, _, _, _, _),
		throw(error(permission_error(modify, object, Ctg), create_category(Ctg, Rels, Dirs, Clauses)))
	;	'$lgt_current_protocol_'(Ctg, _, _, _, _),
		throw(error(permission_error(modify, protocol, Ctg), create_category(Ctg, Rels, Dirs, Clauses)))
	).

create_category(Ctg, Rels, Dirs, Clauses) :-
	'$lgt_must_be'(list, Rels, create_category(Ctg, Rels, Dirs, Clauses)),
	'$lgt_must_be'(list, Dirs, create_category(Ctg, Rels, Dirs, Clauses)),
	'$lgt_must_be'(list, Clauses, create_category(Ctg, Rels, Dirs, Clauses)),
	catch(
		'$lgt_create_category'(Ctg, Rels, Dirs, Clauses),
		Error,
		('$lgt_clean_pp_clauses', throw(error(Error, create_category(Ctg, Rels, Dirs, Clauses))))).


'$lgt_create_category'(Ctg, Rels, Dirs, Clauses) :-
	(	var(Ctg) ->
		'$lgt_gen_entity_identifier'(0'c, Ctg)
	;	true
	),
	'$lgt_clean_pp_clauses',
	'$lgt_tr_category_identifier'(Ctg, (dynamic)),
	'$lgt_tr_category_relations'(Rels, Ctg),
	'$lgt_comp_ctx_mode'(Ctx, runtime),	% set the initial compilation context for compiling
	'$lgt_tr_directives'(Dirs, Ctx),	% the category directives and clauses
	'$lgt_tr_terms'(Clauses, Ctx),		% the list of clauses may also include grammar rules
	'$lgt_gen_local_def_clauses',
	'$lgt_fix_synchronized_predicates',
	'$lgt_fix_predicate_calls',
	'$lgt_gen_category_clauses',
	'$lgt_gen_category_directives',
	'$lgt_assert_tr_entity',
	'$lgt_clean_pp_clauses'.



% create_protocol(?protocol_identifier, +list, +list)

create_protocol(Ptc, Rels, Dirs) :-
	nonvar(Ptc),
	(	\+ atom(Ptc),
		throw(error(type_error(protocol_identifier, Ptc), create_protocol(Ptc, Rels, Dirs)))
	;	'$lgt_current_protocol_'(Ptc, _, _, _, _),
		throw(error(permission_error(modify, protocol, Ptc), create_protocol(Ptc, Rels, Dirs)))
	;	'$lgt_current_object_'(Ptc, _, _, _, _, _, _, _, _, _, _),
		throw(error(permission_error(modify, object, Ptc), create_protocol(Ptc, Rels, Dirs)))
	;	'$lgt_current_category_'(Ptc, _, _, _, _, _),
		throw(error(permission_error(modify, category, Ptc), create_protocol(Ptc, Rels, Dirs)))
	).

create_protocol(Ptc, Rels, Dirs) :-
	'$lgt_must_be'(list, Rels, create_protocol(Ptc, Rels, Dirs)),
	'$lgt_must_be'(list, Dirs, create_protocol(Ptc, Rels, Dirs)),
	catch(
		'$lgt_create_protocol'(Ptc, Rels, Dirs),
		Error,
		('$lgt_clean_pp_clauses', throw(error(Error, create_protocol(Ptc, Rels, Dirs))))).


'$lgt_create_protocol'(Ptc, Rels, Dirs) :-
	(	var(Ptc) ->
		'$lgt_gen_entity_identifier'(0'p, Ptc)
	;	true
	),
	'$lgt_clean_pp_clauses',
	'$lgt_tr_protocol_identifier'(Ptc, (dynamic)),
	'$lgt_tr_protocol_relations'(Rels, Ptc),
	'$lgt_comp_ctx_mode'(Ctx, runtime),	% set the initial compilation context
	'$lgt_tr_directives'(Dirs, Ctx),	% for compiling the protocol directives
	'$lgt_gen_protocol_clauses',
	'$lgt_gen_protocol_directives',
	'$lgt_assert_tr_entity',
	'$lgt_clean_pp_clauses'.



% '$lgt_gen_entity_identifier'(+char_code, -entity_identifier)
%
% generates a new, unique entity identifier by appending an integer to a base char

'$lgt_gen_entity_identifier'(Base, Id) :-
	repeat,
		'$lgt_next_integer'(1, New),
		number_codes(New, Codes),
		atom_codes(Id, [Base| Codes]),
	\+ '$lgt_current_protocol_'(Id, _, _, _, _),
	\+ '$lgt_current_object_'(Id, _, _, _, _, _, _, _, _, _, _),
	\+ '$lgt_current_category_'(Id, _, _, _, _, _),
	!.


'$lgt_next_integer'(I, I).
'$lgt_next_integer'(I, J) :-
	I2 is I + 1,
	'$lgt_next_integer'(I2, J).



% abolish_object(@object_identifier)

abolish_object(Obj) :-
	'$lgt_must_be'(object_identifier, Obj, abolish_object(Obj)),
	(	'$lgt_current_object_'(Obj, _, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm, Flags) ->
		(	Flags /\ 2 =:= 2 ->
			'$lgt_abolish_entity_predicates'(Def),
			'$lgt_abolish_entity_predicates'(DDef),
			abolish(Dcl/4),
			abolish(Dcl/6),
			abolish(Def/3),
			abolish(Def/4),
			abolish(Super/4),
			abolish(IDcl/6),
			abolish(IDef/4),
			abolish(DDcl/2),
			abolish(DDef/3),
			abolish(Rnm/3),
			retractall('$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, _)),
			retractall('$lgt_entity_property_'(Obj, _)),
			retractall('$lgt_extends_object_'(Obj, _, _)),
			retractall('$lgt_instantiates_class_'(Obj, _, _)),
			retractall('$lgt_specializes_class_'(Obj, _, _)),
			retractall('$lgt_implements_protocol_'(Obj, _, _)),
			retractall('$lgt_imports_category_'(Obj, _, _)),
			retractall('$lgt_debugging_entity_'(Obj)),
			'$lgt_clean_lookup_caches'
		;	throw(error(permission_error(modify, static_object, Obj), abolish_object(Obj)))
		)
	;	throw(error(existence_error(object, Obj), abolish_object(Obj)))
	).



% abolish_category(@category_identifier)

abolish_category(Ctg) :-
	'$lgt_must_be'(category_identifier, Ctg, abolish_category(Ctg)),
	(	'$lgt_current_category_'(Ctg, _, Dcl, Def, Rnm, Flags) ->
		(	Flags /\ 2 =:= 2 ->
			'$lgt_abolish_entity_predicates'(Def),
			abolish(Dcl/4),
			abolish(Dcl/5),
			abolish(Def/3),
			abolish(Rnm/3),
			retractall('$lgt_current_category_'(Ctg, _, _, _, _, _)),
			retractall('$lgt_entity_property_'(Ctg, _)),
			retractall('$lgt_extends_category_'(Ctg, _, _)),
			retractall('$lgt_implements_protocol_'(Ctg, _, _)),
			retractall('$lgt_debugging_entity_'(Ctg)),
			'$lgt_clean_lookup_caches'
		;	throw(error(permission_error(modify, static_category, Ctg), abolish_category(Ctg)))
		)
	;	throw(error(existence_error(category, Ctg), abolish_category(Ctg)))
	).



% abolish_protocol(@protocol_identifier)

abolish_protocol(Ptc) :-
	'$lgt_must_be'(protocol_identifier, Ptc, abolish_protocol(Ptc)),
	(	'$lgt_current_protocol_'(Ptc, _, Dcl, Rnm, Flags) ->
		(	Flags /\ 2 =:= 2 ->
			abolish(Dcl/4),
			abolish(Dcl/5),
			abolish(Rnm/3),
			retractall('$lgt_current_protocol_'(Ptc, _, _, _, _)),
			retractall('$lgt_entity_property_'(Ptc, _)),
			retractall('$lgt_extends_protocol_'(Ptc, _, _)),
			retractall('$lgt_debugging_entity_'(Ptc)),
			'$lgt_clean_lookup_caches'
		;	throw(error(permission_error(modify, static_protocol, Ptc), abolish_protocol(Ptc)))
		)
	;	throw(error(existence_error(protocol, Ptc), abolish_protocol(Ptc)))
	).



% '$lgt_abolish_entity_predicates'(+atom)

'$lgt_abolish_entity_predicates'(Def) :-
	call(Def, _, _, Pred),
		functor(Pred, Functor, Arity),
		abolish(Functor/Arity),
	fail.

'$lgt_abolish_entity_predicates'(_).



% implements_protocol(?object_identifier, ?protocol_identifier)
% implements_protocol(?category_identifier, ?protocol_identifier)

implements_protocol(ObjOrCtg, Ptc) :-
	catch(
		implements_protocol(ObjOrCtg, Ptc, _),
		error(Error, _),
		throw(error(Error, implements_protocol(ObjOrCtg, Ptc)))).



% implements_protocol(?object_identifier, ?protocol_identifier, ?atom)
% implements_protocol(?category_identifier, ?protocol_identifier, ?atom)

implements_protocol(ObjOrCtg, Ptc, Scope) :-
	'$lgt_must_be'(var_or_object_identifier, ObjOrCtg, implements_protocol(ObjOrCtg, Ptc, Scope)),
	'$lgt_must_be'(var_or_protocol_identifier, Ptc, implements_protocol(ObjOrCtg, Ptc, Scope)),
	'$lgt_must_be'(var_or_scope, Scope, implements_protocol(ObjOrCtg, Ptc, Scope)),
	'$lgt_implements_protocol_'(ObjOrCtg, Ptc, Scope).



% imports_category(?object_identifier, ?category_identifier)

imports_category(Obj, Ctg) :-
	catch(
		imports_category(Obj, Ctg, _),
		error(Error, _),
		throw(error(Error, imports_category(Obj, Ctg)))).



% imports_category(?object_identifier, ?category_identifier, ?atom)

imports_category(Obj, Ctg, Scope) :-
	'$lgt_must_be'(var_or_object_identifier, Obj, imports_category(Obj, Ctg, Scope)),
	'$lgt_must_be'(var_or_category_identifier, Ctg, imports_category(Obj, Ctg, Scope)),
	'$lgt_must_be'(var_or_scope, Scope, imports_category(Obj, Ctg, Scope)),
	'$lgt_imports_category_'(Obj, Ctg, Scope).



% instantiates_class(?object_identifier, ?object_identifier)

instantiates_class(Obj, Class) :-
	catch(
		instantiates_class(Obj, Class, _),
		error(Error, _),
		throw(error(Error, instantiates_class(Obj, Class)))).



% instantiates_class(?object_identifier, ?object_identifier, ?atom)

instantiates_class(Obj, Class, Scope) :-
	'$lgt_must_be'(var_or_object_identifier, Obj, instantiates_class(Obj, Class, Scope)),
	'$lgt_must_be'(var_or_object_identifier, Class, instantiates_class(Obj, Class, Scope)),
	'$lgt_must_be'(var_or_scope, Scope, instantiates_class(Obj, Class, Scope)),
	'$lgt_instantiates_class_'(Obj, Class, Scope).



% specializes_class(?object_identifier, ?object_identifier)

specializes_class(Class, Superclass) :-
	catch(
		specializes_class(Class, Superclass, _),
		error(Error, _),
		throw(error(Error, specializes_class(Class, Superclass)))).



% specializes_class(?object_identifier, ?object_identifier, ?atom)

specializes_class(Class, Superclass, Scope) :-
	'$lgt_must_be'(var_or_object_identifier, Class, specializes_class(Class, Superclass, Scope)),
	'$lgt_must_be'(var_or_object_identifier, Superclass, specializes_class(Class, Superclass, Scope)),
	'$lgt_must_be'(var_or_scope, Scope, specializes_class(Class, Superclass, Scope)),
	'$lgt_specializes_class_'(Class, Superclass, Scope).



% extends_category(?category_identifier, ?category_identifier)

extends_category(Ctg1, Ctg2) :-
	catch(
		extends_category(Ctg1, Ctg2, _),
		error(Error, _),
		throw(error(Error, extends_category(Ctg1, Ctg2)))).



% extends_category(?category_identifier, ?category_identifier, ?atom)

extends_category(Ctg1, Ctg2, Scope) :-
	'$lgt_must_be'(var_or_category_identifier, Ctg1, extends_category(Ctg1, Ctg2, Scope)),
	'$lgt_must_be'(var_or_category_identifier, Ctg2, extends_category(Ctg1, Ctg2, Scope)),
	'$lgt_must_be'(var_or_scope, Scope, extends_category(Ctg1, Ctg2, Scope)),
	'$lgt_extends_category_'(Ctg1, Ctg2, Scope).



% extends_protocol(?protocol_identifier, ?protocol_identifier)

extends_protocol(Ptc1, Ptc2) :-
	catch(
		extends_protocol(Ptc1, Ptc2, _),
		error(Error, _),
		throw(error(Error, extends_protocol(Ptc1, Ptc2)))).



% extends_protocol(?protocol_identifier, ?protocol_identifier, ?atom)

extends_protocol(Ptc1, Ptc2, Scope) :-
	'$lgt_must_be'(var_or_protocol_identifier, Ptc1, extends_protocol(Ptc1, Ptc2, Scope)),
	'$lgt_must_be'(var_or_protocol_identifier, Ptc2, extends_protocol(Ptc1, Ptc2, Scope)),
	'$lgt_must_be'(var_or_scope, Scope, extends_protocol(Ptc1, Ptc2, Scope)),
	'$lgt_extends_protocol_'(Ptc1, Ptc2, Scope).



% extends_object(?object_identifier, ?object_identifier)

extends_object(Prototype, Parent) :-
	catch(
		extends_object(Prototype, Parent, _),
		error(Error, _),
		throw(error(Error, extends_object(Prototype, Parent)))).



% extends_object(?object_identifier, ?object_identifier, ?atom)

extends_object(Prototype, Parent, Scope) :-
	'$lgt_must_be'(var_or_object_identifier, Prototype, extends_object(Prototype, Parent, Scope)),
	'$lgt_must_be'(var_or_object_identifier, Parent, extends_object(Prototype, Parent, Scope)),
	'$lgt_must_be'(var_or_scope, Scope, extends_object(Prototype, Parent, Scope)),
	'$lgt_extends_object_'(Prototype, Parent, Scope).



% complements_object(?category_identifier, ?object_identifier)

complements_object(Category, Object) :-
	'$lgt_must_be'(var_or_category_identifier, Category, complements_object(Category, Object)),
	'$lgt_must_be'(var_or_object_identifier, Object, complements_object(Category, Object)),
	'$lgt_complemented_object_'(Object, Category, _, _, _).



% conforms_to_protocol(?object_identifier, ?protocol_identifier)
% conforms_to_protocol(?category_identifier, ?protocol_identifier)

conforms_to_protocol(ObjOrCtg, Protocol) :-
	catch(
		conforms_to_protocol(ObjOrCtg, Protocol, _),
		error(Error, _),
		throw(error(Error, conforms_to_protocol(ObjOrCtg, Protocol)))).



% conforms_to_protocol(?object_identifier, ?protocol_identifier, ?atom)
% conforms_to_protocol(?category_identifier, ?protocol_identifier, ?atom)

conforms_to_protocol(ObjOrCtg, Protocol, Scope) :-
	'$lgt_must_be'(var_or_object_identifier, ObjOrCtg, conforms_to_protocol(ObjOrCtg, Protocol, Scope)),
	'$lgt_must_be'(var_or_protocol_identifier, Protocol, conforms_to_protocol(ObjOrCtg, Protocol, Scope)),
	'$lgt_must_be'(var_or_scope, Scope, conforms_to_protocol(ObjOrCtg, Protocol, Scope)),
	'$lgt_conforms_to_protocol'(ObjOrCtg, Protocol, Scope).


'$lgt_conforms_to_protocol'(Object, Protocol, Scope) :-
	'$lgt_current_object_'(Object, _, _, _, _, _, _, _, _, _, _),
	(	\+ '$lgt_instantiates_class_'(Object, _, _),
		\+ '$lgt_specializes_class_'(Object, _, _) ->
		'$lgt_prototye_conforms_to_protocol'(Object, Protocol, Scope)
	;	'$lgt_instance_conforms_to_protocol'(Object, Protocol, Scope)
	).

'$lgt_conforms_to_protocol'(Category, Protocol, Scope) :-
	'$lgt_current_category_'(Category, _, _, _, _, _),
	'$lgt_category_conforms_to_protocol'(Category, Protocol, Scope).


'$lgt_prototye_conforms_to_protocol'(Prototype, Protocol, Scope) :-
	'$lgt_implements_protocol_'(Prototype, Protocol0, ImplementationScope),
	(	Protocol = Protocol0,
		Scope = ImplementationScope
	;	'$lgt_protocol_conforms_to_protocol'(Protocol0, Protocol, InheritedScope),
		'$lgt_filter_scope'(ImplementationScope, InheritedScope, Scope)
	).

'$lgt_prototye_conforms_to_protocol'(Prototype, Protocol, Scope) :-
	'$lgt_imports_category_'(Prototype, Category, ImportScope),
	'$lgt_category_conforms_to_protocol'(Category, Protocol, InheritedScope),
	'$lgt_filter_scope'(ImportScope, InheritedScope, Scope).

'$lgt_prototye_conforms_to_protocol'(Prototype, Protocol, Scope) :-
	'$lgt_extends_object_'(Prototype, Parent, ExtensionScope),
	'$lgt_prototye_conforms_to_protocol'(Parent, Protocol, InheritedScope),
	'$lgt_filter_scope'(ExtensionScope, InheritedScope, Scope).


'$lgt_instance_conforms_to_protocol'(Instance, Protocol, Scope) :-
	'$lgt_instantiates_class_'(Instance, Class, InstantiationScope),
	'$lgt_class_conforms_to_protocol'(Class, Protocol, InheritedScope),
	'$lgt_filter_scope'(InstantiationScope, InheritedScope, Scope).


'$lgt_class_conforms_to_protocol'(Class, Protocol, Scope) :-
	'$lgt_implements_protocol_'(Class, Protocol0, ImplementationScope),
	(	Protocol = Protocol0,
		Scope = ImplementationScope
	;	'$lgt_protocol_conforms_to_protocol'(Protocol0, Protocol, InheritedScope),
		'$lgt_filter_scope'(ImplementationScope, InheritedScope, Scope)
	).

'$lgt_class_conforms_to_protocol'(Class, Protocol, Scope) :-
	'$lgt_imports_category_'(Class, Category, ImportScope),
	'$lgt_category_conforms_to_protocol'(Category, Protocol, InheritedScope),
	'$lgt_filter_scope'(ImportScope, InheritedScope, Scope).

'$lgt_class_conforms_to_protocol'(Class, Protocol, Scope) :-
	'$lgt_specializes_class_'(Class, Superclass, SpecializationScope),
	'$lgt_class_conforms_to_protocol'(Superclass, Protocol, InheritedScope),
	'$lgt_filter_scope'(SpecializationScope, InheritedScope, Scope).


'$lgt_protocol_conforms_to_protocol'(Protocol0, Protocol, Scope) :-
	'$lgt_extends_protocol_'(Protocol0, Protocol1, ExtensionScope),
	(	Protocol = Protocol1,
		Scope = ExtensionScope
	;	'$lgt_protocol_conforms_to_protocol'(Protocol1, Protocol, InheritedScope),
		'$lgt_filter_scope'(ExtensionScope, InheritedScope, Scope)
	).


'$lgt_category_conforms_to_protocol'(Category, Protocol, Scope) :-
	'$lgt_implements_protocol_'(Category, Protocol0, ImplementationScope),
	(	Protocol = Protocol0,
		Scope = ImplementationScope
	;	'$lgt_protocol_conforms_to_protocol'(Protocol0, Protocol, InheritedScope),
		'$lgt_filter_scope'(ImplementationScope, InheritedScope, Scope)
	).

'$lgt_category_conforms_to_protocol'(Category, Protocol, Scope) :-
	'$lgt_extends_category_'(Category, ExtendedCategory, ExtensionScope),
	'$lgt_category_conforms_to_protocol'(ExtendedCategory, Protocol, InheritedScope),
	'$lgt_filter_scope'(ExtensionScope, InheritedScope, Scope).


'$lgt_filter_scope'((public), Scope, Scope).
'$lgt_filter_scope'(protected, Scope, protected) :-
	Scope \= private.



% current_event(?event, ?object_identifier, ?callable, ?object_identifier, ?object_identifier)

current_event(Event, Obj, Msg, Sender, Monitor) :-
	'$lgt_must_be'(var_or_event, Event, current_event(Event, Obj, Msg, Sender, Monitor)),
	'$lgt_must_be'(var_or_object_identifier, Obj, current_event(Event, Obj, Msg, Sender, Monitor)),
	'$lgt_must_be'(var_or_callable, Msg, current_event(Event, Obj, Msg, Sender, Monitor)),
	'$lgt_must_be'(var_or_object_identifier, Sender, current_event(Event, Obj, Msg, Sender, Monitor)),
	'$lgt_must_be'(var_or_object_identifier, Monitor, current_event(Event, Obj, Msg, Sender, Monitor)),
	(	'$lgt_before_event_'(Obj, Msg, Sender, Monitor, _)
	;	'$lgt_after_event_'(Obj, Msg, Sender, Monitor, _)
	).



%define_events(@event, @object_identifier, @callable, @object_identifier, +object_identifier)

define_events(Event, Obj, Msg, Sender, Monitor) :-
	'$lgt_must_be'(var_or_event, Event, define_events(Event, Obj, Msg, Sender, Monitor)),
	'$lgt_must_be'(var_or_object_identifier, Obj, define_events(Event, Obj, Msg, Sender, Monitor)),
	'$lgt_must_be'(var_or_callable, Msg, define_events(Event, Obj, Msg, Sender, Monitor)),
	'$lgt_must_be'(var_or_object_identifier, Sender, define_events(Event, Obj, Msg, Sender, Monitor)),
	'$lgt_must_be'(object_identifier, Monitor, define_events(Event, Obj, Msg, Sender, Monitor)),
	(	'$lgt_current_object_'(Monitor, _, _, Def, _, _, _, _, _, _, _) ->
		'$lgt_exec_ctx'(ExCtx, Monitor, Monitor, Monitor, [], _),
		(	var(Event) ->
			'$lgt_define_events'(before, Obj, Msg, Sender, Monitor, Def, ExCtx),
			'$lgt_define_events'(after, Obj, Msg, Sender, Monitor, Def, ExCtx)
		;	Event == before ->
			'$lgt_define_events'(before, Obj, Msg, Sender, Monitor, Def, ExCtx)
		;	'$lgt_define_events'(after, Obj, Msg, Sender, Monitor, Def, ExCtx)
		)
	;	throw(error(existence_error(object, Monitor), define_events(Event, Obj, Msg, Sender, Monitor)))
	).


'$lgt_define_events'(before, Obj, Msg, Sender, Monitor, Def, ExCtx) :-
	(	call(Def, before(Obj, Msg, Sender), ExCtx, Call, _) ->
		retractall('$lgt_before_event_'(Obj, Msg, Sender, Monitor, _)),
		assertz('$lgt_before_event_'(Obj, Msg, Sender, Monitor, Call))
	;	throw(error(existence_error(procedure, before/3), define_events(before, Obj, Msg, Sender, Monitor)))
	).

'$lgt_define_events'(after, Obj, Msg, Sender, Monitor, Def, ExCtx) :-
	(	call(Def, after(Obj, Msg, Sender), ExCtx, Call, _) ->
		retractall('$lgt_after_event_'(Obj, Msg, Sender, Monitor, _)),
		assertz('$lgt_after_event_'(Obj, Msg, Sender, Monitor, Call))
	;	throw(error(existence_error(procedure, after/3), define_events(after, Obj, Msg, Sender, Monitor)))
	).



% abolish_events(@event, @object_identifier, @callable, @object_identifier, @object_identifier)

abolish_events(Event, Obj, Msg, Sender, Monitor) :-
	'$lgt_must_be'(var_or_event, Event, abolish_events(Event, Obj, Msg, Sender, Monitor)),
	'$lgt_must_be'(var_or_object_identifier, Obj, abolish_events(Event, Obj, Msg, Sender, Monitor)),
	'$lgt_must_be'(var_or_callable, Msg, abolish_events(Event, Obj, Msg, Sender, Monitor)),
	'$lgt_must_be'(var_or_object_identifier, Sender, abolish_events(Event, Obj, Msg, Sender, Monitor)),
	'$lgt_must_be'(var_or_object_identifier, Monitor, abolish_events(Event, Obj, Msg, Sender, Monitor)),
	(	var(Event) ->
		retractall('$lgt_before_event_'(Obj, Msg, Sender, Monitor, _)),
		retractall('$lgt_after_event_'(Obj, Msg, Sender, Monitor, _))
	;	Event == before ->
		retractall('$lgt_before_event_'(Obj, Msg, Sender, Monitor, _))
	;	retractall('$lgt_after_event_'(Obj, Msg, Sender, Monitor, _))
	).



% built-in multi-threading meta-predicates


% threaded(+callable)

threaded(Goals) :-
	\+ '$lgt_compiler_flag'(threads, supported),
	throw(error(resource_error(threads), threaded(Goals))).

threaded(Goals) :-
	'$lgt_must_be'(callable, Goals, threaded(Goals)),
	'$lgt_tr_threaded_call'(Goals, MTGoals),
	catch(MTGoals, Error, '$lgt_runtime_error_handler'(Error)).


% threaded_call(@callable, -nonvar)

threaded_call(Goal, Tag) :-
	\+ '$lgt_compiler_flag'(threads, supported),
	throw(error(resource_error(threads), threaded_call(Goal, Tag))).

threaded_call(Goal, Tag) :-
	'$lgt_must_be'(callable, Goal, threaded_call(Goal, Tag)),
	'$lgt_must_be'(variable, Tag, threaded_call(Goal, Tag)),
	'$lgt_current_object_'(user, Prefix, _, _, _, _, _, _, _, _, _),
	catch('$lgt_threaded_call_tagged'(Prefix, Goal, user, user, Tag), Error, '$lgt_runtime_error_handler'(Error)).


% threaded_call(@callable)

threaded_call(Goal) :-
	\+ '$lgt_compiler_flag'(threads, supported),
	throw(error(resource_error(threads), threaded_call(Goal))).

threaded_call(Goal) :-
	'$lgt_must_be'(callable, Goal, threaded_call(Goal)),
	'$lgt_current_object_'(user, Prefix, _, _, _, _, _, _, _, _, _),
	catch('$lgt_threaded_call'(Prefix, Goal, user, user), Error, '$lgt_runtime_error_handler'(Error)).


% threaded_once(@callable, -nonvar)

threaded_once(Goal, Tag) :-
	\+ '$lgt_compiler_flag'(threads, supported),
	throw(error(resource_error(threads), threaded_once(Goal, Tag))).

threaded_once(Goal, Tag) :-
	'$lgt_must_be'(callable, Goal, threaded_once(Goal, Tag)),
	'$lgt_must_be'(variable, Tag, threaded_once(Goal, Tag)),
	'$lgt_current_object_'(user, Prefix, _, _, _, _, _, _, _, _, _),
	catch('$lgt_threaded_once_tagged'(Prefix, Goal, user, user, Tag), Error, '$lgt_runtime_error_handler'(Error)).


% threaded_once(@callable)

threaded_once(Goal) :-
	\+ '$lgt_compiler_flag'(threads, supported),
	throw(error(resource_error(threads), threaded_once(Goal))).

threaded_once(Goal) :-
	'$lgt_must_be'(callable, Goal, threaded_once(Goal)),
	'$lgt_current_object_'(user, Prefix, _, _, _, _, _, _, _, _, _),
	catch('$lgt_threaded_once'(Prefix, Goal, user, user), Error, '$lgt_runtime_error_handler'(Error)).


% threaded_ignore(@callable)

threaded_ignore(Goal) :-
	\+ '$lgt_compiler_flag'(threads, supported),
	throw(error(resource_error(threads), threaded_ignore(Goal))).

threaded_ignore(Goal) :-
	'$lgt_must_be'(callable, Goal, threaded_ignore(Goal)),
	catch('$lgt_threaded_ignore'(Goal), Error, '$lgt_runtime_error_handler'(Error)).


% threaded_exit(+callable, +nonvar)

threaded_exit(Goal, Tag) :-
	\+ '$lgt_compiler_flag'(threads, supported),
	throw(error(resource_error(threads), threaded_exit(Goal, Tag))).

threaded_exit(Goal, Tag) :-
	'$lgt_must_be'(callable, Goal, threaded_exit(Goal, Tag)),
	'$lgt_must_be'(nonvar, Tag, threaded_exit(Goal, Tag)),
	'$lgt_current_object_'(user, Prefix, _, _, _, _, _, _, _, _, _),
	catch('$lgt_threaded_exit_tagged'(Prefix, Goal, user, user, user, Tag), Error, '$lgt_runtime_error_handler'(Error)).


% threaded_exit(+callable)

threaded_exit(Goal) :-
	\+ '$lgt_compiler_flag'(threads, supported),
	throw(error(resource_error(threads), threaded_exit(Goal))).

threaded_exit(Goal) :-
	'$lgt_must_be'(callable, Goal, threaded_exit(Goal)),
	'$lgt_current_object_'(user, Prefix, _, _, _, _, _, _, _, _, _),
	catch('$lgt_threaded_exit'(Prefix, Goal, user, user, user), Error, '$lgt_runtime_error_handler'(Error)).


% threaded_peek(+callable, +nonvar)

threaded_peek(Goal, Tag) :-
	\+ '$lgt_compiler_flag'(threads, supported),
	throw(error(resource_error(threads), threaded_peek(Goal, Tag))).

threaded_peek(Goal, Tag) :-
	'$lgt_must_be'(callable, Goal, threaded_peek(Goal, Tag)),
	'$lgt_must_be'(nonvar, Tag, threaded_peek(Goal, Tag)),
	'$lgt_current_object_'(user, Prefix, _, _, _, _, _, _, _, _, _),
	catch('$lgt_threaded_peek_tagged'(Prefix, Goal, user, user, user, Tag), Error, '$lgt_runtime_error_handler'(Error)).


% threaded_peek(+callable)

threaded_peek(Goal) :-
	\+ '$lgt_compiler_flag'(threads, supported),
	throw(error(resource_error(threads), threaded_peek(Goal))).

threaded_peek(Goal) :-
	'$lgt_must_be'(callable, Goal, threaded_peek(Goal)),
	'$lgt_current_object_'(user, Prefix, _, _, _, _, _, _, _, _, _),
	catch('$lgt_threaded_peek'(Prefix, Goal, user, user, user), Error, '$lgt_runtime_error_handler'(Error)).


% threaded_wait(?nonvar)

threaded_wait(Message) :-
	\+ '$lgt_compiler_flag'(threads, supported),
	throw(error(resource_error(threads), threaded_wait(Message))).

threaded_wait(Message) :-
	'$lgt_current_object_'(user, Prefix, _, _, _, _, _, _, _, _, _),
	'$lgt_threaded_wait'(Message, Prefix).


% threaded_notify(@term)

threaded_notify(Message) :-
	var(Message),
	throw(error(instantiation_error, threaded_notify(Message))).

threaded_notify(Message) :-
	\+ '$lgt_compiler_flag'(threads, supported),
	throw(error(resource_error(threads), threaded_notify(Message))).

threaded_notify(Message) :-
	'$lgt_current_object_'(user, Prefix, _, _, _, _, _, _, _, _, _),
	'$lgt_threaded_notify'(Message, Prefix).



% compiling and loading built-in predicates


% '$lgt_compiler_flag'(+atom, ?nonvar)
%
% gets/checks the current value of a compiler flag

'$lgt_compiler_flag'(Option, Value) :-
	(	'$lgt_pp_entity_compiler_flag_'(Option, Value2) ->	% flag value as defined within the entity being compiled
		Value = Value2
	;	'$lgt_pp_file_compiler_flag_'(Option, Value2) ->	% flag value as defined in the options argument of the
		Value = Value2										% compiling and loading predicates or in the source file
	;	'$lgt_current_flag_'(Option, Value2) ->				% default value for the current Logtalk session,
		Value = Value2										% set by calls to the set_logtalk_flag/2 predicate
	;	'$lgt_default_flag'(Option, Value2) ->				% default value, defined on the Prolog config files
		Value = Value2
	;	'$lgt_prolog_feature'(Option, Value)				% back-end Prolog compiler features
	).



% '$lgt_file_type_alt_directory'(+atom, ?atom)
%
% gets/checks the current value of the alternate compilation directory for the given file type

'$lgt_file_type_alt_directory'(xml, Directory) :-
	'$lgt_compiler_flag'(xmldir, Directory).

'$lgt_file_type_alt_directory'(prolog, Directory) :-
	'$lgt_compiler_flag'(tmpdir, Directory).

'$lgt_file_type_alt_directory'(tmp, Directory) :-
	'$lgt_compiler_flag'(tmpdir, Directory).



% logtalk_compile(@source_file_name)
% logtalk_compile(@source_file_name_list)
%
% compiles to disk a source file or list of source files using default options

logtalk_compile(Files) :-
	catch(
		logtalk_compile(Files, []),
		error(Error, _),
		throw(error(Error, logtalk_compile(Files)))).



% logtalk_compile(@source_file_name, @list)
% logtalk_compile(@source_file_name_list, @list)
%
% compiles to disk a source file or a list of source files using a list of flag options

logtalk_compile(Files, Flags) :-
	catch(
		('$lgt_init_warnings_counter'(logtalk_compile(Files, Flags)),
		 '$lgt_check_source_files'(Files),
		 '$lgt_check_compiler_flags'(Flags),
		 '$lgt_compile_files'(Files, Flags),
		 '$lgt_report_warning_numbers'(logtalk_compile(Files, Flags)),
		 '$lgt_clear_compiler_flags'),
		error(Error, _),
		('$lgt_clear_compiler_flags',
		 '$lgt_clean_pp_clauses',
		 '$lgt_reset_warnings_counter',
		 throw(error(Error, logtalk_compile(Files, Flags))))).



% predicates for compilation warning counting and reporting

'$lgt_reset_warnings_counter' :-
	retractall('$lgt_pp_warnings_top_argument_'(_)),
	retractall('$lgt_pp_comp_warnings_counter_'(_)),
	retractall('$lgt_pp_load_warnings_counter_'(_)),
	retractall('$lgt_pp_entity_warnings_flag_').


'$lgt_init_warnings_counter'(Term) :-
	(	'$lgt_pp_warnings_top_argument_'(_) ->
		true
	;	asserta('$lgt_pp_warnings_top_argument_'(Term)),	% remember top compilation/loading goal
		retractall('$lgt_pp_comp_warnings_counter_'(_)),	% initialize compilation warnings counter
		asserta('$lgt_pp_comp_warnings_counter_'(0)),
		retractall('$lgt_pp_load_warnings_counter_'(_)),	% initialize loading warnings counter
		asserta('$lgt_pp_load_warnings_counter_'(0)),
		retractall('$lgt_pp_entity_warnings_flag_'),
		retractall('$lgt_pp_load_warnings_flag_')
	).


'$lgt_inc_compile_warnings_counter' :-
	retract('$lgt_pp_comp_warnings_counter_'(Old)),
	New is Old + 1,
	asserta('$lgt_pp_comp_warnings_counter_'(New)),
	(	'$lgt_pp_entity_warnings_flag_' ->
		true
	;	assertz('$lgt_pp_entity_warnings_flag_')
	).


'$lgt_inc_load_warnings_counter' :-
	retract('$lgt_pp_load_warnings_counter_'(Old)),
	New is Old + 1,
	asserta('$lgt_pp_load_warnings_counter_'(New)),
	(	'$lgt_pp_load_warnings_flag_' ->
		true
	;	assertz('$lgt_pp_load_warnings_flag_')
	).


'$lgt_report_warning_numbers'(Term) :-
	(	retract('$lgt_pp_warnings_top_argument_'(Term)),				% if top compilation/loading goal then
		retract('$lgt_pp_comp_warnings_counter_'(CCounter)),			% report compilation and loading warnings
		retract('$lgt_pp_load_warnings_counter_'(LCounter)) ->
		(	'$lgt_compiler_flag'(report, off) ->
			true
		;	(	CCounter + LCounter =:= 0 ->							% no warnings
				write('% (0 warnings)'), nl
			;	CCounter =:= 0 ->										% no compilation warnings 
				write('% ('), write(LCounter), write(' loading '),
				'$lgt_write_warnings_word'(LCounter), write(')'), nl
			;	LCounter =:= 0 ->										% no loading warnings
				write('% ('), write(CCounter), write(' compilation '),
				'$lgt_write_warnings_word'(CCounter), write(')'), nl
			;	write('% ('), write(CCounter), write(' compilation '),	% both compilation and loading warnings
				'$lgt_write_warnings_word'(CCounter), write(' and '),
				write(LCounter), write(' loading '),
				'$lgt_write_warnings_word'(LCounter), write(')'), nl
			)
		)
	;	% not top compilation/loading goal
		true
	).


'$lgt_write_warnings_word'(Number) :-
	(	Number =:= 1 ->
		write(warning)
	;	write(warnings)
	).



% '$lgt_check_source_files'(@term)
%
% check if the source file names are valid and correspond to existing files

'$lgt_check_source_files'(Files) :-
	var(Files),
	throw(instantiation_error).

'$lgt_check_source_files'([]) :-
	!.

'$lgt_check_source_files'([File| Files]) :-
	!,
	'$lgt_check_source_file'(File),
	'$lgt_check_source_files'(Files).

'$lgt_check_source_files'(File) :-
	'$lgt_check_source_file'(File).


'$lgt_check_source_file'(File) :-
	var(File),
	throw(instantiation_error).

'$lgt_check_source_file'(File) :-
	compound(File),
	!,
	'$lgt_check_library_source_file'(File).

'$lgt_check_source_file'(File) :-
	\+ atom(File),
	throw(type_error(source_file_name, File)).

'$lgt_check_source_file'(File) :-
	'$lgt_file_name'(logtalk, File, FileWithExtension, _),
	\+ '$lgt_file_exists'(FileWithExtension),
	throw(existence_error(file, File)).

'$lgt_check_source_file'(_).



'$lgt_check_library_source_file'(Term) :-
	(	Term =.. [Library, File] ->
		'$lgt_check_library_source_file'(Library, File)
	;	throw(type_error(source_file_name, Term))
	).


'$lgt_check_library_source_file'(Library, File) :-
	'$lgt_expand_library_path'(Library, Path),
	'$lgt_directory_exists'(Path),
	'$lgt_current_directory'(Current),
	'$lgt_change_directory'(Path),
	catch(
		'$lgt_check_source_file'(File),
		Error,
		('$lgt_change_directory'(Current), throw(Error))),
	'$lgt_change_directory'(Current),
	!.

'$lgt_check_library_source_file'(Library, _) :-
	throw(existence_error(library, Library)).


% '$lgt_expand_library_path'(+atom, -atom)
%
% converts a library alias into its corresponding path; uses a depth
% bound to prevent loops (inspired by similar code in SWI-Prolog)

'$lgt_expand_library_path'(Library, ExpandedPath) :-
	'$lgt_expand_library_path'(Library, Path, 16),
	(	'$lgt_expand_path'(Path, ExpandedPath) ->
		true
	;	% some Prolog compilers don't provide the necessary support for expanding paths
		ExpandedPath = Path
	).


'$lgt_expand_library_path'(Library, Path, N) :-
	(	logtalk_library_path(Library, Location) ->
		(	atom(Location) ->
			Path = Location
		;	Location =.. [Library2, Location2],
			N > 0,
			N2 is N - 1,
			'$lgt_expand_library_path'(Library2, Path2, N2),
			atom_concat(Path2, Location2, Path)
		)
	;	atom(Library),
		Path = Library
	).



% '$lgt_check_compiler_flags'(@list)
%
% checks if the compiler flags are valid

'$lgt_check_compiler_flags'(Options) :-
	var(Options),
	throw(instantiation_error).

'$lgt_check_compiler_flags'(Options) :-
	\+ '$lgt_is_proper_list'(Options),
	throw(type_error(list, Options)).

'$lgt_check_compiler_flags'(Options) :-
	'$lgt_check_compiler_flag_list'(Options).


'$lgt_check_compiler_flag_list'([]).

'$lgt_check_compiler_flag_list'([Option| Options]) :-	
	'$lgt_check_compiler_flag'(Option),
	'$lgt_check_compiler_flag_list'(Options).


'$lgt_check_compiler_flag'(Option) :-
	var(Option),
	throw(instantiation_error).

'$lgt_check_compiler_flag'(Option) :-
	\+ compound(Option),
	throw(type_error(flag, Option)).

'$lgt_check_compiler_flag'(Option) :-
	\+ functor(Option, _, 1),
	throw(type_error(flag, Option)).

'$lgt_check_compiler_flag'(Option) :-
	Option =.. [Flag, Value],
	'$lgt_check_compiler_flag'(Flag, Value).


'$lgt_check_compiler_flag'(Flag, _) :-
	var(Flag),
	throw(instantiation_error).

'$lgt_check_compiler_flag'(_, Value) :-
	var(Value),
	throw(instantiation_error).

'$lgt_check_compiler_flag'(Flag, _) :-
	\+ atom(Flag),
	throw(type_error(atom, Flag)).

'$lgt_check_compiler_flag'(Flag, _) :-
	\+ '$lgt_valid_flag'(Flag),
	throw(domain_error(logtalk_flag, Flag)).

'$lgt_check_compiler_flag'(Flag, _) :-
	'$lgt_read_only_flag'(Flag),
	throw(permission_error(modify, flag, Flag)).

'$lgt_check_compiler_flag'(Flag, Value) :-
	\+ '$lgt_valid_flag_value'(Flag, Value),
	throw(domain_error(flag_value, Flag + Value)).

'$lgt_check_compiler_flag'(_, _).



% '$lgt_set_compiler_flags'(@list)
%
% sets the compiler flag options

'$lgt_set_compiler_flags'(Flags) :-
	'$lgt_assert_compiler_flags'(Flags),
	(	'$lgt_pp_file_compiler_flag_'(debug, on) ->							% debug flag on requires the
		retractall('$lgt_pp_file_compiler_flag_'(smart_compilation, _)),	% smart_compilation flag to 
		asserta('$lgt_pp_file_compiler_flag_'(smart_compilation, off)),		% be off and 
		retractall('$lgt_pp_file_compiler_flag_'(reload, _)),				% the reload flag to be set
		asserta('$lgt_pp_file_compiler_flag_'(reload, always))				% to always
	;	true
	),
	(	'$lgt_pp_file_compiler_flag_'(hook, Obj) ->							% pre-compile hooks in order 
		(	Obj == user ->													% to speed up entity compilation
			TermExpansionGoal = term_expansion(Term, Terms),
			GoalExpansionGoal = goal_expansion(Goal, EGoal)
		;	'$lgt_tr_msg'(term_expansion(Term, Terms), Obj, TermExpansionGoal, user),
			'$lgt_tr_msg'(goal_expansion(Goal, EGoal), Obj, GoalExpansionGoal, user)
		),
		assertz(('$lgt_pp_hook_term_expansion_'(Term, Terms) :- catch(TermExpansionGoal, _, fail))),
		assertz(('$lgt_pp_hook_goal_expansion_'(Goal, EGoal) :- catch(GoalExpansionGoal, _, fail)))
	;	true
	).


'$lgt_assert_compiler_flags'([]).

'$lgt_assert_compiler_flags'([Flag| Flags]) :-
	Flag =.. [Name, Value],
	retractall('$lgt_pp_file_compiler_flag_'(Name, _)),
	asserta('$lgt_pp_file_compiler_flag_'(Name, Value)),
	'$lgt_assert_compiler_flags'(Flags).



% '$lgt_clear_compiler_flags'(@list)
%
% clears the compiler flag options

'$lgt_clear_compiler_flags' :-
	retractall('$lgt_pp_file_compiler_flag_'(_, _)),	% retract file flag values
	retractall('$lgt_pp_hook_term_expansion_'(_, _)),	% plus any term and
	retractall('$lgt_pp_hook_goal_expansion_'(_, _)).	% goal expansion hooks



% logtalk_load(@source_file_name)
% logtalk_load(@source_file_name_list)
%
% compiles to disk and then loads to memory a source file 
% or a list of source files using default compiler options

logtalk_load(Files) :-
	catch(
		logtalk_load(Files, []),
		error(Error, _),
		throw(error(Error, logtalk_load(Files)))).



% logtalk_load(@source_file_name, @list)
% logtalk_load(@source_file_name_list, @list)
%
% compiles to disk and then loads to memory a source file 
% or a list of source files using a list of compiler options

logtalk_load(Files, Flags) :-
	catch(
		('$lgt_init_warnings_counter'(logtalk_load(Files, Flags)),
		 '$lgt_check_source_files'(Files),
		 '$lgt_check_compiler_flags'(Flags),
		 '$lgt_load_files'(Files, Flags),
		 '$lgt_report_warning_numbers'(logtalk_load(Files, Flags))),
		error(Error, _),
		('$lgt_clear_compiler_flags',
		 '$lgt_clean_pp_clauses',
		 '$lgt_reset_warnings_counter',
		 throw(error(Error, logtalk_load(Files, Flags))))).



% logtalk_load_context(?atom, ?nonvar)
%
% provides access to the compilation/loading context

logtalk_load_context(file, File) :-
	'$lgt_pp_file_path_'(File, _).

logtalk_load_context(directory, Directory) :-
	'$lgt_pp_file_path_'(_, Directory).

logtalk_load_context(entity_name, Entity) :-
	(	'$lgt_pp_object_'(Entity, _, _, _, _, _, _, _, _, _, _) ->
		true
	;	'$lgt_pp_protocol_'(Entity, _, _, _, _) ->
		true
	;	'$lgt_pp_category_'(Entity, _, _, _, _, _)
	).

logtalk_load_context(entity_prefix, Prefix) :-
	(	'$lgt_pp_object_'(_, Prefix, _, _, _, _, _, _, _, _, _) ->
		true
	;	'$lgt_pp_protocol_'(_, Prefix, _, _, _) ->
		true
	;	'$lgt_pp_category_'(_, Prefix, _, _, _, _)
	).

logtalk_load_context(entity_type, Type) :-
	(	'$lgt_pp_module_'(_) ->
		Type = module
	;	'$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, _, _) ->
		Type = object
	;	'$lgt_pp_protocol_'(_, _, _, _, _) ->
		Type = protocol
	;	'$lgt_pp_category_'(_, _, _, _, _, _),
		Type = category
	).

logtalk_load_context(term_position, Position) :-
	'$lgt_pp_term_position_'(Position).

logtalk_load_context(stream, Stream) :-
	stream_property(Stream, alias('$lgt_input')),
	!.	% avoid a spurious choice-point with some Prolog compilers



% set_logtalk_flag(+atom, +nonvar)
%
% sets a Logtalk flag

set_logtalk_flag(Flag, Value) :-
	catch('$lgt_check_compiler_flag'(Flag, Value), Error, throw(error(Error, set_logtalk_flag(Flag, Value)))),
	retractall('$lgt_current_flag_'(Flag, _)),
	assertz('$lgt_current_flag_'(Flag, Value)),
	(	Flag == debug, Value == on ->
		retractall('$lgt_current_flag_'(smart_compilation, _)),
		assertz('$lgt_current_flag_'(smart_compilation, off))
	;	Flag == smart_compilation, Value == on ->
		retractall('$lgt_current_flag_'(clean, _)),
		assertz('$lgt_current_flag_'(clean, off))
	;	Flag == clean, Value == on ->
		retractall('$lgt_current_flag_'(smart_compilation, _)),
		assertz('$lgt_current_flag_'(smart_compilation, off))
	;	Flag == hook ->
		'$lgt_compile_hooks'(Value)
	;	true
	).



% current_logtalk_flag(?atom, ?nonvar)
%
% tests/gets Logtalk flags

current_logtalk_flag(Flag, Value) :-
	nonvar(Flag),
	\+ atom(Flag),
	throw(error(type_error(atom, Flag), current_logtalk_flag(Flag, Value))).

current_logtalk_flag(Flag, Value) :-
	atom(Flag),
	\+ '$lgt_valid_flag'(Flag),
	throw(error(domain_error(logtalk_flag, Flag), current_logtalk_flag(Flag, Value))).

current_logtalk_flag(Flag, Value) :-
	'$lgt_current_flag_'(Flag, Value).

current_logtalk_flag(Flag, Value) :-
	'$lgt_default_flag'(Flag, Value),
	\+ '$lgt_current_flag_'(Flag, _).

current_logtalk_flag(Flag, Value) :-
	'$lgt_prolog_feature'(Flag, Value).

current_logtalk_flag(version, version(2, 43, 0)).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  built-in methods
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% '$lgt_obj_exists'(+object_identifier, +callable, +object_identifier)
%
% checks if an object exists at runtime; this is necessary in order to
% prevent trivial messages such as true/0 or repeat/0 from succeeding
% when the target object doesn't exist; used in the translation of ::/2
% calls

'$lgt_obj_exists'(Obj, Pred, Sender) :-
	(	'$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, _) ->
		true
	;	throw(error(existence_error(object, Obj), Obj::Pred, Sender))
	).



% '$lgt_current_predicate'(+object_identifier, +predicate_indicator, +object_identifier, +scope)
%
% current_predicate/1 built-in method

'$lgt_current_predicate'(Obj, Pred, Sender, _) :-
	nonvar(Pred),
	Pred \= _/_,
	throw(error(type_error(predicate_indicator, Pred), Obj::current_predicate(Pred), Sender)).

'$lgt_current_predicate'(Obj, Functor/Arity, Sender, _) :-
	nonvar(Functor),
	\+ atom(Functor),
	throw(error(type_error(predicate_indicator, Functor/Arity), Obj::current_predicate(Functor/Arity), Sender)).

'$lgt_current_predicate'(Obj, Functor/Arity, Sender, _) :-
	nonvar(Arity),
	\+ (integer(Arity), Arity >= 0),
	throw(error(type_error(predicate_indicator, Functor/Arity), Obj::current_predicate(Functor/Arity), Sender)).

'$lgt_current_predicate'(Obj, Functor/Arity, Sender, _) :-
	\+ '$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, _),
	throw(error(existence_error(object, Obj), Obj::current_predicate(Functor/Arity), Sender)).

'$lgt_current_predicate'(Obj, Functor/Arity, Sender, Scope) :-
	nonvar(Functor),
	nonvar(Arity),
	!,
	functor(Pred, Functor, Arity),
	'$lgt_visible_predicate'(Obj, Pred, Sender, Scope),
	!.

'$lgt_current_predicate'(Obj, Functor/Arity, Sender, Scope) :-
	setof(
		Functor/Arity,
		(Pred, Scope)^('$lgt_visible_predicate'(Obj, Pred, Sender, Scope), functor(Pred, Functor, Arity)),
		Preds),
	'$lgt_member'(Functor/Arity, Preds).


% '$lgt_visible_predicate'(@object_identifier, ?callable, @object_identifier, @term)
%
% checks/returns object predicates visible/within the scope of the sender 

'$lgt_visible_predicate'(Obj, Pred, Sender, Scope) :-
	'$lgt_current_object_'(Obj, _, Dcl, _, _, _, _, _, _, _, _),
	call(Dcl, Pred, PScope, _, _, SCtn, _),
	(	\+ \+ PScope = Scope ->
		true
	;	Sender = SCtn
	).



% '$lgt_predicate_property'(+object_identifier, @callable, ?predicate_property, +object_identifier, +scope)
%
% predicate_property/2 built-in method

'$lgt_predicate_property'(Obj, Pred, Prop, Sender, _) :-
	var(Pred),
	throw(error(instantiation_error, Obj::predicate_property(Pred, Prop), Sender)).

'$lgt_predicate_property'(Obj, Pred, Prop, Sender, _) :-
	nonvar(Prop),
	\+ '$lgt_valid_predicate_property'(Prop),
	throw(error(domain_error(predicate_property, Prop), Obj::predicate_property(Pred, Prop), Sender)).

'$lgt_predicate_property'(Obj, Pred, Prop, Sender, _) :-
	\+ callable(Pred),
	throw(error(type_error(callable, Pred), Obj::predicate_property(Pred, Prop), Sender)).

'$lgt_predicate_property'(Obj, Pred, Prop, Sender, _) :-
	\+ '$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, _),
	throw(error(existence_error(object, Obj), Obj::predicate_property(Pred, Prop), Sender)).

'$lgt_predicate_property'(Obj, Pred, Prop, Sender, Scope) :-
	'$lgt_current_object_'(Obj, _, Dcl, Def, _, _, _, _, _, Rnm, _),
	call(Dcl, Pred, PScope, Meta, Flags, SCtn, TCtn),
	!,
	(	\+ \+ PScope = Scope ->
		true
	;	Sender = SCtn
	),
	(	Prop = logtalk
	;	'$lgt_scope'(Prop, PScope)
	;	(	Flags /\ 2 =:= 2 ->
			Prop = (dynamic)
		;	Prop = static
		)
	;	Prop = declared_in(TCtn)
	;	Meta \== no,
		Meta =.. [_| MetaArgs],			% Pred can be an alias
		functor(Pred, Functor, _),
		Meta2 =.. [Functor| MetaArgs],
		Prop = meta_predicate(Meta2)
	;	Flags /\ 32 =:= 32,
		Prop = (coinductive)
	;	Flags /\ 16 =:= 16,
		Prop = (multifile)
	;	Flags /\ 8 =:= 8,
		functor(Pred, Functor, Arity2),
		Arity is Arity2 - 2,
		Prop = non_terminal(Functor//Arity)
	;	Flags /\ 4 =:= 4,
		Prop = synchronized
	;	once((	'$lgt_current_object_'(TCtn, _, TCtnDcl, _, _, _, _, _, _, _, _)
			;	'$lgt_current_category_'(TCtn, _, TCtnDcl, _, _, _)
			;	'$lgt_current_protocol_'(TCtn, _, TCtnDcl, _, _)
		)),
		\+ call(TCtnDcl, Pred, _, _, _),
		'$lgt_find_original_pred'(Obj, Rnm, Pred, Original),
		Prop = alias_of(Original)
	;	once(call(Def, Pred, _, _, DCtn)),
		Prop = defined_in(DCtn)
	).

'$lgt_predicate_property'(Obj, Pred, Prop, Sender, Scope) :-
	'$lgt_built_in_method'(Pred, PScope, Meta, Flags),
	!,
	(	\+ \+ PScope = Scope ->
		true
	;	Sender = Obj
	),
	(	Prop = logtalk
	;	'$lgt_scope'(Prop, PScope)
	;	Flags /\ 1 =:= 1,		% just to help document predicate declaration bit patterns
		Prop = built_in			% as this clause only deals with built-in methods
	;	(	Flags /\ 2 =:= 2 ->
			Prop = (dynamic)
		;	Prop = static
		)
	;	Meta \== no,
		Prop = meta_predicate(Meta)
	;	Flags /\ 16 =:= 16,
		Prop = (multifile)
	;	Flags /\ 8 =:= 8,
		functor(Pred, Functor, Arity2),
		Arity is Arity2 - 2,
		Prop = non_terminal(Functor//Arity)
	;	Flags /\ 4 =:= 4,
		Prop = synchronized
	).

'$lgt_predicate_property'(_, Pred, Prop, _, _) :-
	'$lgt_lgt_built_in'(Pred),
	!,
	(	Prop = logtalk
	;	Prop = (public)
	;	Prop = built_in
	;	Prop = static
	).

'$lgt_predicate_property'(_, Pred, Prop, _, _) :-
	'$lgt_pl_built_in'(Pred),
	!,
	(	Prop = prolog
	;	(	'$lgt_pl_meta_predicate'(Pred, Meta, _) ->
			(	Prop = private
			;	Prop = meta_predicate(Meta)
			)
		;	Prop = (public)
		)
	;	Prop = built_in
	;	(	'$lgt_predicate_property'(Pred, (dynamic)) ->
			Prop = (dynamic)
		;	Prop = static
		)
	;	'$lgt_predicate_property'(Pred, (multifile)),
		Prop = (multifile)
	).



% '$lgt_scope'(?atom, ?nonvar).
%
% converts between user and system scope terms

'$lgt_scope'(private, p).
'$lgt_scope'(protected, p(p)).
'$lgt_scope'((public), p(p(p))).



% '$lgt_filter_scope'(+nonvar, -nonvar)
%
% filters predicate scope;
% used in the implementation of protected-qualified relations between entities:
% public predicates become protected predicates, protected and private predicates
% are unaffected

'$lgt_filter_scope'(p(_), p(p)).
'$lgt_filter_scope'(p, p).



% '$lgt_set_scope_container'(+nonvar, +object_identifier, +object_identifier, -object_identifier)
%
% sets predicate scope container;
% used in the implementation of private-qualified relations between entities:
% when the predicate is public or protected, the object inheriting the predicate
% becomes the scope container; when the predicate is private, the scope container
% is the inherited scope container

'$lgt_set_scope_container'(p(_), _, SCtn, SCtn).
'$lgt_set_scope_container'(p, SCtn, _, SCtn).



% '$lgt_find_original_pred'(+object_identifier, +atom, +callable, -callable)
%
% finds the predicate pointed by an alias

'$lgt_find_original_pred'(Obj, Rnm, Alias, Pred) :-
	'$lgt_find_original_pred'(Obj, Rnm, Alias, Pred, _).


'$lgt_find_original_pred'(_, Rnm, Alias, Pred, _) :-
	once(call(Rnm, _, Pred, Alias)),
	Pred \= Alias,
	!.

'$lgt_find_original_pred'(Obj, _, Alias, Pred, _) :-
	'$lgt_implements_protocol_'(Obj, Ptc, _),
	'$lgt_current_protocol_'(Ptc, _, _, Rnm, _),
	'$lgt_find_original_pred'(Ptc, Rnm, Alias, Pred, _).

'$lgt_find_original_pred'(Ptc1, _, Alias, Pred, _) :-
	'$lgt_extends_protocol_'(Ptc1, Ptc2, _),
	'$lgt_current_protocol_'(Ptc2, _, _, Rnm, _),
	'$lgt_find_original_pred'(Ptc2, Rnm, Alias, Pred, _).

'$lgt_find_original_pred'(Ctg1, _, Alias, Pred, _) :-
	'$lgt_extends_category_'(Ctg1, Ctg2, _),
	'$lgt_current_category_'(Ctg2, _, _, _, Rnm, _),
	'$lgt_find_original_pred'(Ctg2, Rnm, Alias, Pred, _).

'$lgt_find_original_pred'(Obj, _, Alias, Pred, _) :-
	'$lgt_imports_category_'(Obj, Ctg, _),
	'$lgt_current_category_'(Ctg, _, _, _, Rnm, _),
	'$lgt_find_original_pred'(Ctg, Rnm, Alias, Pred, _).

'$lgt_find_original_pred'(Obj, _, Alias, Pred, prototype) :-
	'$lgt_extends_object_'(Obj, Parent, _),
	'$lgt_current_object_'(Parent, _, _, _, _, _, _, _, _, Rnm, _),
	'$lgt_find_original_pred'(Parent, Rnm, Alias, Pred, prototype).

'$lgt_find_original_pred'(Instance, _, Alias, Pred, instance) :-
	'$lgt_instantiates_class_'(Instance, Class, _),
	'$lgt_current_object_'(Class, _, _, _, _, _, _, _, _, Rnm, _),
	'$lgt_find_original_pred'(Class, Rnm, Alias, Pred, superclass).

'$lgt_find_original_pred'(Class, _, Alias, Pred, superclass) :-
	'$lgt_specializes_class_'(Class, Superclass, _),
	'$lgt_current_object_'(Superclass, _, _, _, _, _, _, _, _, Rnm, _),
	'$lgt_find_original_pred'(Superclass, Rnm, Alias, Pred, superclass).

'$lgt_find_original_pred'(Obj, _, Alias, Pred, _) :-
	'$lgt_complemented_object_'(Obj, Ctg, _, _, Rnm),
	'$lgt_find_original_pred'(Ctg, Rnm, Alias, Pred, _).



% '$lgt_abolish'(+object_identifier, +predicate_indicator, +object_identifier, +scope)
%
% abolish/1 built-in method

'$lgt_abolish'(Obj, Pred, Sender, Scope) :-
	'$lgt_must_be'(predicate_indicator, Pred, Obj::abolish(Pred), Sender),
	'$lgt_abolish_chk'(Obj, Pred, Sender, Scope).


'$lgt_abolish_chk'(Obj, Functor/Arity, Sender, Scope) :-
	'$lgt_current_object_'(Obj, _, Dcl, _, _, _, _, DDcl, DDef, _, _),
	!,
	functor(Pred, Functor, Arity),
	(	call(Dcl, Pred, PScope, _, Flags, SCtn, _) ->
		(	(\+ \+ PScope = Scope; Sender = SCtn) ->
			(	Flags /\ 2 =:= 2 ->
				functor(DDclClause, DDcl, 2),
				arg(1, DDclClause, Pred),
				(	call(DDclClause) ->
					retractall(DDclClause),
					functor(DDefClause, DDef, 3),
					arg(1, DDefClause, Pred),
					(	call(DDefClause) ->
						arg(3, DDefClause, TPred),
						functor(TPred, TFunctor, TArity),
						abolish(TFunctor/TArity),
						retractall(DDefClause),
						'$lgt_clean_lookup_caches'(Pred)
					;	true
					)
				;	% static predicate declaration:
					throw(error(permission_error(modify, predicate_declaration, Pred), Obj::abolish(Functor/Arity), Sender))
				)
			;	% predicate is static:
				throw(error(permission_error(modify, static_predicate, Pred), Obj::abolish(Functor/Arity), Sender))
			)
		;	% predicate is not within the scope of the sender:
			(	PScope == p ->
				throw(error(permission_error(modify, private_predicate, Pred), Obj::abolish(Functor/Arity), Sender))
			;	throw(error(permission_error(modify, protected_predicate, Pred), Obj::abolish(Functor/Arity), Sender))
			)
		)
	;	functor(DDefClause, DDef, 3),
		arg(1, DDefClause, Pred),
		call(DDefClause) ->
		% local dynamic predicate:
		arg(3, DDefClause, TPred),
		functor(TPred, TFunctor, TArity),
		abolish(TFunctor/TArity),
		retractall(DDefClause),
		'$lgt_clean_lookup_caches'(Pred)
	;	% no predicate declaration:
		throw(error(existence_error(predicate_declaration, Pred), Obj::abolish(Functor/Arity), Sender))
	).

'$lgt_abolish_chk'(Obj, Functor/Arity, Sender, _) :-
	throw(error(existence_error(object, Obj), Obj::abolish(Functor/Arity), Sender)).



% '$lgt_asserta'(+object_identifier, @clause, +object_identifier, +scope, +scope)
%
% asserta/1 built-in method

'$lgt_asserta'(Obj, Clause, Sender, _, _) :-
	var(Clause),
	throw(error(instantiation_error, Obj::asserta(Clause), Sender)).

'$lgt_asserta'(Obj, Clause, Sender, _, _) :-
	'$lgt_db_lookup_cache_'(Obj, Clause, Sender, TClause, _),
	!,
	asserta(TClause).

'$lgt_asserta'(Obj, (Head:-Body), Sender, _, _) :-
	var(Head),
	throw(error(instantiation_error, Obj::asserta((Head:-Body)), Sender)).

'$lgt_asserta'(Obj, (Head:-Body), Sender, _, _) :-
	\+ callable(Head),
	throw(error(type_error(callable, Head), Obj::asserta((Head:-Body)), Sender)).

'$lgt_asserta'(Obj, (Head:-Body), Sender, _, _) :-
	nonvar(Body),
	\+ callable(Body),
	throw(error(type_error(callable, Body), Obj::asserta((Head:-Body)), Sender)).

'$lgt_asserta'(Obj, Clause, Sender, _, _) :-
	Clause \= (_ :- _),
	\+ callable(Clause),
	throw(error(type_error(callable, Clause), Obj::asserta(Clause), Sender)).

'$lgt_asserta'(Obj, Clause, Sender, TestScope, DclScope) :-
	(	Clause = (Head :- Body) ->
		(	Body == true ->
			'$lgt_asserta_fact_chk'(Obj, Head, Sender, TestScope, DclScope)
		;	'$lgt_asserta_rule_chk'(Obj, Clause, Sender, TestScope, DclScope)
		)
	;	'$lgt_asserta_fact_chk'(Obj, Clause, Sender, TestScope, DclScope)
	).


'$lgt_asserta_rule_chk'(Obj, (Head:-Body), Sender, TestScope, DclScope) :-
	'$lgt_current_object_'(Obj, Prefix, Dcl, Def, _, _, _, DDcl, DDef, _, Flags),
	!,
	'$lgt_assert_pred_dcl'(Dcl, DDcl, DDef, Flags, Head, Scope, Type, Meta, SCtn, DclScope, Obj::asserta((Head:-Body)), Sender),
	(	(Type == (dynamic); Flags /\ 2 =:= 2, Sender = SCtn) ->
		(	(\+ \+ Scope = TestScope; Sender = SCtn) ->
			'$lgt_assert_pred_def'(Def, DDef, Prefix, Head, ExCtx, THead, _),
			'$lgt_pred_meta_vars'(Head, Meta, MetaVars),
			'$lgt_comp_ctx'(Ctx, _, _, _, _, Prefix, MetaVars, _, ExCtx, runtime, _),
			'$lgt_tr_body'(Body, TBody, DBody, Ctx),
			(	'$lgt_debugging_entity_'(Obj) ->
				asserta((THead :- ('$lgt_nop'(Body), '$lgt_debugger.head'(Head, 0, ExCtx), DBody)))
			;	asserta((THead :- ('$lgt_nop'(Body), TBody)))
			)
		;	% predicate is not within the scope of the sender:
			(	Scope == p ->
				throw(error(permission_error(modify, private_predicate, Head), Obj::asserta((Head:-Body)), Sender))
			;	throw(error(permission_error(modify, protected_predicate, Head), Obj::asserta((Head:-Body)), Sender))
			)
		)
	;	% predicate is static:
		throw(error(permission_error(modify, static_predicate, Head), Obj::asserta((Head:-Body)), Sender))
	).

'$lgt_asserta_rule_chk'(Obj, (Head:-Body), Sender, _, _) :-
	throw(error(existence_error(object, Obj), Obj::asserta((Head:-Body)), Sender)).


'$lgt_asserta_fact_chk'(Obj, Head, Sender, _, _) :-
	'$lgt_db_lookup_cache_'(Obj, Head, Sender, THead, _),
	!,
	asserta(THead).

'$lgt_asserta_fact_chk'(Obj, Head, Sender, TestScope, DclScope) :-
	'$lgt_current_object_'(Obj, Prefix, Dcl, Def, _, _, _, DDcl, DDef, _, Flags),
	!,
	'$lgt_assert_pred_dcl'(Dcl, DDcl, DDef, Flags, Head, Scope, Type, _, SCtn, DclScope, Obj::asserta(Head), Sender),
	(	(Type == (dynamic); Flags /\ 2 =:= 2, Sender = SCtn) ->
		(	(\+ \+ Scope = TestScope; Sender = SCtn)  ->
			'$lgt_assert_pred_def'(Def, DDef, Prefix, Head, ExCtx, THead, Update),
			(	'$lgt_debugging_entity_'(Obj) ->
				asserta((THead :- '$lgt_debugger.fact'(Head, 0, ExCtx)))
			;	'$lgt_add_db_lookup_cache_entry'(Obj, Head, SCtn, DclScope, Type, Sender, THead, DDef, Update),
				asserta(THead)
			)
		;	% predicate is not within the scope of the sender:
			(	Scope == p ->
				throw(error(permission_error(modify, private_predicate, Head), Obj::asserta(Head), Sender))
			;	throw(error(permission_error(modify, protected_predicate, Head), Obj::asserta(Head), Sender))
			)
		)
	;	% predicate is static:
		throw(error(permission_error(modify, static_predicate, Head), Obj::asserta(Head), Sender))
	).

'$lgt_asserta_fact_chk'(Obj, Head, Sender, _, _) :-
	throw(error(existence_error(object, Obj), Obj::asserta(Head), Sender)).



% '$lgt_assertz'(+object_identifier, @clause, +object_identifier, +scope, +scope)
%
% assertz/1 built-in method

'$lgt_assertz'(Obj, Clause, Sender, _, _) :-
	var(Clause),
	throw(error(instantiation_error, Obj::assertz(Clause), Sender)).

'$lgt_assertz'(Obj, Clause, Sender, _, _) :-
	'$lgt_db_lookup_cache_'(Obj, Clause, Sender, TClause, _),
	!,
	assertz(TClause).

'$lgt_assertz'(Obj, (Head:-Body), Sender, _, _) :-
	var(Head),
	throw(error(instantiation_error, Obj::assertz((Head:-Body)), Sender)).

'$lgt_assertz'(Obj, (Head:-Body), Sender, _, _) :-
	\+ callable(Head),
	throw(error(type_error(callable, Head), Obj::assertz((Head:-Body)), Sender)).

'$lgt_assertz'(Obj, (Head:-Body), Sender, _, _) :-
	nonvar(Body),
	\+ callable(Body),
	throw(error(type_error(callable, Body), Obj::assertz((Head:-Body)), Sender)).

'$lgt_assertz'(Obj, Clause, Sender, _, _) :-
	Clause \= (_ :- _),
	\+ callable(Clause),
	throw(error(type_error(callable, Clause), Obj::assertz(Clause), Sender)).

'$lgt_assertz'(Obj, Clause, Sender, TestScope, DclScope) :-
	(	Clause = (Head :- Body) ->
		(	Body == true ->
			'$lgt_assertz_fact_chk'(Obj, Head, Sender, TestScope, DclScope)
		;	'$lgt_assertz_rule_chk'(Obj, Clause, Sender, TestScope, DclScope)
		)
	;	'$lgt_assertz_fact_chk'(Obj, Clause, Sender, TestScope, DclScope)
	).


'$lgt_assertz_rule_chk'(Obj, (Head:-Body), Sender, TestScope, DclScope) :-
	'$lgt_current_object_'(Obj, Prefix, Dcl, Def, _, _, _, DDcl, DDef, _, Flags),
	!,
	'$lgt_assert_pred_dcl'(Dcl, DDcl, DDef, Flags, Head, Scope, Type, Meta, SCtn, DclScope, Obj::assertz((Head:-Body)), Sender),
	(	(Type == (dynamic); Flags /\ 2 =:= 2, Sender = SCtn) ->
		(	(\+ \+ Scope = TestScope; Sender = SCtn)  ->
			'$lgt_assert_pred_def'(Def, DDef, Prefix, Head, ExCtx, THead, _),
			'$lgt_pred_meta_vars'(Head, Meta, MetaVars),
			'$lgt_comp_ctx'(Ctx, _, _, _, _, Prefix, MetaVars, _, ExCtx, runtime, _),
			'$lgt_tr_body'(Body, TBody, DBody, Ctx),
			(	'$lgt_debugging_entity_'(Obj) ->
				assertz((THead :- ('$lgt_nop'(Body), '$lgt_debugger.head'(Head, 0, ExCtx), DBody)))
			;	assertz((THead :- ('$lgt_nop'(Body), TBody)))
			)
		;	% predicate is not within the scope of the sender:
			(	Scope == p ->
				throw(error(permission_error(modify, private_predicate, Head), Obj::assertz((Head:-Body)), Sender))
			;	throw(error(permission_error(modify, protected_predicate, Head), Obj::assertz((Head:-Body)), Sender))
			)
		)
	;	% predicate is static:
		throw(error(permission_error(modify, static_predicate, Head), Obj::assertz((Head:-Body)), Sender))
	).

'$lgt_assertz_rule_chk'(Obj, (Head:-Body), Sender, _, _) :-
	throw(error(existence_error(object, Obj), Obj::assertz((Head:-Body)), Sender)).


'$lgt_assertz_fact_chk'(Obj, Head, Sender, _, _) :-
	'$lgt_db_lookup_cache_'(Obj, Head, Sender, THead, _),
	!,
	assertz(THead).

'$lgt_assertz_fact_chk'(Obj, Head, Sender, TestScope, DclScope) :-
	'$lgt_current_object_'(Obj, Prefix, Dcl, Def, _, _, _, DDcl, DDef, _, Flags),
	!,
	'$lgt_assert_pred_dcl'(Dcl, DDcl, DDef, Flags, Head, Scope, Type, _, SCtn, DclScope, Obj::assertz(Head), Sender),
	(	(Type == (dynamic); Flags /\ 2 =:= 2, Sender = SCtn) ->
		(	(\+ \+ Scope = TestScope; Sender = SCtn)  ->
			'$lgt_assert_pred_def'(Def, DDef, Prefix, Head, ExCtx, THead, Update),
			(	'$lgt_debugging_entity_'(Obj) ->
				assertz((THead :- '$lgt_debugger.fact'(Head, 0, ExCtx)))
			;	'$lgt_add_db_lookup_cache_entry'(Obj, Head, SCtn, DclScope, Type, Sender, THead, DDef, Update),
				assertz(THead)
			)
		;	% predicate is not within the scope of the sender:
			(	Scope == p ->
				throw(error(permission_error(modify, private_predicate, Head), Obj::assertz(Head), Sender))
			;	throw(error(permission_error(modify, protected_predicate, Head), Obj::assertz(Head), Sender))
			)
		)
	;	% predicate is static:
		throw(error(permission_error(modify, static_predicate, Head), Obj::assertz(Head), Sender))
	).

'$lgt_assertz_fact_chk'(Obj, Head, Sender, _, _) :-
	throw(error(existence_error(object, Obj), Obj::assertz(Head), Sender)).



% gets or sets (if it doesn't exist) the declaration for an asserted predicate (but we must
% not add a scope declaration when asserting clauses for a *local* dynamic predicate)

'$lgt_assert_pred_dcl'(Dcl, DDcl, DDef, ObjFlags, Pred, Scope, Type, Meta, SCtn, DclScope, Goal, Sender) :-
	(	call(Dcl, Pred, Scope, Meta, PredFlags, SCtn, _) ->
		% predicate declaration found; get predicate type:
		(	PredFlags /\ 2 =:= 2 ->
			Type = (dynamic)
		;	Type = static
		)
	;	% no predicate declaration; check for a local dynamic predicate if we're asserting locally:
		(DclScope == p, call(DDef, Pred, _, _)) ->
		Scope = DclScope, Type = (dynamic), Meta = no
	;	% not a local dynamic predicate; check if dynamic declaration of new predicates is allowed:
		(DclScope == p; ObjFlags /\ 64 =:= 64) ->
		'$lgt_term_template'(Pred, DPred),
		functor(Clause, DDcl, 2),
		arg(1, Clause, DPred),
		arg(2, Clause, DclScope),
		assertz(Clause),
		Scope = DclScope, Type = (dynamic), Meta = no
	;	% object doesn't allow dynamic declaration of new predicates:
		throw(error(permission_error(create, predicate_declaration, Pred), Goal, Sender))
	).



% gets or sets (if it doesn't exist) the compiled call for an asserted predicate

'$lgt_assert_pred_def'(Def, DDef, Prefix, Head, ExCtx, THead, NeedsUpdate) :-
	(	call(Def, Head, ExCtx, THead) ->
		% static definition lookup entries don't require update goals
		NeedsUpdate = false
	;	call(DDef, Head, ExCtx, THead) ->
		% dynamic definition lookup entries always require update goals
		NeedsUpdate = true
	;	% no definition lookup entry exists; construct and assert a dynamic one 
		functor(Head, Functor, Arity),
		functor(GHead, Functor, Arity),
		'$lgt_construct_predicate_indicator'(Prefix, Functor/Arity, TFunctor/TArity),
		functor(THead, TFunctor, TArity),
		'$lgt_unify_head_thead_args'(Arity, GHead, THead),
		arg(TArity, THead, ExCtx),
		functor(DDefClause, DDef, 3),
		arg(1, DDefClause, GHead),
		arg(2, DDefClause, ExCtx),
		arg(3, DDefClause, THead),
		assertz(DDefClause),
		'$lgt_clean_lookup_caches'(GHead),
		NeedsUpdate = true,
		GHead = Head
	).



% '$lgt_clause'(+object_identifier, @callable, @callable, +object_identifier, +scope)
%
% clause/2 built-in method

'$lgt_clause'(Obj, Head, Body, Sender, _) :-
	var(Head),
	throw(error(instantiation_error, Obj::clause(Head, Body), Sender)).

'$lgt_clause'(Obj, Head, Body, Sender, _) :-
	\+ callable(Head),
	throw(error(type_error(callable, Head), Obj::clause(Head, Body), Sender)).

'$lgt_clause'(Obj, Head, Body, Sender, _) :-
	nonvar(Body),
	\+ callable(Body),
	throw(error(type_error(callable, Body), Obj::clause(Head, Body), Sender)).

'$lgt_clause'(Obj, Head, Body, Sender, Scope) :-
	'$lgt_clause_chk'(Obj, Head, Body, Sender, Scope).


'$lgt_clause_chk'(Obj, Head, Body, Sender, _) :-
	'$lgt_db_lookup_cache_'(Obj, Head, Sender, THead, _),
	!,
	clause(THead, TBody),
	(	TBody = ('$lgt_nop'(Body), _) ->			% rules (compiled both in normal and debug mode)
		true
	;	TBody = '$lgt_debugger.fact'(_, _, _) ->	% facts compiled in debug mode
		Body = true
	;	TBody = Body								% facts compiled in normal mode
	).

'$lgt_clause_chk'(Obj, Head, Body, Sender, Scope) :-
	'$lgt_current_object_'(Obj, _, Dcl, Def, _, _, _, _, DDef, _, ObjFlags),
	!,
	(	call(Dcl, Head, PScope, _, PredFlags, SCtn, _) ->
		(	(PredFlags /\ 2 =:= 2; ObjFlags /\ 2 =:= 2, Sender = SCtn) ->
			(	(\+ \+ PScope = Scope; Sender = SCtn) ->
				(	(call(DDef, Head, _, THead); call(Def, Head, _, THead)) ->
					clause(THead, TBody),
					(	TBody = ('$lgt_nop'(Body), _) ->
						true
					;	TBody = '$lgt_debugger.fact'(_, _, _) ->
						Body = true
					;	TBody = Body
					)
				)
			;	% predicate is not within the scope of the sender:
				(	PScope == p ->
					throw(error(permission_error(access, private_predicate, Head), Obj::clause(Head, Body), Sender))
				;	throw(error(permission_error(access, protected_predicate, Head), Obj::clause(Head, Body), Sender))
				)
			)
		;	% predicate is static:
			throw(error(permission_error(access, static_predicate, Head), Obj::clause(Head, Body), Sender))
		)
	;	% local dynamic predicate with no scope declaration:
		(	Obj = Sender,
			(call(DDef, Head, _, THead); call(Def, Head, _, THead)) ->
			clause(THead, TBody),
			(	TBody = ('$lgt_nop'(Body), _) ->
				true
			;	TBody = '$lgt_debugger.fact'(_, _, _) ->
				Body = true
			;	TBody = Body
			)
		;	throw(error(existence_error(predicate_declaration, Head), Obj::clause(Head, Body), Sender))
		)
	).

'$lgt_clause_chk'(Obj, Head, Body, Sender, _) :-
	throw(error(existence_error(object, Obj), Obj::clause(Head, Body), Sender)).



% '$lgt_retract'(+object_identifier, @clause, +object_identifier, +scope)
%
% retract/1 built-in method

'$lgt_retract'(Obj, Clause, Sender, _) :-
	var(Clause),
	throw(error(instantiation_error, Obj::retract(Clause), Sender)).

'$lgt_retract'(Obj, (Head:-Body), Sender, _) :-
	var(Head),
	throw(error(instantiation_error, Obj::retract((Head:-Body)), Sender)).

'$lgt_retract'(Obj, (Head:-Body), Sender, _) :-
	\+ callable(Head),
	throw(error(type_error(callable, Head), Obj::retract((Head:-Body)), Sender)).

'$lgt_retract'(Obj, (Head:-Body), Sender, _) :-
	nonvar(Body),
	\+ callable(Body),
	throw(error(type_error(callable, Body), Obj::retract((Head:-Body)), Sender)).

'$lgt_retract'(Obj, Clause, Sender, Scope) :-
	(	Clause = (Head :- Body) ->
		(	var(Body) ->
			'$lgt_retract_var_body_chk'(Obj, Clause, Sender, Scope)
		;	Body == true ->
			'$lgt_retract_fact_chk'(Obj, Head, Sender, Scope)
		;	'$lgt_retract_rule_chk'(Obj, Clause, Sender, Scope)
		)
	;	'$lgt_retract_fact_chk'(Obj, Clause, Sender, Scope)
	).


'$lgt_retract_var_body_chk'(Obj, (Head:-Body), Sender, Scope) :-
	'$lgt_current_object_'(Obj, _, Dcl, Def, _, _, _, _, DDef, _, ObjFlags),
	!,
	(	call(Dcl, Head, PScope, _, PredFlags, SCtn, _) ->
		(	(PredFlags /\ 2 =:= 2; ObjFlags /\ 2 =:= 2, Sender = SCtn) ->
			(	(\+ \+ PScope = Scope; Sender = SCtn) ->
				(	call(DDef, Head, _, THead) ->
					retract((THead :- TBody)),
					(	TBody = ('$lgt_nop'(Body), _) ->
						true
					;	TBody = '$lgt_debugger.fact'(_, _, _) ->
						Body = true
					;	TBody = Body
					),
					'$lgt_update_ddef_table'(DDef, Head, THead)
				;	call(Def, Head, _, THead) ->
					retract((THead :- TBody)),
					(	TBody = ('$lgt_nop'(Body), _) ->
						true
					;	TBody = '$lgt_debugger.fact'(_, _, _) ->
						Body = true
					;	TBody = Body
					)
				)
			;	% predicate is not within the scope of the sender:
				(	PScope == p ->
					throw(error(permission_error(modify, private_predicate, Head), Obj::retract((Head:-Body)), Sender))
				;	throw(error(permission_error(modify, protected_predicate, Head), Obj::retract((Head:-Body)), Sender))
				)
			)
		;	% predicate is static:
			throw(error(permission_error(modify, static_predicate, Head), Obj::retract((Head:-Body)), Sender))
		)
	;	% local dynamic predicate with no scope declaration:
		(	Obj = Sender,
			call(DDef, Head, _, THead) ->
			retract((THead :- TBody)),
			(	TBody = ('$lgt_nop'(Body), _) ->
				true
			;	TBody = '$lgt_debugger.fact'(_, _, _) ->
				Body = true
			;	TBody = Body
			)
		;	throw(error(existence_error(predicate_declaration, Head), Obj::retract((Head:-Body)), Sender))
		)
	).

'$lgt_retract_var_body_chk'(Obj, (Head:-Body), Sender, _) :-
	throw(error(existence_error(object, Obj), Obj::retract((Head:-Body)), Sender)).


'$lgt_retract_rule_chk'(Obj, (Head:-Body), Sender, Scope) :-
	'$lgt_current_object_'(Obj, _, Dcl, Def, _, _, _, _, DDef, _, ObjFlags),
	!,
	(	call(Dcl, Head, PScope, _, PredFlags, SCtn, _) ->
		(	(PredFlags /\ 2 =:= 2; ObjFlags /\ 2 =:= 2, Sender = SCtn) ->
			(	(\+ \+ PScope = Scope; Sender = SCtn) ->
				(	call(DDef, Head, _, THead) ->
					retract((THead :- ('$lgt_nop'(Body), _))),
					'$lgt_update_ddef_table'(DDef, Head, THead)
				;	call(Def, Head, _, THead) ->
					retract((THead :- ('$lgt_nop'(Body), _)))
				)
			;	% predicate is not within the scope of the sender:
				(	PScope == p ->
					throw(error(permission_error(modify, private_predicate, Head), Obj::retract((Head:-Body)), Sender))
				;	throw(error(permission_error(modify, protected_predicate, Head), Obj::retract((Head:-Body)), Sender))
				)
			)
		;	% predicate is static:
			throw(error(permission_error(modify, static_predicate, Head), Obj::retract((Head:-Body)), Sender))
		)
	;	% local dynamic predicate with no scope declaration:
		(	Obj = Sender,
			call(DDef, Head, _, THead) ->
			retract((THead :- ('$lgt_nop'(Body), _)))
		;	throw(error(existence_error(predicate_declaration, Head), Obj::retract((Head:-Body)), Sender))
		)
	).

'$lgt_retract_rule_chk'(Obj, (Head:-Body), Sender, _) :-
	throw(error(existence_error(object, Obj), Obj::retract((Head:-Body)), Sender)).


'$lgt_retract_fact_chk'(Obj, Head, Sender, _) :-
	'$lgt_db_lookup_cache_'(Obj, Head, Sender, THead, UClause),
	!,
	retract(THead),
	'$lgt_update_ddef_table_opt'(UClause).

'$lgt_retract_fact_chk'(Obj, Head, Sender, Scope) :-
	'$lgt_current_object_'(Obj, _, Dcl, Def, _, _, _, _, DDef, _, ObjFlags),
	!,
	(	call(Dcl, Head, PScope, _, PredFlags, SCtn, _) ->
		(	(PredFlags /\ 2 =:= 2; ObjFlags /\ 2 =:= 2, Sender = SCtn) ->
			Type = (dynamic),
			(	(\+ \+ PScope = Scope; Sender = SCtn) ->
				(	call(DDef, Head, _, THead) ->
					(	'$lgt_debugging_entity_'(Obj) ->
						retract((THead :- '$lgt_debugger.fact'(_, _, _)))
					;	'$lgt_add_db_lookup_cache_entry'(Obj, Head, SCtn, PScope, Type, Sender, THead, DDef, true),
						retract(THead)
					),
					'$lgt_update_ddef_table'(DDef, Head, THead)
				;	call(Def, Head, _, THead) ->
					(	'$lgt_debugging_entity_'(Obj) ->
						retract((THead :- '$lgt_debugger.fact'(_, _, _)))
					;	'$lgt_add_db_lookup_cache_entry'(Obj, Head, PScope, Type, Sender, THead),
						retract(THead)
					)
				)
			;	% predicate is not within the scope of the sender:
				(	PScope == p ->
					throw(error(permission_error(modify, private_predicate, Head), Obj::retract(Head), Sender))
				;	throw(error(permission_error(modify, protected_predicate, Head), Obj::retract(Head), Sender))
				)
			)
		;	% predicate is static:
			throw(error(permission_error(modify, static_predicate, Head), Obj::retract(Head), Sender))
		)
	;	% local dynamic predicate with no scope declaration:
		(	call(DDef, Head, _, THead) ->
			(	'$lgt_debugging_entity_'(Obj) ->
				retract((THead :- '$lgt_debugger.fact'(_, _, _)))
			;	'$lgt_add_db_lookup_cache_entry'(Obj, Head, p, (dynamic), Sender, THead),
				retract(THead)
			)
		;	throw(error(existence_error(predicate_declaration, Head), Obj::retract(Head), Sender))
		)
	).

'$lgt_retract_fact_chk'(Obj, Head, Sender, _) :-
	throw(error(existence_error(object, Obj), Obj::retract(Head), Sender)).



% '$lgt_retractall'(+object_identifier, @callable, +object_identifier, +scope)
%
% retractall/1 built-in method

'$lgt_retractall'(Obj, Head, Sender, Scope) :-
	'$lgt_must_be'(callable, Head, Obj::retractall(Head), Sender),
	'$lgt_retractall_chk'(Obj, Head, Sender, Scope).


'$lgt_retractall_chk'(Obj, Head, Sender, _) :-
	'$lgt_db_lookup_cache_'(Obj, Head, Sender, THead, UClause),
	!,
	retractall(THead),
	'$lgt_update_ddef_table_opt'(UClause).

'$lgt_retractall_chk'(Obj, Head, Sender, Scope) :-
	'$lgt_current_object_'(Obj, _, Dcl, Def, _, _, _, _, DDef, _, ObjFlags),
	!,
	(	call(Dcl, Head, PScope, _, PredFlags, SCtn, _) ->
		(	(PredFlags /\ 2 =:= 2; ObjFlags /\ 2 =:= 2, Sender = SCtn) ->
			Type = (dynamic),
			(	(\+ \+ PScope = Scope; Sender = SCtn) ->
				(	call(DDef, Head, _, THead) ->
					retractall(THead),
					'$lgt_update_ddef_table'(DDef, Head, THead)
				;	call(Def, Head, _, THead) ->
					(	'$lgt_debugging_entity_'(Obj) ->
						true
					;	'$lgt_add_db_lookup_cache_entry'(Obj, Head, PScope, Type, Sender, THead)
					),
					retractall(THead)
				;	true
				)
			;	% predicate is not within the scope of the sender:
				(	PScope == p ->
					throw(error(permission_error(modify, private_predicate, Head), Obj::retractall(Head), Sender))
				;	throw(error(permission_error(modify, protected_predicate, Head), Obj::retractall(Head), Sender))
				)
			)
		;	% predicate is static:
			throw(error(permission_error(modify, static_predicate, Head), Obj::retractall(Head), Sender))
		)
	;	% local dynamic predicate with no scope declaration:
		(	Obj = Sender,
			call(DDef, Head, _, THead) ->
			(	'$lgt_debugging_entity_'(Obj) ->
				true
			;	'$lgt_add_db_lookup_cache_entry'(Obj, Head, p, (dynamic), Sender, THead)
			),
			retractall(THead)
		;	throw(error(existence_error(predicate_declaration, Head), Obj::retractall(Head), Sender))
		)
	).

'$lgt_retractall_chk'(Obj, Head, Sender, _) :-
	throw(error(existence_error(object, Obj), Obj::retractall(Head), Sender)).



% '$lgt_nop'(+goal)
%
% used in the implementation of the built-in method
% clause/2 to store the original clause body

'$lgt_nop'(_).



% '$lgt_add_db_lookup_cache_entry'(@object_identifier, @callable, @callable, +atom, @object_identifier, @callable)
%
% adds a new database lookup cache entry (when an update goal is not needed)

'$lgt_add_db_lookup_cache_entry'(Obj, Head, Scope, Type, Sender, THead) :-
	'$lgt_entity_template'(Obj, GObj),
	'$lgt_term_template'(Head, GHead),
	'$lgt_term_template'(THead, GTHead),
	'$lgt_unify_head_thead_args'(GHead, GTHead), 
	(	(Scope = p(p(p)), Type == (dynamic)) ->
		asserta('$lgt_db_lookup_cache_'(GObj, GHead, _, GTHead, true))
	;	'$lgt_entity_template'(Sender, GSender),
		asserta('$lgt_db_lookup_cache_'(GObj, GHead, GSender, GTHead, true))
	).



% '$lgt_add_db_lookup_cache_entry'(@object_identifier, @callable, @callable, @callable, +atom, @object_identifier, @callable, +atom, +atom)
%
% adds a new database lookup cache entry

'$lgt_add_db_lookup_cache_entry'(Obj, Head, SCtn, Scope, Type, Sender, THead, DDef, NeedsUpdate) :-
	'$lgt_entity_template'(Obj, GObj),
	'$lgt_term_template'(Head, GHead),
	'$lgt_term_template'(THead, GTHead),
	'$lgt_unify_head_thead_args'(GHead, GTHead),
	(	NeedsUpdate == true, Sender \= SCtn ->
		'$lgt_term_template'(Head, UHead),
		'$lgt_term_template'(THead, UTHead),
		functor(UClause, DDef, 3),
		arg(1, UClause, UHead),
		arg(3, UClause, UTHead),
		(	(Scope = p(p(p)), Type == (dynamic)) ->
			asserta('$lgt_db_lookup_cache_'(GObj, GHead, _, GTHead, UClause))
		;	'$lgt_entity_template'(Sender, GSender),
			asserta('$lgt_db_lookup_cache_'(GObj, GHead, GSender, GTHead, UClause))
		)
	;	(	(Scope = p(p(p)), Type == (dynamic)) ->
			asserta('$lgt_db_lookup_cache_'(GObj, GHead, _, GTHead, true))
		;	'$lgt_entity_template'(Sender, GSender),
			asserta('$lgt_db_lookup_cache_'(GObj, GHead, GSender, GTHead, true))
		)
	).


% translated clause heads use an extra argument for passing the execution context

'$lgt_unify_head_thead_args'(Head, THead) :-
	functor(Head, _, Arity),
	'$lgt_unify_head_thead_args'(Arity, Head, THead).


'$lgt_unify_head_thead_args'(0, _, _) :-
	!.

'$lgt_unify_head_thead_args'(N, Head, THead) :-
	arg(N, Head, Arg),
	arg(N, THead, Arg),
	M is N - 1,
	'$lgt_unify_head_thead_args'(M, Head, THead).



% '$lgt_phrase'(+grbody, ?list, +execution_context)
%
% phrase/2 built-in method

'$lgt_phrase'(GRBody, Input, ExCtx) :-
	'$lgt_exec_ctx_this'(ExCtx, This),
	catch(
		'$lgt_phrase'(GRBody, Input, [], ExCtx),
		error(Error, phrase(GRBody, Input, []), This),
		throw(error(Error, phrase(GRBody, Input), This))).



% '$lgt_phrase'(+grbody, ?list, ?list, +execution_context)
%
% phrase/3 built-in method

'$lgt_phrase'(GRBody, Input, Rest, ExCtx) :-
	var(GRBody),
	'$lgt_exec_ctx_this'(ExCtx, This),
	throw(error(instantiation_error, phrase(GRBody, Input, Rest), This)).

'$lgt_phrase'(GRBody, Input, Rest, ExCtx) :-
	\+ callable(GRBody),
	'$lgt_exec_ctx_this'(ExCtx, This),
	throw(error(type_error(callable, GRBody), phrase(GRBody, Input, Rest), This)).

'$lgt_phrase'(GRBody, Input, Rest, ExCtx) :-
	nonvar(Input),
	\+ '$lgt_is_list'(Input),
	'$lgt_exec_ctx_this'(ExCtx, This),
	throw(error(type_error(list, Input), phrase(GRBody, Input, Rest), This)).

'$lgt_phrase'(GRBody, Input, Rest, ExCtx) :-
	nonvar(Rest),
	\+ '$lgt_is_list'(Rest),
	'$lgt_exec_ctx_this'(ExCtx, This),
	throw(error(type_error(list, Rest), phrase(GRBody, Input, Rest), This)).

'$lgt_phrase'(GRBody, Input, Rest, ExCtx) :-
	'$lgt_dcg_body'(GRBody, S0, S, Pred),
	'$lgt_exec_ctx'(ExCtx, Sender, This, Self, _, _),
	'$lgt_current_object_'(This, Prefix, _, _, _, _, _, _, _, _, _),
	'$lgt_comp_ctx'(Ctx, _, Sender, This, Self, Prefix, [], _, ExCtx, runtime, _),
	'$lgt_tr_body'(Pred, TPred, DPred, Ctx),
	Input = S0, Rest = S,
	(	'$lgt_debugger.debugging_', '$lgt_debugging_entity_'(This) ->
		call(DPred)
	;	call(TPred)
	).



% '$lgt_expand_term'(+object_identifier, ?term, ?term, +object_identifier, @scope)
%
% expand_term/2 built-in method

'$lgt_expand_term'(Obj, Term, Expansion, Sender, Scope) :-
	(	var(Term) ->
		Expansion = Term
	;	'$lgt_term_expansion'(Obj, Term, Expand, Sender, Scope) ->
		Expansion = Expand
	;	Term = (_ --> _) ->
		'$lgt_dcg_rule_to_clause'(Term, Clause),
		Expansion = Clause
	;	Expansion = Term
	).



% '$lgt_term_expansion'(+object_identifier, ?term, ?term, +object_identifier, @scope)
%
% calls the term_expansion/2 user-defined predicate
%
% if there is a scope directive, then the call fails if the sender is not within scope;
% when there is no scope directive, then we call any local definition when the sender
% and the target object are the same

'$lgt_term_expansion'(Obj, Term, Expansion, Sender, Scope) :-
	'$lgt_current_object_'(Obj, _, Dcl, Def, _, _, _, _, DDef, _, _),
	(	call(Dcl, term_expansion(_, _), PScope, _, _, SCtn, _) ->
		(	(\+ \+ PScope = Scope; Sender = SCtn) ->
			'$lgt_exec_ctx'(ExCtx, Sender, Obj, Obj, _, _),
			call(Def, term_expansion(Term, Expansion), ExCtx, Call, _)
		;	fail
		)
	;	Obj = Sender,
		'$lgt_exec_ctx'(ExCtx, Obj, Obj, Obj, _, _),
		(	call(Def, term_expansion(Term, Expansion), ExCtx, Call) ->
			true
		;	call(DDef, term_expansion(Term, Expansion), ExCtx, Call)
		)
	),
	!,
	once(Call).



% '$lgt_expand_goal'(+object_identifier, ?term, ?term, +object_identifier, @scope)
%
% expand_goal/2 built-in method

'$lgt_expand_goal'(Obj, Goal, EGoal, Sender, Scope) :-
	(	var(Goal) ->
		EGoal = Goal
	;	'$lgt_goal_expansion'(Obj, Goal, Expanded, Sender, Scope) ->
		'$lgt_expand_goal'(Obj, Expanded, EGoal, Sender, Scope)
	;	EGoal = Goal
	). 



% '$lgt_goal_expansion'(+object_identifier, ?term, ?term, +object_identifier, @scope)
%
% calls the goal_expansion/2 user-defined predicate
%
% if there is a scope directive, then the call fails if the sender is not within scope;
% when there is no scope directive, then we call any local definition when the sender
% and the target object are the same

'$lgt_goal_expansion'(Obj, Goal, Expansion, Sender, Scope) :-
	'$lgt_current_object_'(Obj, _, Dcl, Def, _, _, _, _, DDef, _, _),
	(	call(Dcl, goal_expansion(_, _), PScope, _, _, SCtn, _) ->
		(	(\+ \+ PScope = Scope; Sender = SCtn) ->
			'$lgt_exec_ctx'(ExCtx, Sender, Obj, Obj, _, _),
			call(Def, goal_expansion(Goal, Expansion), ExCtx, Call, _)
		;	fail
		)
	;	Obj = Sender,
		'$lgt_exec_ctx'(ExCtx, Obj, Obj, Obj, _, _),
		(	call(Def, goal_expansion(Goal, Expansion), ExCtx, Call) ->
			true
		;	call(DDef, goal_expansion(Goal, Expansion), ExCtx, Call)
		)
	),
	!,
	once(Call).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  message sending
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% '$lgt_send_to_self'(+object_identifier, ?term, +object_identifier)
%
% runtime processing of a message sending call when the arguments are not
% known at compile time

'$lgt_send_to_self'(Obj, Pred, Sender) :-
	'$lgt_must_be'(callable, Pred, ::Pred, Sender),
	'$lgt_send_to_self_'(Obj, Pred, Sender).



% '$lgt_send_to_self_'(+object_identifier, +term, +object_identifier)
%
% the last clause of this cache predicate must always exist and must
% call the predicate that generates the missing cache entry

'$lgt_send_to_self_'(Obj, Pred, Sender) :-
	'$lgt_send_to_self_nv'(Obj, Pred, Sender).



% '$lgt_send_to_self_nv'(+object_identifier, +term, +object_identifier)
%
% runtime processing of a message sending call when the arguments have already
% been type-checked; generates a cache entry to speed up future calls

'$lgt_send_to_self_nv'(Obj, Pred, Sender) :-
	'$lgt_current_object_'(Obj, _, Dcl, Def, _, _, _, _, _, _, _),
	(	call(Dcl, Pred, Scope, Meta, _, SCtn, _) ->									% lookup declaration
		(	(Scope = p(_); Sender = SCtn) ->										% check scope
			(	'$lgt_term_template'(Pred, GPred),									% construct predicate template
				'$lgt_entity_template'(Obj, GObj),									% construct object template
				'$lgt_entity_template'(Sender, GSender),							% construct "sender" template
				'$lgt_pred_meta_vars'(GPred, Meta, GMetaVars),						% construct list of the meta-variables
				'$lgt_exec_ctx'(ExCtx, GSender, GObj, GObj, GMetaVars, []),			% that will be called in the "sender"
				call(Def, GPred, ExCtx, GCall, _) ->								% lookup definition
				asserta(('$lgt_send_to_self_'(GObj, GPred, GSender) :- !, GCall)),	% cache lookup result
				GObj = Obj, GPred = Pred, GSender = Sender,							% unify message arguments
				call(GCall)
			;	% closed-world assumption
				fail
			)
		;	% message is not within the scope of the sender:
			throw(error(permission_error(access, private_predicate, Pred), ::Pred, Sender))
		)
	;	% no predicate declaration, check if it's a private built-in method or a Prolog built-in meta-predicate:
		('$lgt_built_in_method'(Pred, _, _, _); '$lgt_pl_meta_predicate'(Pred, _, _)) ->
		throw(error(permission_error(access, private_predicate, Pred), ::Pred, Sender))
	;	% no predicate declaration, check if it's a built-in predicate:
		'$lgt_built_in'(Pred) ->
		call(Pred)
	;	throw(error(existence_error(predicate_declaration, Pred), ::Pred, Sender))
	).



% '$lgt_send_to_obj'(@object_identifier, ?term, +object_identifier)
%
% runtime processing of an event-aware message sending call when the arguments
% are not known at compile time

'$lgt_send_to_obj'(Obj, Pred, Sender) :-
	'$lgt_must_be'(object_identifier, Obj, Obj::Pred, Sender),
	'$lgt_must_be'(callable, Pred, Obj::Pred, Sender),
	'$lgt_send_to_obj_'(Obj, Pred, Sender).



% '$lgt_send_to_obj_'(+object_identifier, +term, +object_identifier)
%
% the last clause of this cache predicate must always exist and must
% call the predicate that generates the missing cache entry

'$lgt_send_to_obj_'(Obj, Pred, Sender) :-
	'$lgt_send_to_obj_nv'(Obj, Pred, Sender).



% '$lgt_send_to_obj_nv'(+object_identifier, +term, +object_identifier)
%
% runtime processing of an event-aware message sending call when the arguments
% have already been type-checked; generates a cache entry to speed up future calls

'$lgt_send_to_obj_nv'(Obj, Pred, Sender) :-
	'$lgt_current_object_'(Obj, _, Dcl, Def, _, _, _, _, _, _, _),
	!,
	(	call(Dcl, Pred, Scope, Meta, _, SCtn, _) ->										% lookup declaration
		(	Scope = p(p(_)) ->															% check public scope
			(	'$lgt_term_template'(Pred, GPred),										% construct predicate template
				'$lgt_entity_template'(Obj, GObj),										% construct object template
				'$lgt_pred_meta_vars'(GPred, Meta, GMetaVars),							% construct list of the meta-variables
				'$lgt_exec_ctx'(ExCtx, GSender, GObj, GObj, GMetaVars, []),				% that will be called in the "sender"
				call(Def, GPred, ExCtx, GCall, _) ->									% lookup definition
				GGCall = '$lgt_guarded_method_call'(GObj, GPred, GSender, GCall),
				asserta(('$lgt_send_to_obj_'(GObj, GPred, GSender) :- !, GGCall)),		% cache lookup result
				GObj = Obj, GPred = Pred, GSender = Sender,								% unify message arguments
				'$lgt_guarded_method_call'(Obj, Pred, Sender, GCall)					% call method
			;	% closed-world assumption
				fail
			)
		;	Sender = SCtn ->															% check scope container
			(	'$lgt_term_template'(Pred, GPred),										% construct predicate template
				'$lgt_entity_template'(Obj, GObj),										% construct object template
				'$lgt_entity_template'(Sender, GSender),								% construct "sender" template
				'$lgt_exec_ctx'(ExCtx, GSender, GObj, GObj, _, []),
				call(Def, GPred, ExCtx, GCall, _) ->									% lookup definition
				GGCall = '$lgt_guarded_method_call'(GObj, GPred, GSender, GCall),
				asserta(('$lgt_send_to_obj_'(GObj, GPred, GSender) :- !, GGCall)),		% cache lookup result
				GObj = Obj, GPred = Pred, GSender = Sender,								% unify message arguments
				'$lgt_guarded_method_call'(Obj, Pred, Sender, GCall)					% call method
			;	% closed-world assumption
				fail
			)
		;	% message is not within the scope of the sender:
			(	Scope == p ->
				throw(error(permission_error(access, private_predicate, Pred), Obj::Pred, Sender))
			;	throw(error(permission_error(access, protected_predicate, Pred), Obj::Pred, Sender))
			)
		)
	;	% no predicate declaration, check if it's a private built-in method or a Prolog built-in meta-predicate:
		('$lgt_built_in_method'(Pred, _, _, _); '$lgt_pl_meta_predicate'(Pred, _, _)) ->
		throw(error(permission_error(access, private_predicate, Pred), Obj::Pred, Sender))
	;	% no predicate declaration, check if it's a built-in predicate:
		'$lgt_built_in'(Pred) ->
		call(Pred)
	;	throw(error(existence_error(predicate_declaration, Pred), Obj::Pred, Sender))
	).

'$lgt_send_to_obj_nv'({Proxy}, Pred, Sender) :-
	!,
	catch(Proxy, error(Error, _), throw(error(Error, {Proxy}::Pred, Sender))),
	'$lgt_send_to_obj_'(Proxy, Pred, Sender).

'$lgt_send_to_obj_nv'(Obj, Pred, _) :-		% allow Obj::Pred to be used as a shortcut
	atom(Obj),
	catch(current_module(Obj), _, fail),	% for calling module predicates
	!,
	':'(Obj, Pred).

'$lgt_send_to_obj_nv'(Obj, Pred, Sender) :-
	throw(error(existence_error(object, Obj), Obj::Pred, Sender)).



% '$lgt_guarded_method_call'(+object_identifier, +callable, +object_identifier, +callable)
%
% wraps the method call with the before and after event handler calls; the "before" event handler
% may prevent a method from being executed by failing and an "after" event handler may prevent a
% method from succeeding by failing; however, event handlers cannot modify the method call

'$lgt_guarded_method_call'(Obj, Msg, Sender, Method) :-
	\+ ('$lgt_before_event_'(Obj, Msg, Sender, _, Before), \+ Before),	% call before event handlers
	call(Method),														% call method
	\+ ('$lgt_after_event_'(Obj, Msg, Sender, _, After), \+ After).		% call after event handlers



% '$lgt_send_to_obj_ne'(@object_identifier, ?term, +object_identifier)
%
% runtime processing of an event-transparent message sending call when the arguments
% are not known at compile time

'$lgt_send_to_obj_ne'(Obj, Pred, Sender) :-
	'$lgt_must_be'(object_identifier, Obj, Obj::Pred, Sender),
	'$lgt_must_be'(callable, Pred, Obj::Pred, Sender),
	'$lgt_send_to_obj_ne_'(Obj, Pred, Sender).



% '$lgt_send_to_obj_ne_'(+object_identifier, +term, +object_identifier)
%
% the last clause of this cache predicate must always exist and must
% call the predicate that generates the missing cache entry

'$lgt_send_to_obj_ne_'(Obj, Pred, Sender) :-
	'$lgt_send_to_obj_ne_nv'(Obj, Pred, Sender).



% '$lgt_send_to_obj_ne_nv'(+object_identifier, +term, +object_identifier)
%
% runtime processing of an event-transparent message sending call when the arguments
% have already been type-checked; generates a cache entry to speed up future calls

'$lgt_send_to_obj_ne_nv'(Obj, Pred, Sender) :-
	'$lgt_current_object_'(Obj, _, Dcl, Def, _, _, _, _, _, _, _),
	!,
	(	call(Dcl, Pred, Scope, Meta, _, SCtn, _) ->										% lookup declaration
		(	Scope = p(p(_)) ->															% check public scope
			(	'$lgt_term_template'(Pred, GPred),										% construct predicate template
				'$lgt_entity_template'(Obj, GObj),										% construct object template
				'$lgt_pred_meta_vars'(GPred, Meta, GMetaVars),							% construct list of the meta-variables
				'$lgt_exec_ctx'(ExCtx, GSender, GObj, GObj, GMetaVars, []),				% that will be called in the "sender"
				call(Def, GPred, ExCtx, GCall, _) ->									% lookup definition
				asserta(('$lgt_send_to_obj_ne_'(GObj, GPred, GSender) :- !, GCall)),	% cache lookup result
				GObj = Obj, GPred = Pred, GSender = Sender,								% unify message arguments
				call(GCall)																% call method
			;	% closed-world assumption
				fail
			)
		;	Sender = SCtn ->															% check scope container
			(	'$lgt_term_template'(Pred, GPred),										% construct predicate template
				'$lgt_entity_template'(Obj, GObj),										% construct object template
				'$lgt_entity_template'(Sender, GSender),								% construct "sender" template
				'$lgt_exec_ctx'(ExCtx, GSender, GObj, GObj, _, []),
				call(Def, GPred, ExCtx, GCall, _) ->									% lookup definition
				asserta(('$lgt_send_to_obj_ne_'(GObj, GPred, GSender) :- !, GCall)),	% cache lookup result
				GObj = Obj, GPred = Pred, GSender = Sender,								% unify message arguments
				call(GCall)																% call method
			;	% closed-world assumption
				fail
			)
		;	% message is not within the scope of the sender:
			(	Scope == p ->
				throw(error(permission_error(access, private_predicate, Pred), Obj::Pred, Sender))
			;	throw(error(permission_error(access, protected_predicate, Pred), Obj::Pred, Sender))
			)
		)
	;	% no predicate declaration, check if it's a private built-in method or a Prolog built-in meta-predicate:
		('$lgt_built_in_method'(Pred, _, _, _); '$lgt_pl_meta_predicate'(Pred, _, _)) ->
		throw(error(permission_error(access, private_predicate, Pred), Obj::Pred, Sender))
	;	% no predicate declaration, check if it's a built-in predicate:
		'$lgt_built_in'(Pred) ->
		call(Pred)
	;	throw(error(existence_error(predicate_declaration, Pred), Obj::Pred, Sender))
	).

'$lgt_send_to_obj_ne_nv'({Proxy}, Pred, Sender) :-
	!,
	catch(Proxy, error(Error, _), throw(error(Error, {Proxy}::Pred, Sender))),
	'$lgt_send_to_obj_ne_'(Proxy, Pred, Sender).

'$lgt_send_to_obj_ne_nv'(Obj, Pred, _) :-	% allow Obj::Pred to be used as a shortcut
	atom(Obj),
	catch(current_module(Obj), _, fail),	% for calling module predicates
	!,
	':'(Obj, Pred).

'$lgt_send_to_obj_ne_nv'(Obj, Pred, Sender) :-
	throw(error(existence_error(object, Obj), Obj::Pred, Sender)).



% '$lgt_obj_super_call_same_'(+object_identifier, +term, +object_identifier)
%
% the last clause of this cache predicate must always exist and must
% call the predicate that generates the missing cache entry

'$lgt_obj_super_call_same_'(Super, Pred, ExCtx) :-
	'$lgt_obj_super_call_same'(Super, Pred, ExCtx).



% '$lgt_obj_super_call_same'(+atom, +callable, +execution_context)
%
% runtime processing of an object "super" call when the arguments have already
% been type-checked; generates a cache entry to speed up future calls

'$lgt_obj_super_call_same'(Super, Pred, ExCtx) :-
	(	'$lgt_exec_ctx'(ExCtx, _, This, Self, _, _),
		'$lgt_term_template'(Pred, GPred),											% construct predicate template
		'$lgt_entity_template'(This, GThis),										% construct "this" template
		'$lgt_entity_template'(Self, GSelf),										% construct "self" template
		'$lgt_exec_ctx'(GExCtx, _, GThis, GSelf, _, _),
		call(Super, GPred, GExCtx, GCall, Ctn), Ctn \= GThis ->						% lookup definition
		asserta(('$lgt_obj_super_call_same_'(Super, GPred, GExCtx) :- !, GCall)),	% cache lookup result
		GPred = Pred, GExCtx = ExCtx,												% unify message arguments
		call(GCall)																	% call inherited definition
	;	% closed-world assumption
		fail
	).



% '$lgt_ctg_super_call_same_'(+object_identifier, +term, +object_identifier)
%
% the last clause of this cache predicate must always exist and must
% call the predicate that generates the missing cache entry

'$lgt_ctg_super_call_same_'(Ctg, Pred, ExCtx) :-
	'$lgt_ctg_super_call_same'(Ctg, Pred, ExCtx).



% '$lgt_ctg_super_call_same'(+category_identifier, +callable, +execution_context)
%
% runtime processing of a category "super" call when the arguments have already
% been type-checked; generates a cache entry to speed up future calls

'$lgt_ctg_super_call_same'(Ctg, Pred, ExCtx) :-
	(	'$lgt_current_category_'(Ctg, _, _, Def, _, _),
		'$lgt_term_template'(Pred, GPred),											% construct predicate template
		call(Def, GPred, GExCtx, GCall, Ctn), Ctn \= Ctg ->							% lookup definition
		asserta(('$lgt_ctg_super_call_same_'(Ctg, GPred, GExCtx) :- !, GCall)),		% cache lookup result
		GPred = Pred, GExCtx = ExCtx,												% unify message arguments
		call(GCall)																	% call inherited definition
	;	% closed-world assumption
		fail
	).



% '$lgt_obj_super_call_other'(+atom, +callable, +execution_context)
%
% runtime processing of an object "super" call when the arguments are not
% known at compile time

'$lgt_obj_super_call_other'(Super, Pred, ExCtx) :-
	(	var(Pred) ->
		'$lgt_exec_ctx_this'(ExCtx, This),
		throw(error(instantiation_error, ^^Pred, This))
	;	callable(Pred) ->
		'$lgt_obj_super_call_other_'(Super, Pred, ExCtx)
	;	'$lgt_exec_ctx_this'(ExCtx, This),
		throw(error(type_error(callable, Pred), ^^Pred, This))
	).



% '$lgt_obj_super_call_other_'(+object_identifier, +term, +object_identifier)
%
% the last clause of this cache predicate must always exist and must
% call the predicate that generates the missing cache entry

'$lgt_obj_super_call_other_'(Super, Pred, ExCtx) :-
	'$lgt_obj_super_call_other_nv'(Super, Pred, ExCtx).



% '$lgt_obj_super_call_other_nv'(+atom, +callable, +execution_context)
%
% runtime processing of an object "super" call when the arguments have already
% been type-checked; generates a cache entry to speed up future calls

'$lgt_obj_super_call_other_nv'(Super, Pred, ExCtx) :-
	'$lgt_exec_ctx'(ExCtx, _, This, Self, _, _),
	(	'$lgt_current_object_'(Self, _, Dcl, _, _, _, _, _, _, _, _),
		call(Dcl, Pred, Scope, _, _, SCtn, _) ->
		(	(Scope = p(_); This = SCtn) ->													% check scope
			(	'$lgt_term_template'(Pred, GPred),											% construct predicate template
				'$lgt_entity_template'(This, GThis),										% construct "this" template
				'$lgt_entity_template'(Self, GSelf),										% construct "self" template
				'$lgt_exec_ctx'(GExCtx, _, GThis, GSelf, _, _),
				call(Super, GPred, GExCtx, GCall, Ctn), Ctn \= GThis ->						% lookup definition
				asserta(('$lgt_obj_super_call_other_'(Super, GPred, GExCtx) :- !, GCall)),	% cache lookup result
				GPred = Pred, GExCtx = ExCtx,												% unify message arguments
				call(GCall)																	% call inherited definition
			;	% closed-world assumption
				fail
			)
		;	% predicate is not within the scope of the sender:
			throw(error(permission_error(access, private_predicate, Pred), ^^Pred, This))
		)
	;	% no predicate declaration, check if it's a private built-in method or a Prolog built-in meta-predicate:
		('$lgt_built_in_method'(Pred, _, _, _); '$lgt_pl_meta_predicate'(Pred, _, _)) ->
		throw(error(permission_error(access, private_predicate, Pred), ^^Pred, This))
	;	% no predicate declaration, check if it's a built-in predicate:
		'$lgt_built_in'(Pred) ->
		call(Pred)
	;	throw(error(existence_error(predicate_declaration, Pred), ^^Pred, This))
	).



% '$lgt_ctg_super_call_other'(+category_identifier, +callable, +execution_context)
%
% runtime processing of a category "super" call when the arguments are not
% known at compile time

'$lgt_ctg_super_call_other'(Ctg, Pred, ExCtx) :-
	(	var(Pred) ->
		throw(error(instantiation_error, ^^Pred, Ctg))
	;	callable(Pred) ->
		'$lgt_ctg_super_call_other_'(Ctg, Pred, ExCtx)
	;	throw(error(type_error(callable, Pred), ^^Pred, Ctg))
	).



% '$lgt_ctg_super_call_other_'(+object_identifier, +term, +object_identifier)
%
% the last clause of this cache predicate must always exist and must
% call the predicate that generates the missing cache entry

'$lgt_ctg_super_call_other_'(Ctg, Pred, ExCtx) :-
	'$lgt_ctg_super_call_other_nv'(Ctg, Pred, ExCtx).



% '$lgt_ctg_super_call_other_nv'(+category_identifier, +callable, +execution_context)
%
% runtime processing of a category "super" call when the arguments have already
% been type-checked; generates a cache entry to speed up future calls

'$lgt_ctg_super_call_other_nv'(Ctg, Pred, ExCtx) :-
	(	'$lgt_current_category_'(Ctg, _, Dcl, Def, _, _),
		call(Dcl, Pred, Scope, _, _, _) ->
		(	Scope = p(_) ->																	% check scope
			(	'$lgt_term_template'(Pred, GPred),											% construct predicate template
				call(Def, GPred, GExCtx, GCall, Ctn), Ctn \= Ctg ->							% lookup definition
				asserta(('$lgt_ctg_super_call_other_'(Ctg, GPred, GExCtx) :- !, GCall)),	% cache lookup result
				GPred = Pred, GExCtx = ExCtx,												% unify message arguments
				call(GCall)																	% call inherited definition
			;	% closed-world assumption
				fail
			)
		;	% predicate is not within the scope of the sender:
			throw(error(permission_error(access, private_predicate, Pred), ^^Pred, Ctg))
		)
	;	% no predicate declaration, check if it's a private built-in method or a Prolog built-in meta-predicate:
		('$lgt_built_in_method'(Pred, _, _, _); '$lgt_pl_meta_predicate'(Pred, _, _)) ->
		throw(error(permission_error(access, private_predicate, Pred), ^^Pred, Ctg))
	;	% no predicate declaration, check if it's a built-in predicate:
		'$lgt_built_in'(Pred) ->
		call(Pred)
	;	throw(error(existence_error(predicate_declaration, Pred), ^^Pred, Ctg))
	).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  meta-calls
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% '$lgt_lambda'(+curly_bracketed_term, @callable)
%
% calls a lambda-call with free variables but no parameters (Free/Goal) where the
% arguments are already cheked and compiled; typically used in bagof/3 and setof/3
% as an alternative to the enumeration of all existentially quantified variables

'$lgt_lambda'(Free, Goal) :-
	'$lgt_copy_term_without_constraints'(Free/Goal, Free/GoalCopy),
	call(GoalCopy).



% '$lgt_metacall'(?term, +list, @term, +object_identifier, +object_identifier, +object_identifier)
%
% performs a meta-call constructed from a closure and a list of additional arguments

'$lgt_metacall'(Closure, ExtraArgs, MetaCallCtx, Sender, This, _) :-
	var(Closure),
	Call =.. [call, Closure| ExtraArgs],
	(	atom(MetaCallCtx) ->
		throw(error(instantiation_error, This::Call, This))
	;	throw(error(instantiation_error, Sender::Call, This))
	).

'$lgt_metacall'({Closure}, ExtraArgs, _, _, This, _) :-		% pre-compiled closures or calls
	!,														% in "user" (compiler bypass)
	(	atom(Closure) ->
		Goal =.. [Closure| ExtraArgs],
		call(Goal)
	;	compound(Closure) ->
		Closure =.. [Functor| Args],
		'$lgt_append'(Args, ExtraArgs, FullArgs),
		Goal =.. [Functor| FullArgs],
		call(Goal)
	;	var(Closure) ->
		Call =.. [call, {Closure}| ExtraArgs],
		throw(error(instantiation_error, Call, This))
	;	Call =.. [call, {Closure}| ExtraArgs],
		throw(error(type_error(callable, Closure), Call, This))
	).

'$lgt_metacall'(::Closure, ExtraArgs, _, _, This, Self) :-
	!,
	(	var(Closure) ->
		Call =.. [call, ::Closure| ExtraArgs],
		throw(error(instantiation_error, Call, This))
	;	callable(Closure) ->
		Closure =.. [Functor| Args],
		'$lgt_append'(Args, ExtraArgs, FullArgs),
		Goal =.. [Functor| FullArgs],
		'$lgt_send_to_self_'(Self, Goal, This)
	;	Call =.. [call, ::Closure| ExtraArgs],
		throw(error(type_error(callable, Closure), Call, This))
	).

'$lgt_metacall'(Obj::Closure, ExtraArgs, _, _, This, _) :-
	!,
	(	var(Obj) ->
		Call =.. [call, Obj::Closure| ExtraArgs],
		throw(error(instantiation_error, Call, This))
	;	var(Closure) ->
		Call =.. [call, Obj::Closure| ExtraArgs],
		throw(error(instantiation_error, Call, This))
	;	callable(Obj), callable(Closure) ->
		Closure =.. [Functor| Args],
		'$lgt_append'(Args, ExtraArgs, FullArgs),
		Goal =.. [Functor| FullArgs],
		(	'$lgt_current_object_'(This, _, _, _, _, _, _, _, _, _, Flags), Flags /\ 16 =:= 16 ->
			'$lgt_send_to_obj_'(Obj, Goal, This)
		;	'$lgt_send_to_obj_ne_'(Obj, Goal, This)
		)
	;	Call =.. [call, Obj::Closure| ExtraArgs],
		throw(error(type_error(callable, Closure), Call, This))
	).

'$lgt_metacall'(Obj<<Closure, ExtraArgs, _, _, This, _) :-
	!,
	(	var(Obj) ->
		Call =.. [call, Obj<<Closure| ExtraArgs],
		throw(error(instantiation_error, Call, This))
	;	var(Closure) ->
		Call =.. [call, Obj<<Closure| ExtraArgs],
		throw(error(instantiation_error, Call, This))
	;	callable(Obj), callable(Closure) ->
		Closure =.. [Functor| Args],
		'$lgt_append'(Args, ExtraArgs, FullArgs),
		Goal =.. [Functor| FullArgs],
		'$lgt_call_within_context_nv'(Obj, Goal, This)
	;	Call =.. [call, Obj<<Closure| ExtraArgs],
		throw(error(type_error(callable, Closure), Call, This))
	).

'$lgt_metacall'(':'(Module, Closure), ExtraArgs, _, _, This, _) :-
	!,
	(	var(Module) ->
		Call =.. [call, ':'(Module, Closure)| ExtraArgs],
		throw(error(instantiation_error, Call, This))
	;	var(Closure) ->
		Call =.. [call, ':'(Module, Closure)| ExtraArgs],
		throw(error(instantiation_error, Call, This))
	;	atom(Module) ->
		(	callable(Closure) ->
			Closure =.. [Functor| Args],
			'$lgt_append'(Args, ExtraArgs, FullArgs),
			Goal =.. [Functor| FullArgs],
			':'(Module, Goal)
		;	Call =.. [call, ':'(Module, Closure)| ExtraArgs],
			throw(error(type_error(callable, Closure), Call, This))
		)
	;	Call =.. [call, ':'(Module, Closure)| ExtraArgs],
		throw(error(type_error(atom, Module), Call, This))
	).

'$lgt_metacall'(Free/Closure, ExtraArgs, LambdaMetaCallCtx, Sender, This, Self) :-
	!,
	(	var(Free) ->
	 	throw(error(instantiation_error, Free/Closure, This))
	;	\+ (functor(Free, {}, Arity), Arity =< 1) ->
		throw(error(type_error(curly_bracketed_term, Free), Free/Closure, This))
	;	var(Closure) ->
	 	throw(error(instantiation_error, Free/Closure, This))
	;	'$lgt_reduce_lambda_metacall_ctx'(LambdaMetaCallCtx, Free/Closure, MetaCallCtx),
		'$lgt_copy_term_without_constraints'(Free/Closure+MetaCallCtx, Free/ClosureCopy+MetaCallCtxCopy),
		'$lgt_metacall'(ClosureCopy, ExtraArgs, MetaCallCtxCopy, Sender, This, Self)
	).

'$lgt_metacall'(Free/Parameters>>Closure, ExtraArgs, LambdaMetaCallCtx, Sender, This, Self) :-
	!,
	(	var(Free) ->
	 	throw(error(instantiation_error, Free/Parameters>>Closure, This))
	;	\+ (functor(Free, {}, Arity), Arity =< 1) ->
		throw(error(type_error(curly_bracketed_term, Free), Free/Parameters>>Closure, This))
	;	var(Closure) ->
	 	throw(error(instantiation_error, Free/Parameters>>Closure, This))
	;	'$lgt_reduce_lambda_metacall_ctx'(LambdaMetaCallCtx, Free/Parameters>>Closure, MetaCallCtx),
		'$lgt_copy_term_without_constraints'(Free/Parameters>>Closure+MetaCallCtx, Free/ParametersCopy>>ClosureCopy+MetaCallCtxCopy),
		'$lgt_unify_lambda_parameters'(ParametersCopy, ExtraArgs, Rest, Free/Parameters>>Closure, This) ->
		'$lgt_metacall'(ClosureCopy, Rest, MetaCallCtxCopy, Sender, This, Self)
	;	throw(error(representation_error(lambda_parameters), Free/Parameters>>Closure, This))
	).

'$lgt_metacall'(Parameters>>Closure, ExtraArgs, LambdaMetaCallCtx, Sender, This, Self) :-
	!,
	(	var(Closure) ->
	 	throw(error(instantiation_error, Parameters>>Closure, This))
	;	'$lgt_reduce_lambda_metacall_ctx'(LambdaMetaCallCtx, Parameters>>Closure, MetaCallCtx),
		'$lgt_copy_term_without_constraints'(Parameters>>Closure+MetaCallCtx, ParametersCopy>>ClosureCopy+MetaCallCtxCopy),
		'$lgt_unify_lambda_parameters'(ParametersCopy, ExtraArgs, Rest, Parameters>>Closure, This) ->
		'$lgt_metacall'(ClosureCopy, Rest, MetaCallCtxCopy, Sender, This, Self)
	;	throw(error(representation_error(lambda_parameters), Parameters>>Closure, This))
	).

'$lgt_metacall'(Closure, ExtraArgs, MetaCallCtx, Sender, This, _) :-
	\+ callable(Closure),
	Call =.. [call, Closure| ExtraArgs],
	(	atom(MetaCallCtx) ->
		throw(error(type_error(callable, Closure), This::Call, This))
	;	throw(error(type_error(callable, Closure), Sender::Call, This))
	).

'$lgt_metacall'(Closure, ExtraArgs, MetaCallCtx, Sender, This, Self) :-
	(	atom(Closure) ->
		Goal =.. [Closure| ExtraArgs]
	;	Closure =.. [Functor| Args],
		'$lgt_append'(Args, ExtraArgs, FullArgs),
		Goal =.. [Functor| FullArgs]
	),
	(	\+ '$lgt_member'(Closure, MetaCallCtx) ->
		'$lgt_metacall_this'(Goal, Sender, This, Self)
	;	'$lgt_metacall_sender'(Goal, Sender, This, Self, ExtraArgs)
	).


'$lgt_unify_lambda_parameters'(-, _, _, Lambda, This) :-	% catch variables and lists with unbound tails 
	(Lambda = _/Parameters>>_; Lambda = Parameters>>_),
	throw(error(type_error(list, Parameters), Lambda, This)).

'$lgt_unify_lambda_parameters'([], Vars, Vars, _, _).

'$lgt_unify_lambda_parameters'([Parameter| Parameters], [Parameter| Vars], Rest, Lambda, This) :-
	'$lgt_unify_lambda_parameters'(Parameters, Vars, Rest, Lambda, This).


% when using currying, the "inner" lambda expressions must be executed in the same context as the "outer"
% lambda expressions; the same for the "inner" closure; this forces the update of the meta-call context

'$lgt_reduce_lambda_metacall_ctx'(-, _, _).

'$lgt_reduce_lambda_metacall_ctx'([], _, []).

'$lgt_reduce_lambda_metacall_ctx'([Meta| Metas], Lambda, Reduced) :-
	'$lgt_reduce_lambda_metacall_ctx'(Meta, Metas, Lambda, Reduced).


'$lgt_reduce_lambda_metacall_ctx'(Free/Closure, Metas, Free/Closure, [Closure| Metas]) :- !.

'$lgt_reduce_lambda_metacall_ctx'(Parameters>>Closure, Metas, Parameters>>Closure, [Closure| Metas]) :- !.

'$lgt_reduce_lambda_metacall_ctx'(Meta, Metas, Lambda, [Meta| Reduced]) :-
	'$lgt_reduce_lambda_metacall_ctx'(Metas, Lambda, Reduced).



% '$lgt_metacall'(?term, @term, +object_identifier, +object_identifier, +object_identifier)
%
% performs a meta-call at runtime

'$lgt_metacall'(Goal, MetaCallCtx, Sender, This, _) :-
	var(Goal),
	(	atom(MetaCallCtx) ->
		throw(error(instantiation_error, call(This::Goal), This))
	;	throw(error(instantiation_error, call(Sender::Goal), This))
	).

'$lgt_metacall'(Goal, MetaCallCtx, Sender, This, _) :-
	\+ callable(Goal),
	(	atom(MetaCallCtx) ->
		throw(error(type_error(callable, Goal), call(This::Goal), This))
	;	throw(error(type_error(callable, Goal), call(Sender::Goal), This))
	).

'$lgt_metacall'({Goal}, _, _, _, _) :-						% pre-compiled meta-calls or calls
	!,														% in "user" (compiler bypass)
	call(Goal).

'$lgt_metacall'(':'(Module, Goal), _, _, _, _) :-
	!,
	':'(Module, Goal).

'$lgt_metacall'(Goal, MetaCallCtx, Sender, This, Self) :-
	(	\+ '$lgt_member'(Goal, MetaCallCtx) ->
		'$lgt_metacall_this'(Goal, Sender, This, Self)
	;	'$lgt_metacall_sender'(Goal, Sender, This, Self, [])
	).



% '$lgt_metacall_this'(+callable, +object_identifier, +object_identifier, +object_identifier)
%
% performs a meta-call in "this" at runtime

'$lgt_metacall_this'(Pred, Sender, This, Self) :-
	'$lgt_current_object_'(This, Prefix, _, Def, _, _, _, _, DDef, _, _),
	'$lgt_exec_ctx'(ExCtx, Sender, This, Self, [], _),
	(	% in the most common case we're meta-calling a user defined static predicate:
		call(Def, Pred, ExCtx, TPred) ->
		call(TPred)
	;	% or a user defined dynamic predicate:
		call(DDef, Pred, ExCtx, TPred) ->
		call(TPred)
	;	% in the worst case we need to compile the meta-call:
		'$lgt_comp_ctx'(Ctx, _, Sender, This, Self, Prefix, [], _, ExCtx, runtime, _),
		'$lgt_tr_body'(Pred, TPred, DPred, Ctx) ->
		(	'$lgt_debugger.debugging_', '$lgt_debugging_entity_'(This) ->
			call(DPred)
		;	call(TPred)
		)
	;	% of course, the meta-call may happen to be an unfortunate mistake:
		functor(Pred, Functor, Arity),
		throw(error(existence_error(procedure, Functor/Arity), call(Pred), This))
	).



% '$lgt_metacall_sender'(+callable, +object_identifier, +object_identifier, +object_identifier, +list)
%
% performs a meta-call in "sender" at runtime

'$lgt_metacall_sender'(Pred, Sender, This, Self, ExtraVars) :-
	'$lgt_current_object_'(Sender, Prefix, _, Def, _, _, _, _, DDef, _, _),
	'$lgt_exec_ctx'(ExCtx, Sender, Sender, Self, ExtraVars, _),
	(	% in the most common case we're meta-calling a user defined static predicate:
		call(Def, Pred, ExCtx, TPred) ->
		call(TPred)
	;	% or a user defined dynamic predicate:
		call(DDef, Pred, ExCtx, TPred) ->
		call(TPred)
	;	% in the worst case we have a control construct or a built-in predicate:
		'$lgt_comp_ctx'(Ctx, _, This, Sender, Self, Prefix, ExtraVars, _, _, runtime, _),
		'$lgt_tr_body'(Pred, TPred, DPred, Ctx) ->
		(	'$lgt_debugger.debugging_', '$lgt_debugging_entity_'(Sender) ->
			call(DPred)
		;	call(TPred)
		)
	;	% of course, the meta-call may happen to be an unfortunate mistake:
		functor(Pred, Functor, Arity),
		throw(error(existence_error(procedure, Functor/Arity), call(Pred), Sender))
	).



% '$lgt_call_built_in'(+callable, +callable, +execution_context)
%
% necessary for runtime translation of dynamic clauses, for dealing
% with meta-calls that turn out to be calls to built-in predicates,
% and for dealing with <</2 calls to redefined built-in predicates
%
% the first argument, Pred, is the original predicate call, while the second
% argument, MetaExPred, is equal to the first argument for normal predicates
% but is meta-argument expanded for non-redefined built-in meta-predicates

'$lgt_call_built_in'(Pred, MetaExPred, ExCtx) :-
	'$lgt_exec_ctx_this'(ExCtx, This),
	'$lgt_current_object_'(This, _, _, Def, _, _, _, _, DDef, _, _),
	(	call(Def, Pred, ExCtx, TPred) ->
		% call a static redefinition of a built-in predicate:
		call(TPred)
	;	call(DDef, Pred, ExCtx, TPred) ->
		% call a dynamic redefinition of a built-in predicate:
		call(TPred)
	;	% no redefinition; call the built-in predicate:
		call(MetaExPred)
	).



% '$lgt_call_within_context'(?term, ?term, +object_identifier)
%
% calls a goal within the context of the specified object; used mostly for
% debugging and for writing unit tests

'$lgt_call_within_context'(Obj, Goal, This) :-
	'$lgt_must_be'(object_identifier, Obj, Obj<<Goal, This),
	'$lgt_must_be'(callable, Goal, Obj<<Goal, This),
	'$lgt_call_within_context_nv'(Obj, Goal, This).



% '$lgt_call_within_context_nv'(+object_identifier, +callable, +object_identifier)
%
% calls a goal within the context of the specified object; used mostly for
% debugging and for writing unit tests

'$lgt_call_within_context_nv'(Obj, Goal, This) :-
	(	'$lgt_current_object_'(Obj, Prefix, _, Def, _, _, _, _, DDef, _, Flags) ->
		(	Flags /\ 128 =:= 128 ->			
			'$lgt_exec_ctx'(ExCtx, Obj, Obj, Obj, [], _),
			(	% in the most common case we're calling a user defined static predicate:
				call(Def, Goal, ExCtx, TGoal) ->
				call(TGoal)
				% or a user defined dynamic predicate:
			;	call(DDef, Goal, ExCtx, TGoal) ->
				call(TGoal)
			;	% in the worst case we need to compile the goal:
				'$lgt_comp_ctx'(Ctx, _, Obj, Obj, Obj, Prefix, [], _, ExCtx, runtime, _),
				catch('$lgt_tr_body'(Goal, TGoal, DGoal, Ctx), Error, throw(error(Error, Obj<<Goal, This))),
				(	'$lgt_debugger.debugging_', '$lgt_debugging_entity_'(Obj) ->
					call(DGoal)
				;	call(TGoal)
				)
			)
		;	throw(error(permission_error(access, predicate, Goal), Obj<<Goal, This))
		)
	;	throw(error(existence_error(object, Obj), Obj<<Goal, This))
	).



% '$lgt_ctg_call'(+atom, ?term, +execution_context)
%
% calls a category predicate directly, without using the message sending mechanism

'$lgt_ctg_call'(Dcl, Pred, ExCtx) :-
	(	var(Pred) ->
		'$lgt_exec_ctx_this'(ExCtx, This),
		throw(error(instantiation_error, ':'(Pred), This))
	;	callable(Pred) ->
		'$lgt_ctg_call_'(Dcl, Pred, ExCtx)
	;	'$lgt_exec_ctx_this'(ExCtx, This),
		throw(error(type_error(callable, Pred), ':'(Pred), This))
	).



% '$lgt_ctg_call_'(+object_identifier, +term, +object_identifier)
%
% the last clause of this cache predicate must always exist and must
% call the predicate that generates the missing cache entry

'$lgt_ctg_call_'(Dcl, Pred, ExCtx) :-
	'$lgt_call_ctg_pred_nv'(Dcl, Pred, ExCtx).



% '$lgt_call_ctg_pred_nv'(+atom, +callable, +execution_context)
%
% calls a category predicate directly, without using the message sending mechanism

'$lgt_call_ctg_pred_nv'(Dcl, Alias, ExCtx) :-
	'$lgt_exec_ctx'(ExCtx, _, This, Self, _, _),
	(	'$lgt_current_object_'(This, _, _, _, _, _, _, _, _, Rnm, _),
		call(Dcl, Alias, _, _, _, _, _) ->
		(	'$lgt_term_template'(Alias, GAlias),							% construct predicate template
			'$lgt_entity_template'(This, GThis),							% construct "this" template
			'$lgt_entity_template'(Self, GSelf),							% construct "self" template
			call(Rnm, Ctg, GPred, GAlias),
			'$lgt_imports_category_'(GThis, Ctg, _),
			'$lgt_current_category_'(Ctg, _, _, Def, _, _),
			'$lgt_exec_ctx'(GExCtx, _, GThis, GSelf, _, _),
			call(Def, GPred, GExCtx, GCall, _) ->
			asserta(('$lgt_ctg_call_'(Dcl, GAlias, GExCtx) :- !, GCall)),	% cache lookup result
			GAlias = Alias, GExCtx = ExCtx,									% unify message arguments
			call(GCall)														% call inherited definition
		;	% closed-world assumption
			fail
		)
	;	% no predicate declaration, check if it's a built-in predicate:
		'$lgt_built_in'(Alias) ->
		call(Alias)
	;	throw(error(existence_error(predicate_declaration, Alias), ':'(Alias), This))
	).



% '$lgt_call_this'(+callable, +execution_context)
%
% calls a dynamic predicate in "this" from within a category at runtime

'$lgt_call_this'(Pred, ExCtx) :-
	'$lgt_exec_ctx_this'(ExCtx, This),
	'$lgt_current_object_'(This, _, _, Def, _, _, _, _, DDef, _, _),
	(	% the object definition may include some initial clauses for the dynamic predicate:
		call(Def, Pred, ExCtx, TPred) ->
		call(TPred)
	;	% or the clauses for the dynamic predicate may be defined only at runtime:
		call(DDef, Pred, ExCtx, TPred) ->
		call(TPred)
	;	% closed-world assumption:
		fail
	).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  support for categories that complement objects
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% lookup predicate declarations in any category that complements the given object

'$lgt_complemented_object'(This, ThisDcl, Alias, Scope, Meta, Flags, SCtn, TCtn) :-
	'$lgt_complemented_object_'(This, _, Dcl, _, Rnm),
	(	call(Dcl, Alias, Scope, Meta, Flags, TCtn),
		SCtn = This
	;	% categories can define aliases for complemented object predicates:
		call(Rnm, This, Pred, Alias),
		Pred \= Alias,
		call(ThisDcl, Pred, Scope, Meta, Flags, SCtn, TCtn)
	).



% lookup predicate definitions in any category that complements the given object

'$lgt_complemented_object'(ThisDef, Alias, ExCtx, Call, Ctn) :-
	'$lgt_exec_ctx_this'(ExCtx, This),
	'$lgt_complemented_object_'(This, _, _, Def, Rnm),
	(	call(Def, Alias, ExCtx, Call, Ctn)
	;	% categories can define aliases for complemented object predicates:
		call(Rnm, This, Pred, Alias),
		Pred \= Alias,
		call(ThisDef, Pred, ExCtx, Call, Ctn)
	).	




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  built-in entity tables
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



'$lgt_built_in_object'(logtalk).
'$lgt_built_in_object'(user).
'$lgt_built_in_object'(debugger).


'$lgt_built_in_protocol'(expanding).
'$lgt_built_in_protocol'(monitoring).


'$lgt_built_in_category'(_) :-
	fail.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  built-in entity runtime table clauses and properties
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



'$lgt_current_object_'(logtalk, '$lgt_logtalk.', '$lgt_logtalk._dcl', '$lgt_logtalk._def', '$lgt_logtalk._super', '$lgt_logtalk._idcl', '$lgt_logtalk._idef', '$lgt_logtalk._ddcl', '$lgt_logtalk._ddef', '$lgt_logtalk._alias', Flags) :-
	(	'$lgt_compiler_flag'(threads, supported) ->
		Flags = 249		% 0b11111001
	;	Flags = 241		% 0b11110001
	).
'$lgt_current_object_'(user, '$lgt_user.', '$lgt_user._dcl', '$lgt_user._def', '$lgt_user._super', '$lgt_user._idcl', '$lgt_user._idef', '$lgt_user._ddcl', '$lgt_user._ddef', '$lgt_user._alias', Flags) :-
	(	'$lgt_compiler_flag'(threads, supported) ->
		Flags = 249		% 0b11111001
	;	Flags = 241		% 0b11110001
	).
'$lgt_current_object_'(debugger, '$lgt_debugger.', '$lgt_debugger._dcl', '$lgt_debugger._def', '$lgt_debugger._super', '$lgt_debugger._idcl', '$lgt_debugger._idef', '$lgt_debugger._ddcl', '$lgt_debugger._ddef', '$lgt_debugger._alias', Flags) :-
	(	'$lgt_compiler_flag'(threads, supported) ->
		Flags = 249		% 0b11111001
	;	Flags = 241		% 0b11110001
	).


'$lgt_current_protocol_'(expanding, '$lgt_expanding.', '$lgt_expanding._dcl', '$lgt_expanding._alias', 1).		% 0b00000001
'$lgt_current_protocol_'(monitoring, '$lgt_monitoring.', '$lgt_monitoring._dcl', '$lgt_monitoring._alias', 1).	% 0b00000001


'$lgt_implements_protocol_'(logtalk, expanding, (public)).
'$lgt_implements_protocol_'(logtalk, monitoring, (public)).


'$lgt_static_binding_entity_'(logtalk).
'$lgt_static_binding_entity_'(debugger).
'$lgt_static_binding_entity_'(expanding).
'$lgt_static_binding_entity_'(monitoring).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  "logtalk" built-in object
%
%  defines Logtalk hook predicates
%
%  its clauses correspond to a virtual compilation of the object using a
%  "code_prefix" flag set to '$lgt_'
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- dynamic('$lgt_logtalk._ddcl'/2).
:- dynamic('$lgt_logtalk._ddef'/3).


'$lgt_logtalk._dcl'(expand_library_path(_, _), p(p(p)), no, 0).
'$lgt_logtalk._dcl'(loaded_file(_, _), p(p(p)), no, 0).
'$lgt_logtalk._dcl'(compile_aux_clauses(_), p(p(p)), no, 0).
'$lgt_logtalk._dcl'(entity_prefix(_, _), p(p(p)), no, 0).
'$lgt_logtalk._dcl'(compile_predicate_heads(_, _), p(p(p)), no, 0).
'$lgt_logtalk._dcl'(compile_predicate_heads(_, _, _), p(p(p)), no, 0).
'$lgt_logtalk._dcl'(compile_predicate_heads(_, _, _, _), p(p(p)), no, 0).
'$lgt_logtalk._dcl'(compile_predicate_indicators(_, _), p(p(p)), no, 0).
'$lgt_logtalk._dcl'(compile_predicate_indicators(_, _, _), p(p(p)), no, 0).
'$lgt_logtalk._dcl'(decompile_predicate_heads(_, _), p(p(p)), no, 0).
'$lgt_logtalk._dcl'(decompile_predicate_heads(_, _, _), p(p(p)), no, 0).
'$lgt_logtalk._dcl'(decompile_predicate_heads(_, _, _, _), p(p(p)), no, 0).
'$lgt_logtalk._dcl'(decompile_predicate_indicators(_, _), p(p(p)), no, 0).
'$lgt_logtalk._dcl'(decompile_predicate_indicators(_, _, _), p(p(p)), no, 0).
'$lgt_logtalk._dcl'(decompile_predicate_indicators(_, _, _, _), p(p(p)), no, 0).
% deprecated
'$lgt_logtalk._dcl'(decompile_predicate_head(_, _, _, _), p(p(p)), no, 0).
'$lgt_logtalk._dcl'(decompile_predicate_indicator(_, _, _, _), p(p(p)), no, 0).


'$lgt_logtalk._dcl'(Pred, Scope, Meta, Flags, logtalk, logtalk) :-
	'$lgt_logtalk._dcl'(Pred, Scope, Meta, Flags).

'$lgt_logtalk._dcl'(Pred, Scope, no, 2, logtalk, logtalk) :-
	'$lgt_logtalk._ddcl'(Pred, Scope).

'$lgt_logtalk._dcl'(Pred, Scope, Meta, Flags, logtalk, Ctn) :-
	'$lgt_expanding._dcl'(Pred, Scope, Meta, Flags, Ctn).

'$lgt_logtalk._dcl'(Pred, Scope, Meta, Flags, logtalk, Ctn) :-
	'$lgt_monitoring._dcl'(Pred, Scope, Meta, Flags, Ctn).


'$lgt_logtalk._def'(expand_library_path(Library, Path), _, '$lgt_expand_library_path'(Library, Path)).
'$lgt_logtalk._def'(loaded_file(File, Directory), _, '$lgt_loaded_file_'(File, Directory)).
'$lgt_logtalk._def'(compile_aux_clauses(Clauses), _, '$lgt_compile_aux_clauses'(Clauses)).
'$lgt_logtalk._def'(entity_prefix(Entity, Prefix), _, '$lgt_entity_prefix'(Entity, Prefix)).
'$lgt_logtalk._def'(compile_predicate_heads(Heads, THeads), _, '$lgt_compile_predicate_heads'(Heads, THeads)).
'$lgt_logtalk._def'(compile_predicate_heads(Heads, THeads, Ctx), _, '$lgt_compile_predicate_heads'(Heads, THeads, Ctx)).
'$lgt_logtalk._def'(compile_predicate_heads(Heads, Entity, THeads, Ctx), _, '$lgt_compile_predicate_heads'(Heads, Entity, THeads, Ctx)).
'$lgt_logtalk._def'(compile_predicate_indicators(PIs, TPIs), _, '$lgt_compile_predicate_indicators'(PIs, TPIs)).
'$lgt_logtalk._def'(compile_predicate_indicators(PIs, Entity, TPIs), _, '$lgt_compile_predicate_indicators'(PIs, Entity, TPIs)).
'$lgt_logtalk._def'(decompile_predicate_indicators(TPIs, PIs), _, '$lgt_decompile_predicate_indicators'(TPIs, PIs)).
'$lgt_logtalk._def'(decompile_predicate_indicators(TPIs, Entity, PIs), _, '$lgt_decompile_predicate_indicators'(TPIs, Entity, PIs)).
'$lgt_logtalk._def'(decompile_predicate_indicators(TPIs, Entity, Type, PIs), _, '$lgt_decompile_predicate_indicators'(TPIs, Entity, Type, PIs)).
'$lgt_logtalk._def'(decompile_predicate_heads(THeads, Heads), _, '$lgt_decompile_predicate_heads'(THeads, Heads)).
'$lgt_logtalk._def'(decompile_predicate_heads(THeads, Entity, Heads), _, '$lgt_decompile_predicate_heads'(THeads, Entity, Heads)).
'$lgt_logtalk._def'(decompile_predicate_heads(THeads, Entity, Type, Heads), _, '$lgt_decompile_predicate_heads'(THeads, Entity, Type, Heads)).
% deprecated
'$lgt_logtalk._def'(decompile_predicate_indicator(TPI, Entity, Type, PI), _, '$lgt_decompile_predicate_indicator'(TPI, Entity, Type, PI)).
'$lgt_logtalk._def'(decompile_predicate_head(THead, Entity, Type, Head), _, '$lgt_decompile_predicate_head'(THead, Entity, Type, Head)).


'$lgt_logtalk._def'(Pred, ExCtx, Call, logtalk) :-
	'$lgt_logtalk._def'(Pred, ExCtx, Call).

'$lgt_logtalk._def'(Pred, ExCtx, Call, logtalk) :-
	'$lgt_logtalk._ddef'(Pred, ExCtx, Call).


'$lgt_logtalk._super'(_, _, _, _) :-
	fail.


'$lgt_logtalk._alias'(_, Pred, Pred).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  "user" built-in pseudo-object
%
%  represents the Prolog database (excluding built-in predicates)
%
%  the clauses correspond to a virtual compilation of the object using a
%  "code_prefix" flag set to '$lgt_'
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% the following clauses correspond to a virtual compilation of the built-in pseudo-object "user"


:- dynamic('$lgt_user._ddcl'/2).
:- dynamic('$lgt_user._ddef'/3).


'$lgt_user._dcl'(Pred, p(p(p)), no, Flags) :-
	(	nonvar(Pred) ->
		\+ '$lgt_built_in'(Pred),
		functor(Pred, Functor, Arity),
		current_predicate(Functor/Arity)
	;	current_predicate(Functor/Arity),
		functor(Pred, Functor, Arity),
		\+ '$lgt_built_in'(Pred),
		\+ '$lgt_hidden_functor'(Functor)
	),
	(	'$lgt_predicate_property'(Pred, (dynamic)) ->
		Flags = 2
	;	Flags = 0
	).


'$lgt_user._dcl'(Pred, Scope, Meta, Flags, user, user) :-
	'$lgt_user._dcl'(Pred, Scope, Meta, Flags).


'$lgt_user._def'(Pred, _, Pred) :-
	\+ '$lgt_built_in'(Pred),
	functor(Pred, Functor, Arity),
	current_predicate(Functor/Arity).


'$lgt_user._def'(Pred, _, Pred, user) :-
	'$lgt_user._def'(Pred, _, Pred).


'$lgt_user._super'(_, _, _, _) :-
	fail.


'$lgt_user._alias'(_, Pred, Pred).



% '$lgt_hidden_functor'(+atom)
%
% hidden functors include Logtalk pre-processor and runtime internal functors
% and those used in the compiled code of objects, protocols, and categories

'$lgt_hidden_functor'(Functor) :-
	atom_concat('$lgt_', _, Functor),
	!.

'$lgt_hidden_functor'(Functor) :-
	'$lgt_current_category_'(_, Prefix, _, _, _, _),
	atom_concat(Prefix, _, Functor),
	!.

'$lgt_hidden_functor'(Functor) :-
	'$lgt_current_object_'(_, Prefix, _, _, _, _, _, _, _, _, _),
	atom_concat(Prefix, _, Functor),
	!.

'$lgt_hidden_functor'(Functor) :-
	'$lgt_current_protocol_'(_, Prefix, _, _, _),
	atom_concat(Prefix, _, Functor),
	!.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  "debugger" built-in object
%
%  implements the Logtalk buit-in debugging features
%
%  the clauses correspond to a virtual compilation of the object using a
%  "code_prefix" flag set to '$lgt_'
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% the following clauses correspond to a virtual compilation of the built-in object debugger

:- dynamic('$lgt_debugger._ddcl'/2).
:- dynamic('$lgt_debugger._ddef'/3).


% debugger public protocol

'$lgt_debugger._dcl'(reset, p(p(p)), no, 0).

'$lgt_debugger._dcl'(debug, p(p(p)), no, 0).
'$lgt_debugger._dcl'(nodebug, p(p(p)), no, 0).

'$lgt_debugger._dcl'(debugging, p(p(p)), no, 0).
'$lgt_debugger._dcl'(debugging(_), p(p(p)), no, 0).

'$lgt_debugger._dcl'(trace, p(p(p)), no, 0).
'$lgt_debugger._dcl'(notrace, p(p(p)), no, 0).

'$lgt_debugger._dcl'(spy(_), p(p(p)), no, 0).
'$lgt_debugger._dcl'(spy(_, _, _, _), p(p(p)), no, 0).
'$lgt_debugger._dcl'(nospy(_), p(p(p)), no, 0).
'$lgt_debugger._dcl'(nospy(_, _, _, _), p(p(p)), no, 0).
'$lgt_debugger._dcl'(nospyall, p(p(p)), no, 0).

'$lgt_debugger._dcl'(leash(_), p(p(p)), no, 0).


'$lgt_debugger._dcl'(Pred, Scope, Meta, Flags, debugger, debugger) :-
	'$lgt_debugger._dcl'(Pred, Scope, Meta, Flags).

'$lgt_debugger._dcl'(Pred, Scope, no, 2, debugger, debugger) :-
	'$lgt_debugger._ddcl'(Pred, Scope).


'$lgt_debugger._def'(reset, _, '$lgt_debugger.reset').

'$lgt_debugger._def'(debug, _, '$lgt_debugger.debug').
'$lgt_debugger._def'(nodebug, _, '$lgt_debugger.nodebug').

'$lgt_debugger._def'(debugging, _, '$lgt_debugger.debugging').
'$lgt_debugger._def'(debugging(Entity), _, '$lgt_debugger.debugging'(Entity)).

'$lgt_debugger._def'(trace, _, '$lgt_debugger.trace').
'$lgt_debugger._def'(notrace, _, '$lgt_debugger.notrace').

'$lgt_debugger._def'(spy(Preds), _, '$lgt_debugger.spy'(Preds)).
'$lgt_debugger._def'(nospy(Preds), _, '$lgt_debugger.nospy'(Preds)).
'$lgt_debugger._def'(spy(Sender, This, Self, Goal), _, '$lgt_debugger.spy'(Sender, This, Self, Goal)).
'$lgt_debugger._def'(nospy(Sender, This, Self, Goal), _, '$lgt_debugger.nospy'(Sender, This, Self, Goal)).
'$lgt_debugger._def'(nospyall, _, '$lgt_debugger.nospyall').

'$lgt_debugger._def'(leash(Ports), _, '$lgt_debugger.leash'(Ports)).


'$lgt_debugger._def'(Pred, ExCtx, Call, debugger) :-
	'$lgt_debugger._def'(Pred, ExCtx, Call).

'$lgt_debugger._def'(Pred, ExCtx, Call, debugger) :-
	'$lgt_debugger._ddef'(Pred, ExCtx, Call).


'$lgt_debugger._super'(_, _, _, _) :-
	fail.


'$lgt_debugger._alias'(_, Pred, Pred).


'$lgt_debugger.reset' :-
	'$lgt_debugger.nospyall',
	'$lgt_debugger.leash'(full),
	'$lgt_debugger.nodebug',
	'$lgt_debugger.reset_invocation_number'.


'$lgt_debugger.debug' :-
	(	'$lgt_debugger.debugging_' ->
		write('Debugger is on: showing spy points for all objects compiled in debug mode.'), nl
	;	assertz('$lgt_debugger.debugging_'),
		retractall('$lgt_debugger.tracing_'),
		'$lgt_debugger.reset_invocation_number',
		write('Debugger switched on: showing spy points for all objects compiled in debug mode.'), nl
	).


'$lgt_debugger.nodebug' :-
	(	'$lgt_debugger.debugging_' ->
		retractall('$lgt_debugger.debugging_'),
		retractall('$lgt_debugger.tracing_'),
		write('Debugger switched off.'), nl
	;	write('Debugger is off.'), nl
	).


'$lgt_debugger.suspend'(Tracing) :-
	(	'$lgt_debugger.tracing_' ->
		Tracing = true
	;	Tracing = false
	),
	retractall('$lgt_debugger.debugging_'),
	retractall('$lgt_debugger.tracing_').


'$lgt_debugger.resume'(Tracing) :-
	(	Tracing == true ->
		retractall('$lgt_debugger.tracing_'),
		assertz('$lgt_debugger.tracing_')
	;	true
	),
	retractall('$lgt_debugger.debugging_'),
	assertz('$lgt_debugger.debugging_').


'$lgt_debugger.trace' :-
	(	'$lgt_debugger.tracing_' ->
		write('Debugger is on: tracing everything for all objects compiled in debug mode.'), nl
	;	assertz('$lgt_debugger.tracing_'),
		retractall('$lgt_debugger.debugging_'),
		assertz('$lgt_debugger.debugging_'),
		'$lgt_debugger.reset_invocation_number',
		write('Debugger switched on: tracing everything for all objects compiled in debug mode.'), nl
	).


'$lgt_debugger.notrace' :-
	(	'$lgt_debugger.tracing_' ->
		retractall('$lgt_debugger.tracing_'),
		retractall('$lgt_debugger.debugging_'),
		write('Debugger switched off.'), nl
	;	write('Debugger is off.'), nl
	).


'$lgt_debugger.debugging' :-
	(	'$lgt_debugger.debugging_' ->
		write('Debugger is on: '),
		(	'$lgt_debugger.tracing_' ->
			write('tracing everything.'), nl
		;	write('showing spy points.'), nl
		)
	;	write('Debugger is off.'), nl
	), nl,
	(	'$lgt_debugger.spying_'(_, _) ->
		write('Defined predicate spy points (Functor/Arity):'), nl,
		forall(
			'$lgt_debugger.spying_'(Functor, Arity),
			(write('    '), writeq(Functor), write('/'), write(Arity), nl))
	;	write('No predicate spy points are defined.'), nl
	), nl,
	(	'$lgt_debugger.spying_'(_, _, _, _) ->
		write('Defined context spy points (Sender, This, Self, Goal):'), nl,
		forall(
			'$lgt_debugger.spying_'(Sender, This, Self, Goal),
			(write('    '), '$lgt_debugger.pretty_print_spypoint'(Sender, This, Self, Goal), nl))
	;	write('No context spy points are defined.'), nl
	), nl,
	write('Leashed ports:'), nl, write('    '),
	(	'$lgt_debugger.leashing_'(_) ->
		forall('$lgt_debugger.leashing_'(Port), (write(Port), write(' ')))
	;	write('(none)')
	),
	nl.


'$lgt_debugger.debugging'(Entity) :-
	'$lgt_debugging_entity_'(Entity).


'$lgt_debugger.pretty_print_spypoint'(Sender, This, Self, Goal) :-
	current_output(Output),
	(	var(Sender) -> write('_, ')
	;	'$lgt_pretty_print_vars_quoted'(Output, Sender), write(', ')
	),
	(	var(This) -> write('_, ')
	;	'$lgt_pretty_print_vars_quoted'(Output, This), write(', ')
	),
	(	var(Self) -> write('_, ')
	;	'$lgt_pretty_print_vars_quoted'(Output, Self), write(', ')
	),
	(	var(Goal) -> write('_')
	;	'$lgt_pretty_print_vars_quoted'(Output, Goal)
	).


'$lgt_debugger.spy'(Preds) :-
	nonvar(Preds),
	'$lgt_debugger.spy_aux'(Preds),
	write('Predicate spy points set.'), nl,
	(	'$lgt_debugger.debugging_' ->
		true
	;	'$lgt_debugger.debug'
	).


'$lgt_debugger.spy_aux'([]).

'$lgt_debugger.spy_aux'([Functor/Arity| Preds]) :-
	nonvar(Functor),
	nonvar(Arity),
	(	'$lgt_debugger.spying_'(Functor, Arity) ->
		true
	;	assertz('$lgt_debugger.spying_'(Functor, Arity))
	),
	'$lgt_debugger.spy_aux'(Preds).

'$lgt_debugger.spy_aux'(Functor/Arity) :-
	nonvar(Functor),
	nonvar(Arity),
	(	'$lgt_debugger.spying_'(Functor, Arity) ->
		true
	;	assertz('$lgt_debugger.spying_'(Functor, Arity))
	).


'$lgt_debugger.nospy'(Preds) :-
	'$lgt_debugger.nospy_aux'(Preds),
	write('All matching predicate spy points removed.'), nl.


'$lgt_debugger.nospy_aux'(Preds) :-
	(	var(Preds) ->
		retractall('$lgt_debugger.spying_'(_, _))
	;	'$lgt_debugger.nospy_aux2'(Preds)
	).


'$lgt_debugger.nospy_aux2'([]).

'$lgt_debugger.nospy_aux2'([Functor/Arity| Preds]) :-
	retractall('$lgt_debugger.spying_'(Functor, Arity)),
	'$lgt_debugger.nospy_aux2'(Preds).

'$lgt_debugger.nospy_aux2'(Functor/Arity) :-
	retractall('$lgt_debugger.spying_'(Functor, Arity)).


'$lgt_debugger.spy'(Sender, This, Self, Goal) :-
	asserta('$lgt_debugger.spying_'(Sender, This, Self, Goal)),
	write('Context spy point set.'), nl,
	(	'$lgt_debugger.debugging_' ->
		true
	;	'$lgt_debugger.debug'
	).


'$lgt_debugger.nospy'(Sender, This, Self, Goal) :-
	retractall('$lgt_debugger.spying_'(Sender, This, Self, Goal)),
	write('All matching context spy points removed.'), nl.


'$lgt_debugger.nospyall' :-
	retractall('$lgt_debugger.spying_'(_, _)),
	write('All predicate spy points removed.'), nl,
	retractall('$lgt_debugger.spying_'(_, _, _, _)),
	write('All context spy points removed.'), nl.


'$lgt_debugger.leash'(Value) :-
	'$lgt_debugger.valid_leash_value'(Value, Ports),
	retractall('$lgt_debugger.leashing_'(_)),
	'$lgt_debugger.set_leash_ports'(Ports),
	write('Debugger leash ports set to '), write(Ports), nl.

	
'$lgt_debugger.set_leash_ports'([]).

'$lgt_debugger.set_leash_ports'([Port| Ports]) :-
	assertz('$lgt_debugger.leashing_'(Port)),
	'$lgt_debugger.set_leash_ports'(Ports).


'$lgt_debugger.leashing_'(fact).
'$lgt_debugger.leashing_'(rule).
'$lgt_debugger.leashing_'(call).
'$lgt_debugger.leashing_'(exit).
'$lgt_debugger.leashing_'(redo).
'$lgt_debugger.leashing_'(fail).
'$lgt_debugger.leashing_'(exception).


'$lgt_debugger.valid_leash_value'(Shorthand, Ports) :-
	atom(Shorthand),
	Shorthand \== [],
	!,
	'$lgt_debugger.leash_shortand_ports'(Shorthand, Ports).

'$lgt_debugger.valid_leash_value'(Ports, Ports) :-
	nonvar(Ports),
	'$lgt_is_proper_list'(Ports),
	'$lgt_debugger.valid_leash_ports'(Ports).


'$lgt_debugger.valid_leash_ports'([]).

'$lgt_debugger.valid_leash_ports'([Port| Ports]) :-
	nonvar(Port),
	'$lgt_debugger.valid_leash_port'(Port),
	'$lgt_debugger.valid_leash_ports'(Ports).


'$lgt_debugger.valid_leash_port'(fact).
'$lgt_debugger.valid_leash_port'(rule).
'$lgt_debugger.valid_leash_port'(call).
'$lgt_debugger.valid_leash_port'(exit).
'$lgt_debugger.valid_leash_port'(redo).
'$lgt_debugger.valid_leash_port'(fail).
'$lgt_debugger.valid_leash_port'(exception).


'$lgt_debugger.leash_shortand_ports'(none, []).
'$lgt_debugger.leash_shortand_ports'(loose, [fact, rule, call]).
'$lgt_debugger.leash_shortand_ports'(half, [fact, rule, call, redo]).
'$lgt_debugger.leash_shortand_ports'(tight, [fact, rule, call, redo, fail, exception]).
'$lgt_debugger.leash_shortand_ports'(full, [fact, rule, call, exit, redo, fail, exception]).


'$lgt_debugger.leashing'(Port, Goal, ExCtx, Code) :-
	functor(Port, PortName, _),
	'$lgt_debugger.leashing_'(PortName) ->
	(	'$lgt_debugger.tracing_' ->
		Code = ' '
	;	'$lgt_debugger.spying'(Port, Goal, ExCtx, Code),
		(	'$lgt_debugger.tracing_' ->
			true
		;	assertz('$lgt_debugger.tracing_')
		)
	).


'$lgt_debugger.spying'(_, Goal, _, '+') :-
	functor(Goal, Functor, Arity),
	\+ \+ '$lgt_debugger.spying_'(Functor, Arity),
	!.
	
'$lgt_debugger.spying'(_, Goal, ExCtx, '*') :-
	'$lgt_exec_ctx'(ExCtx, Sender, This, Self, _, _),
	\+ \+ '$lgt_debugger.spying_'(Sender, This, Self, Goal).


'$lgt_debugger.fact'(Fact, N, ExCtx) :-
	(	'$lgt_debugger.debugging_', \+ '$lgt_debugger.skipping_' ->
		'$lgt_debugger.port'(fact(N), _, Fact, _, ExCtx, Action),
		call(Action)
	;	true
	).


'$lgt_debugger.head'(Head, N, ExCtx) :-
	(	'$lgt_debugger.debugging_', \+ '$lgt_debugger.skipping_' ->
		'$lgt_debugger.port'(rule(N), _, Head, _, ExCtx, Action),
		call(Action)
	;	true
	).


'$lgt_debugger.goal'(Goal, TGoal, ExCtx) :-
	'$lgt_debugger.inc_invocation_number'(N),
	(	'$lgt_debugger.debugging_', \+ '$lgt_debugger.skipping_' ->
		(	'$lgt_debugger.port'(call, N, Goal, _, ExCtx, CAction),
			(	(CAction == ignore; CAction == unify) ->
				true
			;	call(CAction),
				catch(TGoal, Error, '$lgt_debugger.exception'(N, Goal, Error, ExCtx)),
				(	'$lgt_debugger.port'(exit, N, Goal, _, ExCtx, EAction),
					call(EAction)
				;	'$lgt_debugger.port'(redo, N, Goal, _, ExCtx, RAction),
					RAction == ignore
				)
			;	retractall('$lgt_debugger.skipping_'),
				'$lgt_debugger.port'(fail, N, Goal, _, ExCtx, _),
				fail
			)
		),
		retractall('$lgt_debugger.skipping_')
	;	call(TGoal)
	).


'$lgt_debugger.exception'(_, _, logtalk_debugger_aborted, _) :-
	throw(logtalk_debugger_aborted).

'$lgt_debugger.exception'(N, Goal, Error, ExCtx) :-
	'$lgt_debugger.port'(exception, N, Goal, Error, ExCtx, TAction),
	(	TAction == fail ->
		fail
	;	throw(Error)
	).


'$lgt_debugger.port'(Port, N, Goal, Error, ExCtx, Action) :-
	'$lgt_debugger.debugging_',
	!,
	(	'$lgt_debugger.leashing'(Port, Goal, ExCtx, Code) ->
		repeat,
			write(Code), '$lgt_debugger.write_port_name'(Port), '$lgt_debugger.write_invocation_number'(Port, N), writeq(Goal), write(' ? '),
			catch('$lgt_read_single_char'(Option), _, fail),
		'$lgt_debugger.valid_port_option'(Option, Port, Code),
		'$lgt_debugger.do_port_option'(Option, Port, Goal, Error, ExCtx, Action),
		!
	;	(	'$lgt_debugger.tracing_' ->
			write(' '), '$lgt_debugger.write_port_name'(Port), '$lgt_debugger.write_invocation_number'(Port, N), writeq(Goal), nl
		;	true
		),
		Action = true
	).

'$lgt_debugger.port'(_, _, _, _, _, true).


'$lgt_debugger.write_invocation_number'(fact(_), _) :- !.
'$lgt_debugger.write_invocation_number'(rule(_), _) :- !.
'$lgt_debugger.write_invocation_number'(_, N) :-
	write('('), write(N), write(') ').


'$lgt_debugger.write_port_name'(fact(N)) :-
	(	N =:= 0 ->
		write(' Fact: ')
	;	write(' Fact: (clause #'), write(N), write(') ')
	).
'$lgt_debugger.write_port_name'(rule(N)) :-
	(	N =:= 0 ->
		write(' Rule: ')
	;	write(' Rule: (clause #'), write(N), write(') ')
	).
'$lgt_debugger.write_port_name'(call) :-
	write(' Call: ').
'$lgt_debugger.write_port_name'(exit) :-
	write(' Exit: ').
'$lgt_debugger.write_port_name'(redo) :-
	write(' Redo: ').
'$lgt_debugger.write_port_name'(fail) :-
	write(' Fail: ').
'$lgt_debugger.write_port_name'(exception) :-
	write(' Exception: ').


'$lgt_debugger.valid_port_option'('\r', _, _) :- !.
'$lgt_debugger.valid_port_option'('\n', _, _) :- !.
'$lgt_debugger.valid_port_option'(' ', _, _) :- !.
'$lgt_debugger.valid_port_option'(c, _, _) :- !.
'$lgt_debugger.valid_port_option'(l, _, _) :- !.
'$lgt_debugger.valid_port_option'(s, _, _) :- !.
'$lgt_debugger.valid_port_option'(i, call, _) :- !.
'$lgt_debugger.valid_port_option'(i, redo, _) :- !.
'$lgt_debugger.valid_port_option'(f, call, _) :- !.
'$lgt_debugger.valid_port_option'(f, fact, _) :- !.
'$lgt_debugger.valid_port_option'(f, rule, _) :- !.
'$lgt_debugger.valid_port_option'(f, redo, _) :- !.
'$lgt_debugger.valid_port_option'(u, call, _) :- !.
'$lgt_debugger.valid_port_option'(n, _, _) :- !.
'$lgt_debugger.valid_port_option'(!, _, _) :- !.
'$lgt_debugger.valid_port_option'(@, _, _) :- !.
'$lgt_debugger.valid_port_option'(b, _, _) :- !.
'$lgt_debugger.valid_port_option'(a, _, _) :- !.
'$lgt_debugger.valid_port_option'('Q', _, _) :- !.
'$lgt_debugger.valid_port_option'(p, _, _) :- !.
'$lgt_debugger.valid_port_option'(d, _, _) :- !.
'$lgt_debugger.valid_port_option'(w, _, _) :- !.
'$lgt_debugger.valid_port_option'(x, _, _) :- !.
'$lgt_debugger.valid_port_option'(h, _, _) :- !.
'$lgt_debugger.valid_port_option'(?, _, _) :- !.
'$lgt_debugger.valid_port_option'(=, _, _) :- !.
'$lgt_debugger.valid_port_option'(*, _, ' ') :- !.
'$lgt_debugger.valid_port_option'(+, _, ' ') :- !.
'$lgt_debugger.valid_port_option'(-, _, +) :- !.
'$lgt_debugger.valid_port_option'(e, exception, _) :- !.


'$lgt_debugger.do_port_option'('\r', _, _, _, _, true).
'$lgt_debugger.do_port_option'('\n', _, _, _, _, true).
'$lgt_debugger.do_port_option'(' ', _, _, _, _, true).
'$lgt_debugger.do_port_option'(c, _, _, _, _, true).

'$lgt_debugger.do_port_option'(l, _, _, _, _, true) :-
	retractall('$lgt_debugger.tracing_').

'$lgt_debugger.do_port_option'(s, call, _, _, _, true) :-
	!,
	retractall('$lgt_debugger.skipping_'),
	assertz('$lgt_debugger.skipping_').
'$lgt_debugger.do_port_option'(s, redo, _, _, _, fail) :-
	!,
	retractall('$lgt_debugger.skipping_'),
	assertz('$lgt_debugger.skipping_').
'$lgt_debugger.do_port_option'(s, _, _, _, _, true).

'$lgt_debugger.do_port_option'(i, _, _, _, _, ignore).

'$lgt_debugger.do_port_option'(f, _, _, _, _, fail).

'$lgt_debugger.do_port_option'(u, _, Goal, _, _, Result) :-
	write('  |: '),
	read(Term),
	(	Goal = Term ->
		Result = unify
	;	Result = fail
	).

'$lgt_debugger.do_port_option'(t, _, _, _, _, _) :-
	(	'$lgt_debugger.tracing_' ->
		true
	;	assertz('$lgt_debugger.tracing_')
	),
	fail.

'$lgt_debugger.do_port_option'(n, _, _, _, _, true) :-
	'$lgt_debugger.nodebug'.

'$lgt_debugger.do_port_option'(=, _, _, _, _, _) :-
	'$lgt_debugger.debugging',
	fail.

'$lgt_debugger.do_port_option'(+, _, Goal, _, _, _) :-
	(	Goal = (_ :: Pred) ->
		functor(Pred, Functor, Arity)
	;	functor(Goal, Functor, Arity)
	),
	'$lgt_debugger.spy'(Functor/Arity),
	fail.

'$lgt_debugger.do_port_option'(-, _, Goal, _, _, true) :-
	(	Goal = (_ :: Pred) ->
		functor(Pred, Functor, Arity)
	;	functor(Goal, Functor, Arity)
	),
	'$lgt_debugger.nospy'(Functor/Arity).

'$lgt_debugger.do_port_option'(*, _, Goal, _, _, _) :-
	'$lgt_term_template'(Goal, CGoal),
	write('  Enter a context spy point term formatted as (Sender, This, Self, Goal): '),
	read(Spypoint),
	Spypoint = (Sender, This, Self, CGoal),
	'$lgt_debugger.spy'(Sender, This, Self, CGoal),
	fail.

'$lgt_debugger.do_port_option'(/, _, Goal, _, _, _) :-
	'$lgt_term_template'(Goal, CGoal),
	write('  Enter a context spy point term formatted as (Sender, This, Self, Goal): '),
	read(Spypoint),
	Spypoint = (Sender, This, Self, CGoal),
	'$lgt_debugger.nospy'(Sender, This, Self, CGoal),
	fail.

'$lgt_debugger.do_port_option'(!, Port, Goal, Error, ExCtx, Action) :-
	'$lgt_debugger.do_port_option'(@, Port, Goal, Error, ExCtx, Action).

'$lgt_debugger.do_port_option'(@, _, _, _, _, _) :-
	write('  ?- '),
	read(Goal),
	once(Goal),
	fail.

'$lgt_debugger.do_port_option'(b, _, _, _, _, _) :-
	(	'$lgt_compiler_flag'(break_predicate, supported) ->
		'$lgt_debugger.suspend'(Tracing),
		break,
		'$lgt_debugger.resume'(Tracing)
	;	write('  break no supportd by the back-end Prolog compiler.'), nl
	),
	fail.

'$lgt_debugger.do_port_option'(a, _, _, _, _, _) :-
	retractall('$lgt_debugger.skipping_'),
	throw(logtalk_debugger_aborted).

'$lgt_debugger.do_port_option'('Q', _, _, _, _, _) :-
	halt.

'$lgt_debugger.do_port_option'(p, _, Goal, _, _, _) :-
	write('  Current goal: '), catch(print(Goal), _, write(Goal)), nl,
	fail.

'$lgt_debugger.do_port_option'(d, _, Goal, _, _, _) :-
	write('  Current goal: '), write_term(Goal, [quoted(true), ignore_ops(true), numbervars(false)]), nl,
	fail.

'$lgt_debugger.do_port_option'(w, _, Goal, _, _, _) :-
	write('  Current goal: '), write_term(Goal, [quoted(true), ignore_ops(false), numbervars(true)]), nl,
	fail.

'$lgt_debugger.do_port_option'(x, _, _, _, ExCtx, _) :-
	'$lgt_exec_ctx'(ExCtx, Sender, This, Self, _, _),
	write('  Sender: '), writeq(Sender), nl,
	write('  This:   '), writeq(This), nl,
	write('  Self:   '), writeq(Self), nl,
	fail.

'$lgt_debugger.do_port_option'(e, _, _, Error, _, _) :-
	write('  Exception term: '), writeq(Error), nl,
	fail.

'$lgt_debugger.do_port_option'(h, _, _, _, _, _) :-
	write('  Available options are:'), nl,
	write('      c - creep (go on; you may use also the spacebar, return, or enter keys)'), nl,
	write('      l - leap (continues execution until the next spy point is found)'), nl,
	write('      s - skip (skips debugging for the current goal; only meaningful at call and redo ports)'), nl,
	write('      i - ignore (ignores goal, assumes that it succeeded; only valid at call and redo ports)'), nl,
	write('      f - fail (forces backtracking; may also be used to convert an exception into a failure)'), nl,
	write('      u - unify (reads and unifies a term with the current goal; only valid at the call port)'), nl,
	write('      n - nodebug (turns off debugging)'), nl,
	write('      ! - command (reads and executes a query)'), nl,
	write('      @ - command (reads and executes a query)'), nl,
	write('      b - break (suspends execution and starts new interpreter; type end_of_file to terminate)'), nl,
	write('      a - abort (returns to top level interpreter)'), nl,
	write('      Q - quit (quits Logtalk)'), nl,
	write('      p - print (writes current goal using print/1 if available)'), nl,
	write('      d - display (writes current goal without using operator notation)'), nl,
	write('      w - write (writes current goal quoting atoms if necessary)'), nl,
	write('      x - context (prints execution context)'), nl,
	write('      e - exception (prints exception term thrown by current goal)'), nl,
	write('      = - debugging (prints debugging information)'), nl,
	write('      * - add (adds a context spy point for current goal)'), nl,
	write('      / - remove (removes a context spy point for current goal)'), nl,
	write('      + - add (adds a predicate spy point for current goal)'), nl,
	write('      - - remove (removes a predicate spy point for current goal)'), nl,
	write('      h - help (prints this list of options)'), nl,
	write('      ? - help (prints this list of options)'), nl,
	fail.

'$lgt_debugger.do_port_option'(?, Port, Goal, Error, ExCtx, Action) :-
	'$lgt_debugger.do_port_option'(h, Port, Goal, Error, ExCtx, Action).



% '$lgt_debugger.inc_invocation_number'(-integer)
%
% we should have used '$lgt_compiler_flag'/2 instead of directly calling
% '$lgt_prolog_feature'/2 but we want to minimize the performance penalty
% while avoiding race conditions when running multi-threaded code compiled
% in debug mode

'$lgt_debugger.inc_invocation_number'(New) :-
	(	'$lgt_prolog_feature'(threads, supported) ->
		with_mutex('$lgt_threaded_dbg', '$lgt_debugger.inc_invocation_number_aux'(New))
	;	retract('$lgt_debugger.invocation_number_'(Old)),
		New is Old + 1,
		asserta('$lgt_debugger.invocation_number_'(New))
	).


'$lgt_debugger.inc_invocation_number_aux'(New) :-
	retract('$lgt_debugger.invocation_number_'(Old)),
	New is Old + 1,
	asserta('$lgt_debugger.invocation_number_'(New)).



'$lgt_debugger.reset_invocation_number' :-
	retractall('$lgt_debugger.invocation_number_'(_)),
	asserta('$lgt_debugger.invocation_number_'(0)).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  "expanding" built-in protocol
%
%  implements the Logtalk term and goal expansion protocol
%
%  the clauses correspond to a virtual compilation of the protocol using a
%  "code_prefix" flag set to '$lgt_'
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



'$lgt_expanding._dcl'(goal_expansion(_, _), p(p(p)), no, 18).		% dynamic + multifile
'$lgt_expanding._dcl'(term_expansion(_, _), p(p(p)), no, 18).		% dynamic + multifile

'$lgt_expanding._dcl'(Pred, Scope, Meta, Flags, expanding) :-
	'$lgt_expanding._dcl'(Pred, Scope, Meta, Flags).

'$lgt_expanding._alias'(_, Pred, Pred).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  "monitoring" built-in protocol
%
%  implements the Logtalk event handlers protocol
%
%  the clauses correspond to a virtual compilation of the protocol using a
%  "code_prefix" flag set to '$lgt_'
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



'$lgt_monitoring._dcl'(before(_, _, _), p(p(p)), no, 0).			% static
'$lgt_monitoring._dcl'(after(_, _, _), p(p(p)), no, 0).				% static

'$lgt_monitoring._dcl'(Pred, Scope, Meta, Flags, monitoring) :-
	'$lgt_monitoring._dcl'(Pred, Scope, Meta, Flags).

'$lgt_monitoring._alias'(_, Pred, Pred).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  pre-processor - compiles Logtalk source files into Prolog source files
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% '$lgt_load_files'(@source_file_name, @list)
% '$lgt_load_files'(@source_file_name_list, @list)
%
% compiles to disk and then loads to memory a source file or a list of source files
%
% a call to this predicate can trigger other calls to it, therefore we must clean
% the compilation auxiliary files before compiling a file

'$lgt_load_files'([], _) :-
	!,
	'$lgt_clear_compiler_flags',
	'$lgt_clean_pp_clauses'.

'$lgt_load_files'([File| Files], Flags) :-
	!,
	'$lgt_clean_pp_clauses',
	'$lgt_set_compiler_flags'(Flags),
	'$lgt_load_file'(File),
	'$lgt_load_files'(Files, Flags).

'$lgt_load_files'(File, Flags) :-
	'$lgt_clean_pp_clauses',
	'$lgt_set_compiler_flags'(Flags),
	'$lgt_load_file'(File),
	'$lgt_clear_compiler_flags',
	'$lgt_clean_pp_clauses'.



% '$lgt_load_file'(@source_file_name)
%
% compiles to disk and then loads to memory a source file

'$lgt_load_file'(Term) :-
	compound(Term),
	!,
	Term =.. [Library, Source],
	'$lgt_expand_library_path'(Library, Path),
	'$lgt_current_directory'(Current),
	'$lgt_change_directory'(Path),				% a little trick necessary to support Prolog compilers that
	'$lgt_current_directory'(ExpandedPath),		% don't provide the necessary support for expanding paths
	(	Current \== ExpandedPath ->
		'$lgt_report_working_directory'(ExpandedPath),
		'$lgt_load_file'(Source),
		'$lgt_change_directory'(Current),
		'$lgt_report_working_directory'(Current)
	;	'$lgt_load_file'(Source)
	).

'$lgt_load_file'(Source) :-
	'$lgt_current_directory'(Directory),
	'$lgt_file_name'(logtalk, Source, File, _),
	(	'$lgt_loaded_file_'(File, Directory) ->
		(	'$lgt_compiler_flag'(reload, skip) ->
			'$lgt_report_skipping_file'(Source)
		;	'$lgt_report_reloading_file'(Source),
			'$lgt_compile_file'(Source),
			'$lgt_load_compiled_file'(Source),
			'$lgt_report_reloaded_file'(Source)
		)
	;	'$lgt_report_loading_file'(Source),
		'$lgt_compile_file'(Source),
		'$lgt_load_compiled_file'(Source),
		'$lgt_report_loaded_file'(Source),
		assertz('$lgt_loaded_file_'(File, Directory))
	).


'$lgt_load_compiled_file'(Source) :-
	'$lgt_file_name'(logtalk, Source, _, SourceFile),
	'$lgt_file_name'(prolog, Source, _, PrologFile),
	'$lgt_clean_lookup_caches',
	'$lgt_check_redefined_entities',
	(	'$lgt_pp_file_encoding_'(_, Encoding) ->
		% use the same encoding as the original source file:
		'$lgt_load_prolog_code'(PrologFile, SourceFile, [encoding(Encoding)])
	;	'$lgt_load_prolog_code'(PrologFile, SourceFile, [])
	),
	(	'$lgt_compiler_flag'(clean, on) ->
		% try to delete the intermediate Prolog ignoring any failure or error:
		catch(('$lgt_delete_file'(PrologFile) -> true; true), _, true),
		forall(
			'$lgt_file_name'(tmp, Source, _, TmpFile),
			catch(('$lgt_delete_file'(TmpFile) -> true; true), _, true))
	;	true
	).



% '$lgt_check_redefined_entities'
%
% checks and prints a warning for all entities that are about to be redefined;
% also retract old runtime clauses for the entity being redefined for safety

'$lgt_check_redefined_entities' :-
	(	'$lgt_pp_file_relation_clause_'('$lgt_current_protocol_'(Entity, _, _, _, _))
	;	'$lgt_pp_file_relation_clause_'('$lgt_current_category_'(Entity, _, _, _, _, _))
	;	'$lgt_pp_file_relation_clause_'('$lgt_current_object_'(Entity, _, _, _, _, _, _, _, _, _, _))
	),
	'$lgt_redefined_entity'(Entity, Type, File),
	'$lgt_report_redefined_entity'(Type, Entity, File),
	'$lgt_retract_old_runtime_clauses'(Entity),
	fail.

'$lgt_check_redefined_entities'.



% '$lgt_redefined_entity'(@entity_identifier, -atom, -atom)
%
% true if an entity of the same name is already loaded; returns entity type

'$lgt_redefined_entity'(Entity, Type, File) :-
	(	'$lgt_current_object_'(Entity, _, _, _, _, _, _, _, _, _, _) ->
		Type = object
	;	'$lgt_current_protocol_'(Entity, _, _, _, _) ->
		Type = protocol
	;	'$lgt_current_category_'(Entity, _, _, _, _, _),
		Type = category
	),
	(	% check file information using the file/2 entity property, if available:
		'$lgt_entity_property_'(Entity, file(OldBase, _, _, OldPath)),
		'$lgt_pp_file_relation_clause_'('$lgt_entity_property_'(Entity, file(NewBase, _, _, NewPath))),
		(OldPath \== NewPath; OldBase \== NewBase) ->
		File = OldBase-OldPath
	;	% either no file/2 entity property or we're reloading the same file 
		File = nil
	).



% '$lgt_report_redefined_entity'(+atom, @entity_identifier, +atom)
%
% prints a warning for redefined entities

'$lgt_report_redefined_entity'(Type, Entity, File) :-
	(	'$lgt_compiler_flag'(report, off) ->
		true
	;	(	'$lgt_pp_load_warnings_flag_' ->
			% not the first warning so we're already in a new line
			true
		;	'$lgt_compiler_flag'(report, warnings) ->
			% in "warnings" mode, print the first warning in a new line
			nl
		;	true
		),
		'$lgt_inc_load_warnings_counter',
		write('%         WARNING!  Redefining '), write(Type), write(' '), 
		current_output(Output), '$lgt_pretty_print_vars_quoted'(Output, Entity), nl,
		(	File == nil ->
			true
		;	% we've conflicting entities coming from different source files:
			File = Base-Path,
			write('%                   loaded from file '), write(Base), write(' ('), write(Path), write(')'), nl
		)
	).



% '$lgt_retract_old_runtime_clauses'(@entity_identifier)
%
% cleans all references to an entity that is about to be redefined from the 
% runtime tables

'$lgt_retract_old_runtime_clauses'(Entity) :-
	retractall('$lgt_before_event_'(_, _, _, Entity, _)),
	retractall('$lgt_after_event_'(_, _, _, Entity, _)),
	retractall('$lgt_current_object_'(Entity, _, _, _, _, _, _, _, _, _, _)),
	retractall('$lgt_current_protocol_'(Entity, _, _, _, _)),
	retractall('$lgt_current_category_'(Entity, _, _, _, _, _)),
	retractall('$lgt_entity_property_'(Entity, _)),
	retractall('$lgt_implements_protocol_'(Entity, _, _)),
	retractall('$lgt_imports_category_'(Entity, _, _)),
	retractall('$lgt_instantiates_class_'(Entity, _, _)),
	retractall('$lgt_specializes_class_'(Entity, _, _)),
	retractall('$lgt_extends_protocol_'(Entity, _, _)),
	retractall('$lgt_extends_object_'(Entity, _, _)),
	retractall('$lgt_extends_category_'(Entity, _, _)),
	retractall('$lgt_complemented_object_'(_, Entity, _, _, _)),
	retractall('$lgt_debugging_entity_'(Entity)).



% '$lgt_report_compiling_entity'(+atom, +entity_identifier)
%
% prints a message that an entity is being compiled

'$lgt_report_compiling_entity'(Type, Entity) :-
	retractall('$lgt_pp_entity_warnings_flag_'),
	(	'$lgt_compiler_flag'(report, on) ->
		write('%     compiling '), write(Type),	write(' '),
		current_output(Output), '$lgt_pretty_print_vars_quoted'(Output, Entity),
		(	'$lgt_compiler_flag'(debug, on) ->
			write(' in debug mode... ')
		;	write('... ')
		)
	;	true
	).



% '$lgt_report_compiled_entity'(+atom, +entity_identifier)
%
% prints a message that an entity is finished compiling

'$lgt_report_compiled_entity'(_, _) :-
	(	'$lgt_compiler_flag'(report, on) ->
		(	'$lgt_pp_entity_warnings_flag_' ->
			write('%     compiled'), nl
		;	write('compiled'), nl
		)
	;	true
	).



% '$lgt_report_working_directory'(+atom)
%
% prints the working directory being used for compiling/loading source files

'$lgt_report_working_directory'(Directory) :-
	(	'$lgt_compiler_flag'(report, on) ->
		nl, write('% +++ working in directory '), write(Directory), nl
	;	true
	).



% '$lgt_report_compiling_file'(+entity_identifier)
%
% prints a message that an entity is being compiled

'$lgt_report_compiling_file'(File) :-
	(	'$lgt_compiler_flag'(report, on) ->
		write('% >>> compiling source file '), writeq(File),
		(	'$lgt_compiler_flag'(debug, on) ->
			write(' in debug mode...')
		;	write('...')
		),
		nl
	;	true
	).



% '$lgt_report_up_to_date_file'(+entity_identifier)
%
% prints a message that an entity is up-to-date

'$lgt_report_up_to_date_file'(File) :-
	(	'$lgt_compiler_flag'(report, on) ->
		write('% >>> compiling source file '), writeq(File), write('... up-to-date'), nl
	;	true
	).



% '$lgt_report_compiled_file'(+atom)
%
% prints a message that a source file is finished compiling

'$lgt_report_compiled_file'(File) :-
	(	'$lgt_compiler_flag'(report, on) ->
		write('% >>> '), writeq(File), write(' source file compiled'), nl
	;	true
	).



% '$lgt_report_loading_file'(+atom)
%
% prints a message that a file is being loaded

'$lgt_report_loading_file'(File) :-
	(	'$lgt_compiler_flag'(report, on) ->
		write('% <<< loading source file '), writeq(File), write('... '), nl
	;	true
	).


% '$lgt_report_reloading_file'(+atom)
%
% prints a message that a file is being reloaded

'$lgt_report_reloading_file'(File) :-
	(	'$lgt_compiler_flag'(report, on) ->
		write('% <<< reloading source file '), writeq(File), write('... '), nl
	;	true
	).


% '$lgt_report_skipping_file'(+atom)
%
% prints a message that loading a file is being skiped

'$lgt_report_skipping_file'(File) :-
	(	'$lgt_compiler_flag'(report, on) ->
		write('% <<< skipping loading of source file '), writeq(File), write(' (already loaded) '), nl
	;	true
	).


% '$lgt_report_loaded_file'(+entity_identifier)
%
% prints a message that a source file finished loading

'$lgt_report_loaded_file'(File) :-
	(	'$lgt_compiler_flag'(report, on) ->
		write('% <<< '), writeq(File), write(' source file loaded'), nl
	;	true
	).


% '$lgt_report_reloaded_file'(+entity_identifier)
%
% prints a message that a source file finished reloading

'$lgt_report_reloaded_file'(File) :-
	(	'$lgt_compiler_flag'(report, on) ->
		write('% <<< '), writeq(File), write(' source file reloaded'), nl
	;	true
	).



% '$lgt_compile_files'(@source_file_name, @list)
% '$lgt_compile_files'(@source_file_name_list, @list)
%
% compiles to disk a source file or a list of source files
%
% a call to this predicate can trigger other calls to it, therefore we must clean
% the compilation auxiliary files before compiling a file

'$lgt_compile_files'([], _) :-
	!,
	'$lgt_clear_compiler_flags',
	'$lgt_clean_pp_clauses'.

'$lgt_compile_files'([File| Files], Flags) :-
	!,
	'$lgt_clean_pp_clauses',
	'$lgt_set_compiler_flags'(Flags),
	'$lgt_compile_file'(File),
	'$lgt_compile_files'(Files, Flags).

'$lgt_compile_files'(File, Flags) :-
	'$lgt_clean_pp_clauses',
	'$lgt_set_compiler_flags'(Flags),
	'$lgt_compile_file'(File),
	'$lgt_clear_compiler_flags',
	'$lgt_clean_pp_clauses'.



% '$lgt_compile_file'(@source_file_name)
%
% compiles to disk a source file

'$lgt_compile_file'(Term) :-
	compound(Term),
	!,
	Term =.. [Library, File],
	'$lgt_expand_library_path'(Library, Path),
	'$lgt_current_directory'(Current),
	'$lgt_change_directory'(Path),				% a little trick necessary to support Prolog compilers that
	'$lgt_current_directory'(ExpandedPath),		% don't provide the necessary support for expanding paths
	(	Current \== ExpandedPath ->
		'$lgt_report_working_directory'(ExpandedPath),
		'$lgt_compile_file'(File),
		'$lgt_change_directory'(Current),
		'$lgt_report_working_directory'(Current)
	;	'$lgt_compile_file'(File)
	).

'$lgt_compile_file'(File) :-
	'$lgt_compiler_flag'(smart_compilation, on),
	\+ '$lgt_needs_recompilation'(File),
	!,
	'$lgt_report_up_to_date_file'(File).

'$lgt_compile_file'(File) :-
	'$lgt_report_compiling_file'(File),
	'$lgt_tr_file'(File),
	'$lgt_report_compiled_file'(File).



% '$lgt_needs_recompilation'(+atom)
%
% source file needs recompilation

'$lgt_needs_recompilation'(File) :-
	'$lgt_file_name'(prolog, File, _, Path),
	\+ '$lgt_file_exists'(Path),
	!.

'$lgt_needs_recompilation'(File) :-
	'$lgt_file_name'(logtalk, File, _, Source),
	'$lgt_file_name'(prolog, File, _, Object),
	(	'$lgt_compare_file_mtimes'(Result, Source, Object) ->
		Result == (>)
	;	true
	).



% '$lgt_write_tr_entity'
%
% writes to disk the entity compiled code

'$lgt_write_tr_entity' :-
	stream_property(Output, alias('$lgt_output')),
	catch(
		('$lgt_write_prolog_terms'(Output),
		 '$lgt_write_logtalk_directives'(Output),
		 '$lgt_write_logtalk_clauses'(Output)),
		Error,
		'$lgt_compiler_stream_io_error_handler'(Output, Error)).



% '$lgt_write_entity_doc'(@entity_identifier)
%
% writes to disk the entity documentation in XML format

'$lgt_write_entity_doc'(Entity) :-
	(	'$lgt_compiler_flag'(xmldocs, on) ->
		'$lgt_entity_doc_file_name'(Entity, File),
		(	'$lgt_pp_file_encoding_'(_, Encoding) ->
			(	'$lgt_pp_file_bom_'(bom(Boolean)) ->
				StreamOptions = [encoding(Encoding), bom(Boolean)]
			;	StreamOptions = [encoding(Encoding)]
			)
		;	StreamOptions = []
		),
		catch(
			(open(File, write, Stream, StreamOptions),
			 '$lgt_write_xml_file'(Stream),
			 close(Stream)),
			Error,
			'$lgt_compiler_stream_io_error_handler'(Stream, Error))	
	;	true
	).



% '$lgt_entity_doc_file_name'(@nonvar, -atom)
%
% generates the XML file name for an entity using the format <functor>_<arity>

'$lgt_entity_doc_file_name'(Entity, File) :-
	functor(Entity, Functor, Arity),
	number_codes(Arity, Codes),
	atom_codes(Atom, Codes),
	atom_concat(Functor, '_', Aux),
	atom_concat(Aux, Atom, Name),
	'$lgt_file_name'(xml, Name, _, File).



% '$lgt_file_name'(+atom, +atom, -atom, -atom)
%
% constructs a file basename (name plus extension) and a file path given the file type
% (logtalk, prolog, or xml) and the file name (file name may include a directory path)

'$lgt_file_name'(Type, Name, Basename, Path) :-
	'$lgt_file_extension'(Type, Extension),			% defined on the Prolog config files
	atom_concat(Name, Extension, Basename),
	(	'$lgt_compiler_flag'(altdirs, on),
		'$lgt_file_type_alt_directory'(Type, Directory) ->
		% file on the alternate compilation directory
		'$lgt_make_directory'(Directory),			% succeeds when the directory already exists
		atom_concat(Directory, Basename, File)		% alternate directories end with a slash
	;	% file local to current working directory
		File = Basename
	),
	(	'$lgt_expand_path'(File, Path) ->			% try to expand the file path in order to
		true										% prevent problems with Prolog compilers
	;	File = Path									% where open/3-4 is not always relative to
	).												% the current working directory



% '$lgt_tr_file'(+atom)
%
% compiles a source file storing the resulting code in memory

'$lgt_tr_file'(File) :-
	'$lgt_save_global_op_table',
	'$lgt_file_name'(logtalk, File, Basename, Source),
	'$lgt_current_directory'(Directory),
	asserta('$lgt_pp_file_path_'(Basename, Directory)),
	% open the Logtalk source code file for reading:
	catch(
		open(Source, read, Input, [alias('$lgt_input')]),
		OpenError,
		'$lgt_compiler_open_stream_error_handler'(OpenError)),
	% look for an enconding/1 directive that, when present, must be the first term on a source file
	catch(
		'$lgt_read_term'(Input, Term, [singletons(Singletons)]),
		InputError,
		'$lgt_compiler_stream_io_error_handler'(Input, InputError)),
	catch(
		'$lgt_check_for_encoding_directive'(Term, Source, Input, NewInput, OutputOptions), 
		FirstTermError,
		'$lgt_compiler_stream_io_error_handler'(NewInput, FirstTermError)),
	% open a corresponding Prolog file for writing generated code using any found encoding/1 directive:
	'$lgt_file_name'(prolog, File, _, Object),
	catch(
		open(Object, write, Output, [alias('$lgt_output')| OutputOptions]),
		OpenError,
		'$lgt_compiler_error_handler'(OpenError)),
	catch(
		'$lgt_write_encoding_directive'(Output),
		WriteError,
		'$lgt_compiler_error_handler'(WriteError)),
	% read and compile the remaining terms in the Logtalk source file:
	catch(
		'$lgt_tr_file'(Term, Singletons, NewInput),
		Error,
		'$lgt_compiler_error_handler'(Error)),
	close(NewInput),
	% finnish writing output Prolog file:
	catch(
		('$lgt_write_prolog_terms'(Output),								% write out any Prolog code occurring after the last source file entity;
		 '$lgt_write_runtime_clauses'(Output),							% write entity runtime directives and clauses;
		 '$lgt_write_init_call'(Output)),								% write initialization/1 directive at the
		OutputError,													% end of the file to improve compatibility 
		'$lgt_compiler_stream_io_error_handler'(Output, OutputError)),	% with non-ISO compliant Prolog compilers;
	close(Output),
	retractall('$lgt_pp_file_path_'(_, _)),
	'$lgt_restore_global_op_table'.



% '$lgt_check_for_encoding_directive'(?term, +atom, @stream, -stream, -list)
%
% encoding/1 directives must be used during entity compilation and for the
% encoding of the generated Prolog and XML files; BOM present in the source
% file is inherited by the generated Prolog and XML files:

'$lgt_check_for_encoding_directive'(Term, _, _, _, _) :-
	var(Term),
	throw(error(instantiation_error, term(Term))).

'$lgt_check_for_encoding_directive'((:- Term), _, _, _, _) :-
	var(Term),
	throw(error(instantiation_error, directive(Term))).

'$lgt_check_for_encoding_directive'((:- encoding(LogtalkEncoding)), Source, Input, NewInput, [encoding(PrologEncoding)|BOM]) :-
	!,
	(	\+ '$lgt_compiler_flag'(encoding_directive, unsupported) ->
		(	'$lgt_logtalk_prolog_encoding'(LogtalkEncoding, PrologEncoding, Input) ->		% defined in the config files
			assertz('$lgt_pp_file_encoding_'(LogtalkEncoding, PrologEncoding)),
			close(Input),
			open(Source, read, NewInput, [alias('$lgt_input'), encoding(PrologEncoding)]),
			(	catch(stream_property(NewInput, bom(Boolean)), _, fail) ->					% SWI-Prolog and YAP
				BOM = [bom(Boolean)],
				assertz('$lgt_pp_file_bom_'(bom(Boolean)))
			;	catch(stream_property(NewInput, encoding_signature(Boolean)), _, fail) ->	% SICStus Prolog
				BOM = [encoding_signature(Boolean)]
			;	BOM = []
			),
			'$lgt_read_term'(NewInput, _, [singletons(_)])									% throw away encoding/1 directive
		;	throw(error(domain_error(directive, encoding/1), directive(encoding(LogtalkEncoding))))
		)
	;	throw(error(resource_error(text_encoding_support), directive(encoding(LogtalkEncoding))))
	).

'$lgt_check_for_encoding_directive'(_, _, Input, Input, []).	% assume no encoding/1 directive present on the source file



% '$lgt_tr_file'(?term, +list, @stream)

'$lgt_tr_file'(Term, _, _) :-
	var(Term),
	throw(instantiation_error).

'$lgt_tr_file'(end_of_file, _, _) :-							% module definitions start with an opening module/1-2
	'$lgt_pp_module_'(Module),									% directive and are assumed to end at the end of a 
	'$lgt_pp_object_'(Module, _, _, _, _, _, _, _, _, _, _),	% source file; there is no module closing directive
	'$lgt_comp_ctx_mode'(Ctx, compile(regular)),				% set the initial compilation context
	'$lgt_tr_term'(end_of_file, Ctx),							% for compiling the end_of_file term
	'$lgt_tr_entity'(object, Module),
	'$lgt_report_compiled_entity'(module, Module),
	!.

'$lgt_tr_file'(end_of_file, _, _) :-
	'$lgt_pp_object_'(Obj, _, _, _, _, _, _, _, _, _, _),
	throw(directive_missing(end_object, object(Obj))).

'$lgt_tr_file'(end_of_file, _, _) :-
	'$lgt_pp_protocol_'(Ptc, _, _, _, _),
	throw(directive_missing(end_protocol, protocol(Ptc))).

'$lgt_tr_file'(end_of_file, _, _) :-
	'$lgt_pp_category_'(Ctg, _, _, _, _, _),
	throw(directive_missing(end_category, category(Ctg))).

'$lgt_tr_file'(end_of_file, _, _) :-
	'$lgt_pp_cc_if_found_'(Goal),
	throw(directive_missing(endif, if(Goal))).

'$lgt_tr_file'(end_of_file, _, _) :-							% allow for term-expansion
	'$lgt_comp_ctx_mode'(Ctx, compile(regular)),				% set the initial compilation context
	'$lgt_tr_term'(end_of_file, Ctx),							% for compiling the end_of_file term
	!.

'$lgt_tr_file'(Term, _, Input) :-
	'$lgt_pp_cc_skipping_',										% we're performing conditional compilation and skipping terms 
	\+ '$lgt_lgt_cc_directive'(Term),							% except for conditional compilation directives itself
	!,
	'$lgt_read_term'(Input, Next, [singletons(NextSingletons)]),
	'$lgt_tr_file'(Next, NextSingletons, Input).

'$lgt_tr_file'(Term, Singletons, Input) :-
	'$lgt_report_singletons'(Singletons, Term),
	'$lgt_comp_ctx_mode'(Ctx, compile(regular)),				% set the initial compilation context
	'$lgt_tr_term'(Term, Ctx),									% for compiling the read term
	'$lgt_read_term'(Input, Next, [singletons(NextSingletons)]),
	'$lgt_tr_file'(Next, NextSingletons, Input).



% '$lgt_add_referenced_object'(@object_identifier)
%
% adds referenced object for cheking references to unknown objects

'$lgt_add_referenced_object'(Obj) :-
	(	'$lgt_pp_referenced_object_'(Obj) ->
		true
	;	atom(Obj) ->
		assertz('$lgt_pp_referenced_object_'(Obj))
	;	'$lgt_entity_template'(Obj, Template),
		assertz('$lgt_pp_referenced_object_'(Template))
	).



% '$lgt_add_referenced_protocol'(@protocol_identifier)
%
% adds referenced protocol for cheking references to unknown protocols

'$lgt_add_referenced_protocol'(Ptc) :-
	(	'$lgt_pp_referenced_protocol_'(Ptc) ->
		true
	;	assertz('$lgt_pp_referenced_protocol_'(Ptc))
	).



% '$lgt_add_referenced_category'(@category_identifier)
%
% adds referenced category for cheking references to unknown categories

'$lgt_add_referenced_category'(Ctg) :-
	(	'$lgt_pp_referenced_category_'(Ctg) ->
		true
	;	assertz('$lgt_pp_referenced_category_'(Ctg))
	).



% '$lgt_add_entity_file_properties'(@atom, @entity_identifier)
%
% adds entity properties related to the entity source file

'$lgt_add_entity_file_properties'(start, Entity) :-
	(	'$lgt_pp_file_path_'(File, Path) ->
		(	'$lgt_pp_term_position_'((Start - _)) ->
			assertz('$lgt_pp_relation_clause_'('$lgt_entity_property_'(Entity, file(File, Start, _, Path))))
		;	assertz('$lgt_pp_relation_clause_'('$lgt_entity_property_'(Entity, file(File, -1, _, Path))))
		)
	;	true
	).

'$lgt_add_entity_file_properties'(end, Entity) :-
	(	'$lgt_pp_file_path_'(File, Path) ->
		(	'$lgt_pp_term_position_'((_ - End)) ->
			retract('$lgt_pp_relation_clause_'('$lgt_entity_property_'(Entity, file(File, Start, _, Path)))),
			assertz('$lgt_pp_relation_clause_'('$lgt_entity_property_'(Entity, file(File, Start, End, Path))))
		;	retract('$lgt_pp_relation_clause_'('$lgt_entity_property_'(Entity, file(File, Start, _, Path)))),
			assertz('$lgt_pp_relation_clause_'('$lgt_entity_property_'(Entity, file(File, Start, -1, Path))))
		), !
	;	true
	).



% '$lgt_report_singletons'(+list, +term)
%
% reports the singleton variables found while compiling an entity term

'$lgt_report_singletons'(Singletons, Term) :-
	(	'$lgt_compiler_flag'(singletons, warning),
		\+ '$lgt_compiler_flag'(report, off) ->
		'$lgt_filter_singletons'(Singletons, Names),
		'$lgt_report_singleton_names'(Names, Term)
	;	true
	).


'$lgt_report_singleton_names'([], _) :-
	!.	% cut necessary to prevent problems with compilers with broken read_term/3 implementations

'$lgt_report_singleton_names'([Name| Names], Term) :-
	'$lgt_report_warning_in_new_line',
	'$lgt_inc_compile_warnings_counter',
	(	Names == [] ->
		write('%         WARNING!  Singleton variable ')
	;	write('%         WARNING!  Singleton variables ')
	),
	'$lgt_report_singletons_term'(Term),
	'$lgt_write_list'([Name| Names]), nl,
	(	'$lgt_pp_entity'(Type, Entity, _, _, _) ->
		'$lgt_report_warning_full_context'(Type, Entity)
	;	'$lgt_report_warning_file_context'
	).


'$lgt_report_singletons_term'((:- Term)) :-
	!,
	(	var(Term) ->
		true
	;	functor(Term, Functor, Arity),
		write('in directive '),
		writeq(Functor/Arity),
		write(': ')
	).

'$lgt_report_singletons_term'((Term :- _)) :-
	!,
	(	var(Term) ->
		true
	;	functor(Term, Functor, Arity),
		write('in clause for predicate '),
		writeq(Functor/Arity),
		write(': ')
	).
	
'$lgt_report_singletons_term'((Term, _ --> _)) :-
	!,
	(	var(Term) ->
		true
	;	functor(Term, Functor, Arity),
		write('in grammar rule for non-terminal '),
		writeq(Functor//Arity),
		write(': ')
	).

'$lgt_report_singletons_term'((Term --> _)) :-
	!,
	(	var(Term) ->
		true
	;	functor(Term, Functor, Arity),
		write('in grammar rule for non-terminal '),
		writeq(Functor//Arity),
		write(': ')
	).

'$lgt_report_singletons_term'(Term) :-	% facts
	(	var(Term) ->
		true
	;	functor(Term, Functor, Arity),
		write('in clause for predicate '),
		writeq(Functor/Arity),
		write(': ')
	).



% '$lgt_filter_singletons'(+list, -list)
%
% filters variables whose name start with an underscore from a singletons list if 
% the corresponding compiler flag sets their interpretation to don't care variables

'$lgt_filter_singletons'(List, Result) :-
	(	'$lgt_compiler_flag'(underscore_variables, dont_care) ->
		'$lgt_filter_dont_care_vars'(List, Result)
	;	'$lgt_singleton_var_names'(List, Result)
	).


'$lgt_singleton_var_names'([], []).

'$lgt_singleton_var_names'([Name = _| Singletons], [Name| Names]) :-
	'$lgt_singleton_var_names'(Singletons, Names).


'$lgt_filter_dont_care_vars'([], []).

'$lgt_filter_dont_care_vars'([Name = _| Singletons], Names) :-
	(	atom_concat('_', _, Name) ->
		'$lgt_filter_dont_care_vars'(Singletons, Names)
	;	Names = [Name| Rest],
		'$lgt_filter_dont_care_vars'(Singletons, Rest)
	).



% '$lgt_compiler_error_handler'(+term)
%
% closes the streams being used for reading and writing terms, restores
% the operator table, and reports the compilation error found

'$lgt_compiler_error_handler'(Error) :-
	'$lgt_pp_file_path_'(File, _),
	stream_property(Input, alias('$lgt_input')),
	stream_property(Output, alias('$lgt_output')),
	'$lgt_report_compiler_error_message'(Error),
	'$lgt_report_error_context'(File, Input),
	'$lgt_restore_global_op_table',
	'$lgt_clean_pp_clauses',
	'$lgt_reset_warnings_counter',
	catch(close(Input), _, true),
	(	nonvar(Output) ->
		catch(close(Output), _, true),
		'$lgt_file_name'(logtalk, Name, File, _),	% try to delete the intermediate Prolog file in order to
		'$lgt_file_name'(prolog, Name, _, Prolog),	% avoid problems when using the "smart_compilation" flag
		catch(('$lgt_delete_file'(Prolog) -> true; true), _, true),
		forall(
			'$lgt_file_name'(tmp, Name, _, TmpFile),
			catch(('$lgt_delete_file'(TmpFile) -> true; true), _, true))
	;	true
	),
	throw(Error).



% '$lgt_compiler_stream_io_error_handler'(@stream, +term)
%
% closes the stream being used for reading or writing terms, restores
% the operator table, and reports the compilation error found

'$lgt_compiler_stream_io_error_handler'(Stream, Error) :-
	'$lgt_report_compiler_error_message'(Error),
	'$lgt_restore_global_op_table',
	'$lgt_clean_pp_clauses',
	'$lgt_reset_warnings_counter',
	catch(close(Stream), _, true),
	throw(Error).



% '$lgt_compiler_open_stream_error_handler'(+term)
%
% restores the operator table, and reports the compilation error found

'$lgt_compiler_open_stream_error_handler'(Error) :-
	('$lgt_pp_entity'(_, _, _, _, _) -> nl; true),
	write('%         ERROR!    '), writeq(Error), nl,
	'$lgt_restore_global_op_table',
	'$lgt_clean_pp_clauses',
	'$lgt_reset_warnings_counter',
	throw(Error).



% '$lgt_report_error_context'(+atom, @stream)
%
% reports a compilation error context

'$lgt_report_error_context'(File, Input) :-
	(	'$lgt_compiler_flag'(report, warnings) ->
		write('%                   in file '), write(File),
		(	catch('$lgt_stream_current_line_number'(Input, Line), _, fail) ->
			write(', above line '), write(Line), nl
		;	true
		)
	;	(	catch('$lgt_stream_current_line_number'(Input, Line), _, fail) ->
			write('%                   above line '), write(Line), nl
		;	true
		)
	).



% '$lgt_report_compiler_error_message'(+nonvar)
%
% reports a compilation error message

'$lgt_report_compiler_error_message'(error(Error, entity(Type, Entity))) :-
	!,
	('$lgt_pp_entity'(_, _, _, _, _) -> nl; true),
	write('%         ERROR!    '), writeq(Error), nl,
	write('%                   in '), write(Type), write(': '), writeq(Entity), nl.

'$lgt_report_compiler_error_message'(error(Error, directive(Directive))) :-
	!,
	('$lgt_pp_entity'(_, _, _, _, _) -> nl; true),
	write('%         ERROR!    '), writeq(Error), nl,
	write('%                   in directive '), writeq((:- Directive)), nl.

'$lgt_report_compiler_error_message'(error(Error, clause(Clause))) :-
	!,
	('$lgt_pp_entity'(_, _, _, _, _) -> nl; true),
	write('%         ERROR!    '), writeq(Error), nl,
	write('%                   in clause '), writeq(Clause), nl.

'$lgt_report_compiler_error_message'(error(Error, dcgrule(Rule))) :-
	!,
	('$lgt_pp_entity'(_, _, _, _, _) -> nl; true),
	write('%         ERROR!    '), writeq(Error), nl,
	write('%                   in grammar rule '), writeq((Rule)), nl.

'$lgt_report_compiler_error_message'(error(Error, Term)) :-
	!,
	('$lgt_pp_entity'(_, _, _, _, _) -> nl; true),
	write('%         ERROR!    '), writeq(Error), nl,
	write('%                   in '), writeq(Term), nl.

'$lgt_report_compiler_error_message'(Error) :-
	('$lgt_pp_entity'(_, _, _, _, _) -> nl; true),
	write('%         ERROR!    '), writeq(Error), nl.



% '$lgt_read_term'(@stream, -term, @list)

'$lgt_read_term'(Stream, Term, Options) :-
	'$lgt_read_term'(Stream, Term, Options, Position),	% defined in the config files
	(	Position == -1 ->
		% back-end Prolog compiler doesn't support read term position
		true
	;	% remember term position in order to support logtalk_load_context/2 and more
		% informative compiler warning and error messages
		retractall('$lgt_pp_term_position_'(_)),			
		asserta('$lgt_pp_term_position_'(Position))
	).



% '$lgt_tr_entity'(+atom, @entity_identifier)

'$lgt_tr_entity'(Type, Entity) :-
	'$lgt_generate_code'(Type),
	'$lgt_report_problems'(Type, Entity),
	'$lgt_write_tr_entity',
	'$lgt_write_entity_doc'(Entity),
	'$lgt_save_entity_rclauses',
	'$lgt_clean_pp_entity_clauses'.



% '$lgt_object_flags'(?atom, +integer)

'$lgt_object_flags'(context_switching_calls, Flags) :-
	Flags /\ 128 =:= 128.
'$lgt_object_flags'(dynamic_declarations, Flags) :-
	Flags /\ 64 =:= 64.
'$lgt_object_flags'(complements, Flags) :-
	Flags /\ 32 =:= 32.
'$lgt_object_flags'(events, Flags) :-
	Flags /\ 16 =:= 16.
'$lgt_object_flags'(threaded, Flags) :-
	Flags /\ 8 =:= 8.
'$lgt_object_flags'(synchronized, Flags) :-
	Flags /\ 4 =:= 4.
'$lgt_object_flags'((dynamic), Flags) :-
	Flags /\ 2 =:= 2.
'$lgt_object_flags'(static, Flags) :-
	Flags /\ 2 =:= 0.
'$lgt_object_flags'(built_in, Flags) :-
	Flags /\ 1 =:= 1.



% '$lgt_protocol_flags'(?atom, +integer)

'$lgt_protocol_flags'((dynamic), Flags) :-
	Flags /\ 2 =:= 2.
'$lgt_protocol_flags'(static, Flags) :-
	Flags /\ 2 =:= 0.
'$lgt_protocol_flags'(built_in, Flags) :-
	Flags /\ 1 =:= 1.



% '$lgt_category_flags'(?atom, +integer)

'$lgt_category_flags'(events, Flags) :-
	Flags /\ 16 =:= 16.
'$lgt_category_flags'(synchronized, Flags) :-
	Flags /\ 2 =:= 2.
'$lgt_category_flags'((dynamic), Flags) :-
	Flags /\ 2 =:= 2.
'$lgt_category_flags'(static, Flags) :-
	Flags /\ 2 =:= 0.
'$lgt_category_flags'(built_in, Flags) :-
	Flags /\ 1 =:= 1.



% '$lgt_tr_entity_flags'(+atom, +atom, -integer)
%
% defines the entity flags value when creating a new entity

'$lgt_tr_entity_flags'(protocol, Mode, Flags) :-
	(	Mode == (dynamic) ->
		Dynamic = 2						% 0b00000010
	;	Dynamic = 0
	),
	Flags is Dynamic.

'$lgt_tr_entity_flags'(category, Mode, Flags) :-
	(	'$lgt_compiler_flag'(events, allow) ->
		Events = 16						% 0b0001000
	;	Events = 0
	),
	(	'$lgt_pp_synchronized_' ->
		Synchronized = 4				% 0b00000100
	;	Synchronized = 0
	),
	(	Mode == (dynamic) ->
		Dynamic = 2						% 0b00000010
	;	Dynamic = 0
	),
	Flags is Events + Synchronized + Dynamic.

'$lgt_tr_entity_flags'(object, Mode, Flags) :-
	(	'$lgt_compiler_flag'(context_switching_calls, allow) ->
		ContextSwitchingCalls = 128		% 0b10000000
	;	ContextSwitchingCalls = 0
	),
	(	'$lgt_compiler_flag'(dynamic_declarations, allow) ->
		DynamicDeclarations = 64		% 0b01000000
	;	DynamicDeclarations = 0
	),
	(	'$lgt_compiler_flag'(complements, allow) ->
		Complements = 32				% 0b00100000
	;	Complements = 0
	),
	(	'$lgt_compiler_flag'(events, allow) ->
		Events = 16						% 0b00010000
	;	Events = 0
	),
	(	'$lgt_pp_threaded_' ->
		Threaded = 8					% 0b00001000
	;	Threaded = 0
	),
	(	'$lgt_pp_synchronized_' ->
		Synchronized = 4				% 0b00000100
	;	Synchronized = 0
	),
	(	Mode == (dynamic) ->
		Dynamic = 2						% 0b00000010
	;	Dynamic = 0
	),
	Flags is ContextSwitchingCalls + DynamicDeclarations + Complements + Events + Threaded + Synchronized + Dynamic.



% saves entity runtime clauses in order to be able to check for redefined 
% entities when loading the intermediate Prolog files generated by the 
% Logtalk compiler and for writing the entity runtime multifile and dynamic
% directives and the entity runtime clauses for all defined entities

'$lgt_save_entity_rclauses' :-
	'$lgt_pp_relation_clause'(Clause),
	assertz('$lgt_pp_file_relation_clause_'(Clause)),
	fail.

'$lgt_save_entity_rclauses'.



% cleans up all dynamic predicates used during source file compilation
% (except any user-defined compiler options specified on the compiling 
% and loading predicates)

'$lgt_clean_pp_clauses' :-
	'$lgt_clean_pp_entity_clauses',
	retractall('$lgt_pp_global_op_'(_, _, _)),
	retractall('$lgt_pp_file_op_'(_, _, _)),
	retractall('$lgt_pp_entity_op_'(_, _, _, _)),
	retractall('$lgt_pp_file_init_'(_)),
	retractall('$lgt_pp_entity_init_'(_, _, _)),
	retractall('$lgt_pp_file_encoding_'(_, _)),
	retractall('$lgt_pp_file_bom_'(_)),
	retractall('$lgt_pp_file_path_'(_, _)),
	retractall('$lgt_pp_file_relation_clause_'(_)),
	retractall('$lgt_pp_cc_if_found_'(_)),
	retractall('$lgt_pp_cc_skipping_'),
	retractall('$lgt_pp_cc_mode_'(_)),
	retractall('$lgt_pp_term_position_'(_)).



% cleans up all dynamic predicates used during entity compilation

'$lgt_clean_pp_entity_clauses' :-
	retractall('$lgt_pp_entity_compiler_flag_'(_, _)),
	retractall('$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, _, _)),
	retractall('$lgt_pp_protocol_'(_, _, _, _, _)),
	retractall('$lgt_pp_category_'(_, _, _, _, _, _)),
	retractall('$lgt_pp_module_'(_)),
	retractall('$lgt_pp_implemented_protocol_'(_, _, _, _)),
	retractall('$lgt_pp_imported_category_'(_, _, _, _, _)),
	retractall('$lgt_pp_extended_object_'(_, _, _, _, _, _, _, _, _, _)),
	retractall('$lgt_pp_instantiated_class_'(_, _, _, _, _, _, _, _, _, _)),
	retractall('$lgt_pp_specialized_class_'(_, _, _, _, _, _, _, _, _, _)),
	retractall('$lgt_pp_extended_protocol_'(_, _, _, _)),
	retractall('$lgt_pp_extended_category_'(_, _, _, _, _)),
	retractall('$lgt_pp_complemented_object_'(_)),
	retractall('$lgt_pp_uses_'(_)),
	retractall('$lgt_pp_uses_predicate_'(_, _, _)),
	retractall('$lgt_pp_uses_non_terminal_'(_, _, _)),
	retractall('$lgt_pp_use_module_predicate_'(_, _, _)),
	retractall('$lgt_pp_use_module_non_terminal_'(_, _, _)),
	retractall('$lgt_pp_calls_'(_)),
	retractall('$lgt_pp_info_'(_)),
	retractall('$lgt_pp_info_'(_, _)),
	retractall('$lgt_pp_directive_'(_)),
	retractall('$lgt_pp_synchronized_'(_, _)),
	retractall('$lgt_pp_predicate_mutex_counter_'(_)),
	retractall('$lgt_pp_public_'(_, _)),
	retractall('$lgt_pp_protected_'(_, _)),
	retractall('$lgt_pp_private_'(_, _)),
	retractall('$lgt_pp_dynamic_'(_, _)),
	retractall('$lgt_pp_discontiguous_'(_, _)),
	retractall('$lgt_pp_multifile_'(_, _)),
	retractall('$lgt_pp_coinductive_'(_, _)),
	retractall('$lgt_pp_coinductive_flag_'(_)),
	retractall('$lgt_pp_mode_'(_, _)),
	retractall('$lgt_pp_meta_predicate_'(_)),
	retractall('$lgt_pp_value_annotation_'(_, _, _, _)),
	retractall('$lgt_pp_goal_annotation_'(_, _, _, _)),
	retractall('$lgt_pp_predicate_alias_'(_, _, _)),
	retractall('$lgt_pp_non_terminal_'(_, _, _)),
	retractall('$lgt_pp_entity_init_'(_)),
	retractall('$lgt_pp_final_entity_init_'(_)),
	retractall('$lgt_pp_dcl_'(_)),
	retractall('$lgt_pp_def_'(_)),
	retractall('$lgt_pp_final_def_'(_)),
	retractall('$lgt_pp_ddef_'(_)),
	retractall('$lgt_pp_final_ddef_'(_)),
	retractall('$lgt_pp_super_'(_)),
	retractall('$lgt_pp_prolog_term_'(_, _)),
	retractall('$lgt_pp_relation_clause_'(_)),
	retractall('$lgt_pp_entity_clause_'(_, _)),
	retractall('$lgt_pp_final_entity_clause_'(_, _)),
	retractall('$lgt_pp_entity_aux_clause_'(_)),
	retractall('$lgt_pp_final_entity_aux_clause_'(_)),
	retractall('$lgt_pp_redefined_built_in_'(_, _, _)),
	retractall('$lgt_pp_defines_predicate_'(_, _)),
	retractall('$lgt_pp_calls_predicate_'(_, _, _, _)),
	retractall('$lgt_pp_non_portable_call_'(_, _)),
	retractall('$lgt_pp_non_portable_function_'(_, _)),
	retractall('$lgt_pp_missing_dynamic_directive_'(_, _)),
	retractall('$lgt_pp_missing_discontiguous_directive_'(_, _)),
	retractall('$lgt_pp_previous_predicate_'(_, _)),
	retractall('$lgt_pp_defines_non_terminal_'(_, _)),
	retractall('$lgt_pp_calls_non_terminal_'(_, _)),
	retractall('$lgt_pp_referenced_object_'(_)),
	retractall('$lgt_pp_referenced_protocol_'(_)),
	retractall('$lgt_pp_referenced_category_'(_)),
	retractall('$lgt_pp_threaded_'),
	retractall('$lgt_pp_synchronized_'),
	retractall('$lgt_pp_aux_predicate_counter_'(_)),
	asserta('$lgt_pp_aux_predicate_counter_'(0)).



% '$lgt_clean_lookup_caches'

'$lgt_clean_lookup_caches' :-
	retractall('$lgt_send_to_obj_'(_, _, _)),
	retractall('$lgt_send_to_obj_ne_'(_, _, _)),
	retractall('$lgt_send_to_self_'(_, _, _)),
	retractall('$lgt_obj_super_call_same_'(_, _, _)),
	retractall('$lgt_obj_super_call_other_'(_, _, _)),
	retractall('$lgt_ctg_super_call_same_'(_, _, _)),
	retractall('$lgt_ctg_super_call_other_'(_, _, _)),
	retractall('$lgt_ctg_call_'(_, _, _)),
	retractall('$lgt_db_lookup_cache_'(_, _, _, _, _)),
	'$lgt_reassert_lookup_cache_catchall_clauses'.


% '$lgt_clean_lookup_caches'(@callable)

'$lgt_clean_lookup_caches'(Pred) :-
	retractall('$lgt_send_to_obj_'(_, Pred, _)),
	retractall('$lgt_send_to_obj_ne_'(_, Pred, _)),
	retractall('$lgt_send_to_self_'(_, Pred, _)),
	retractall('$lgt_obj_super_call_same_'(_, Pred, _)),
	retractall('$lgt_obj_super_call_other_'(_, Pred, _)),
	retractall('$lgt_ctg_super_call_same_'(_, Pred, _)),
	retractall('$lgt_ctg_super_call_other_'(_, Pred, _)),
	retractall('$lgt_ctg_call_'(_, Pred, _)),
	retractall('$lgt_db_lookup_cache_'(_, Pred, _, _, _)),
	'$lgt_reassert_lookup_cache_catchall_clauses'.


'$lgt_reassert_lookup_cache_catchall_clauses' :-
	assertz(('$lgt_send_to_obj_'(Obj, Pred, Sender) :- '$lgt_send_to_obj_nv'(Obj, Pred, Sender))),
	assertz(('$lgt_send_to_obj_ne_'(Obj, Pred, Sender) :- '$lgt_send_to_obj_ne_nv'(Obj, Pred, Sender))),
	assertz(('$lgt_send_to_self_'(Obj, Pred, Sender) :- '$lgt_send_to_self_nv'(Obj, Pred, Sender))),
	assertz(('$lgt_obj_super_call_same_'(Super, Pred, ExCtx) :- '$lgt_obj_super_call_same'(Super, Pred, ExCtx))),
	assertz(('$lgt_obj_super_call_other_'(Super, Pred, ExCtx) :- '$lgt_obj_super_call_other_nv'(Super, Pred, ExCtx))),
	assertz(('$lgt_ctg_super_call_same_'(Ctg, Pred, ExCtx) :- '$lgt_ctg_super_call_same'(Ctg, Pred, ExCtx))),
	assertz(('$lgt_ctg_super_call_other_'(Ctg, Pred, ExCtx) :- '$lgt_ctg_super_call_other_nv'(Ctg, Pred, ExCtx))),
	assertz(('$lgt_ctg_call_'(Dcl, Pred, ExCtx) :- '$lgt_call_ctg_pred_nv'(Dcl, Pred, ExCtx))).



% '$lgt_save_global_op_table'
%
% saves current operator table

'$lgt_save_global_op_table' :-
	current_op(Pr, Spec, Op),
		asserta('$lgt_pp_global_op_'(Pr, Spec, Op)),
	fail.

'$lgt_save_global_op_table'.



% '$lgt_restore_global_op_table'
%
% restores current operator table

'$lgt_restore_global_op_table' :-
	retract('$lgt_pp_entity_op_'(_, Spec, Op, _)),
		op(0, Spec, Op),
	fail.

'$lgt_restore_global_op_table' :-
	retractall('$lgt_pp_file_op_'(_, _, ',')),		% ','/2 cannot be an argument to op/3
	retract('$lgt_pp_file_op_'(_, Spec, Op)),
		catch(op(0, Spec, Op), _, fail),			% some Prolog compilers may define other operators as non-redefinable
	fail.

'$lgt_restore_global_op_table' :-
	retractall('$lgt_pp_global_op_'(_, _, ',')),	% ','/2 cannot be an argument to op/3
	retract('$lgt_pp_global_op_'(Pr, Spec, Op)),
		catch(op(Pr, Spec, Op), _, fail),			% some Prolog compilers may define other operators as non-redefinable
	fail.

'$lgt_restore_global_op_table'.



% '$lgt_save_file_op_table'
%
% saves current operator table

'$lgt_save_file_op_table' :-
	current_op(Pr, Spec, Op),
		asserta('$lgt_pp_file_op_'(Pr, Spec, Op)),
	fail.

'$lgt_save_file_op_table'.



% '$lgt_restore_file_op_table'
%
% restores current operator table

'$lgt_restore_file_op_table' :-
	retract('$lgt_pp_entity_op_'(_, Spec, Op, _)),
		op(0, Spec, Op),
	fail.

'$lgt_restore_file_op_table' :-
	retractall('$lgt_pp_file_op_'(_, _, ',')),		% ','/2 cannot be an argument to op/3
	retract('$lgt_pp_file_op_'(Pr, Spec, Op)),
		catch(op(Pr, Spec, Op), _, fail),			% some Prolog compilers may define other operators as non-redefinable
	fail.

'$lgt_restore_file_op_table'.



% '$lgt_assert_entity_operators'(+integer, +operator_specifier, +atom_or_atom_list, +scope)
%
% asserts local entity operators

'$lgt_assert_entity_operators'(_, _, [], _) :-
	!.

'$lgt_assert_entity_operators'(Pr, Spec, [Op| Ops], Scope) :-
	!,
	asserta('$lgt_pp_entity_op_'(Pr, Spec, Op, Scope)),
	'$lgt_assert_entity_operators'(Pr, Spec, Ops, Scope).

'$lgt_assert_entity_operators'(Pr, Spec, Op, Scope) :-
	asserta('$lgt_pp_entity_op_'(Pr, Spec, Op, Scope)).



% '$lgt_pp_entity'(?atom, ?entity_identifier, ?atom, ?atom, ?atom)
%
% provides deterministic access to some common used data on the entity being compiled

'$lgt_pp_entity'(object, Entity, Prefix, Dcl, Mode) :-
	'$lgt_pp_object_'(Entity, Prefix, Dcl, _, _, _, _, _, _, _, Flags),
	(	Flags /\ 2 =:= 2 ->
		Mode = (dynamic)
	;	Mode = static
	),
	!.

'$lgt_pp_entity'(category, Entity, Prefix, Dcl, Mode) :-
	'$lgt_pp_category_'(Entity, Prefix, Dcl, _, _, Flags),
	(	Flags /\ 2 =:= 2 ->
		Mode = (dynamic)
	;	Mode = static
	),
	!.

'$lgt_pp_entity'(protocol, Entity, Prefix, Dcl, Mode) :-
	'$lgt_pp_protocol_'(Entity, Prefix, Dcl, _, Flags),
	(	Flags /\ 2 =:= 2 ->
		Mode = (dynamic)
	;	Mode = static
	),
	!.



% '$lgt_pp_relation_clause'(-compound)
%
% returns runtime table clauses for the entity being compiled

'$lgt_pp_relation_clause'('$lgt_debugging_entity_'(Entity)) :-
	'$lgt_compiler_flag'(debug, on),
	'$lgt_pp_entity'(_, Entity, _, _, _).

'$lgt_pp_relation_clause'(Clause) :-
	'$lgt_pp_relation_clause_'(Clause).

'$lgt_pp_relation_clause'(Clause) :-
	(	'$lgt_pp_object_'(Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm, Flags) ->
		Clause = '$lgt_current_object_'(Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm, Flags)
	;	'$lgt_pp_protocol_'(Ptc, Prefix, Dcl, Rnm, Flags) ->
		Clause = '$lgt_current_protocol_'(Ptc, Prefix, Dcl, Rnm, Flags)
	;	'$lgt_pp_category_'(Ctg, Prefix, Dcl, Def, Rnm, Flags),
		Clause = '$lgt_current_category_'(Ctg, Prefix, Dcl, Def, Rnm, Flags)
	).

'$lgt_pp_relation_clause'('$lgt_static_binding_entity_'(Entity)) :-
	'$lgt_pp_entity'(_, Entity, _, _, Compilation),
	Compilation == static,
	'$lgt_compiler_flag'(reload, skip).



% '$lgt_tr_expand_goal'(+callable, -callable)
%
% expands a goal; fails if no goal expansion hook is defined

'$lgt_tr_expand_goal'(Goal, EGoal) :-
	(	% source-file specific compiler hook:
		'$lgt_pp_hook_goal_expansion_'(Goal, EGoal) ->
		true
	;	% default compiler hook:
		'$lgt_hook_goal_expansion_'(Goal, EGoal) ->
		true
	;	% no compiler hook defined:
		fail
	).



% '$lgt_tr_terms'(+list(term), +ctx)
%
% translates a list of terms (clauses, directives, or grammar rules)

'$lgt_tr_terms'([], _).
'$lgt_tr_terms'([Term| Terms], Ctx) :-
	'$lgt_comp_ctx_mode'(Ctx, Mode),			% only the compilation context mode should
	'$lgt_comp_ctx_mode'(NewCtx, Mode),			% be shared between different terms
	'$lgt_tr_term'(Term, NewCtx),
	'$lgt_tr_terms'(Terms, Ctx).



% '$lgt_tr_term'(@term, +ctx)
%
% translates a source term (clause, directive, or grammar rule)

'$lgt_tr_term'(Term, Ctx) :-
	(	var(Term) ->
		throw(error(instantiantion_error, term(Term)))
	;	% runtime creation of new entities; no expansion:
		'$lgt_comp_ctx_mode'(Ctx, runtime) ->
		'$lgt_tr_expanded_term'(Term, Ctx)
	;	% source-file specific compiler hook:
		'$lgt_pp_hook_term_expansion_'(Term, Terms) ->
		'$lgt_tr_expanded_terms'(Terms, Ctx)
	;	% default compiler hook:
		'$lgt_hook_term_expansion_'(Term, Terms) ->
		'$lgt_tr_expanded_terms'(Terms, Ctx)
	;	% no compiler hook defined:
		'$lgt_tr_expanded_term'(Term, Ctx)
	).



% '$lgt_tr_expanded_terms'(@term, +ctx)
%
% translates the expanded terms (which can be a list of terms)

'$lgt_tr_expanded_terms'(Term, _) :-
	var(Term),
	throw(error(instantiantion_error, term_expansion/2)).

'$lgt_tr_expanded_terms'([], _) :-
	!.

'$lgt_tr_expanded_terms'([Term| Terms], Ctx) :-
	!,
	'$lgt_tr_expanded_term'(Term, Ctx),
	'$lgt_tr_expanded_terms'(Terms, Ctx).

'$lgt_tr_expanded_terms'(Term, Ctx) :-
	!,
	'$lgt_tr_expanded_term'(Term, Ctx).



% '$lgt_tr_expanded_term'(+term, +ctx)
%
% translates a source file term (clauses, directives, and grammar rules)

'$lgt_tr_expanded_term'(Term, _) :-
	var(Term),
	throw(error(instantiantion_error, term_expansion/2)).

'$lgt_tr_expanded_term'(end_of_file, _) :-
	!.

'$lgt_tr_expanded_term'({Term}, _) :-	% bypass control construct; expanded term is final
	!,
	(	var(Term) ->
		throw(error(instantiantion_error, {Term}))
	;	'$lgt_pp_entity'(_, _, _, _, _) ->
		'$lgt_pp_term_location'(Location),
		% ensure that the relative order of the entity terms is kept
		assertz('$lgt_pp_entity_clause_'({Term}, Location))
	;	% non-entity terms
		'$lgt_pp_term_location'(Location),
		assertz('$lgt_pp_prolog_term_'(Term, Location))
	).

'$lgt_tr_expanded_term'((Head :- Body), Ctx) :-
	!,
	'$lgt_tr_clause'((Head :- Body), Ctx).

'$lgt_tr_expanded_term'((:- Directive), Ctx) :-
	!,
	(	var(Directive) ->
		throw(error(instantiantion_error, directive(Directive)))
	;	'$lgt_comp_ctx_mode'(Ctx, runtime) ->
		'$lgt_tr_directive'(Directive, Ctx)
	;	\+ '$lgt_pp_entity'(_, _, _, _, _) ->
		'$lgt_tr_directive'(Directive, Ctx)
	;	'$lgt_ignore_pl_directive'(Directive) ->								% defined in the Prolog config files
		(	'$lgt_compiler_flag'(portability, warning),
			\+ '$lgt_compiler_flag'(report, off) ->
			'$lgt_report_warning_in_new_line',
			'$lgt_inc_compile_warnings_counter',
			write('%         WARNING!  Ignoring Prolog directive: '), writeq(Directive), nl,
			'$lgt_pp_entity'(Type, Entity, _, _, _),
			'$lgt_report_warning_full_context'(Type, Entity)
		;	true
		)
	;	'$lgt_rewrite_and_copy_pl_directive'(Directive, RDirective) ->			% defined in the Prolog config files
		assertz('$lgt_pp_directive_'(RDirective)),
		(	'$lgt_compiler_flag'(portability, warning),
			\+ '$lgt_compiler_flag'(report, off) ->
			'$lgt_report_warning_in_new_line',
			'$lgt_inc_compile_warnings_counter',
			write('%         WARNING!  Rewriting and copying Prolog directive: '), writeq(Directive), nl,
			'$lgt_pp_entity'(Type, Entity, _, _, _),
			'$lgt_report_warning_full_context'(Type, Entity)
		;	true
		)
	;	'$lgt_rewrite_and_recompile_pl_directive'(Directive, RDirective) ->		% defined in the Prolog config files
		(	'$lgt_compiler_flag'(portability, warning),
			\+ '$lgt_compiler_flag'(report, off) ->
			'$lgt_report_warning_in_new_line',
			'$lgt_inc_compile_warnings_counter',
			write('%         WARNING!  Rewriting and recompiling Prolog directive: '), writeq(Directive), nl,
			'$lgt_pp_entity'(Type, Entity, _, _, _),
			'$lgt_report_warning_full_context'(Type, Entity)
		;	true
		),
		'$lgt_tr_directive'(RDirective, Ctx)
	;	'$lgt_tr_directive'(Directive, Ctx)
	).

'$lgt_tr_expanded_term'((Head --> Body), Ctx) :-
	!,
	'$lgt_tr_grammar_rule'((Head --> Body), Ctx).

'$lgt_tr_expanded_term'(Fact, Ctx) :-
	'$lgt_tr_clause'(Fact, Ctx).



% '$lgt_tr_directives'(+list, +ctx)
%
% translates a list of directives

'$lgt_tr_directives'([], _).

'$lgt_tr_directives'([Dir| Dirs], Ctx) :-
	'$lgt_comp_ctx_mode'(Ctx, Mode),			% only the compilation context mode should
	'$lgt_comp_ctx_mode'(NewCtx, Mode),			% be shared between different directives
	'$lgt_tr_directive'(Dir, NewCtx),
	'$lgt_tr_directives'(Dirs, Ctx).



% '$lgt_tr_directive'(+term, +ctx)
%
% translates a directive

'$lgt_tr_directive'(Dir, _) :-
	var(Dir),
	throw(error(instantiantion_error, directive(Dir))).


% conditional compilation directives

'$lgt_tr_directive'(if(Goal), _) :-
	var(Goal),
	throw(error(instantiantion_error, directive(if(Goal)))).

'$lgt_tr_directive'(if(Goal), _) :-
	\+ callable(Goal),
	throw(error(type_error(callable, Goal), directive(if(Goal)))).

'$lgt_tr_directive'(if(Goal), Ctx) :-
	'$lgt_comp_ctx_mode'(Ctx, compile(_)),		% only expand goals when compiling a source file
	'$lgt_tr_expand_goal'(Goal, EGoal),
	!,
	'$lgt_tr_directive'(if(EGoal), Ctx).

'$lgt_tr_directive'(if(predicate_property(Pred, Prop)), Ctx) :-
	!,	% workaround lack of standardization of the predicate_property/2 predicate
	'$lgt_tr_directive'(if('$lgt_predicate_property'(Pred, Prop)), Ctx).

'$lgt_tr_directive'(if(Goal), _) :-
	'$lgt_pp_cc_mode_'(Value),					% not top-level if
	!,
	asserta('$lgt_pp_cc_if_found_'(Goal)),
	(	Value == ignore ->
		asserta('$lgt_pp_cc_mode_'(ignore))
	;	Value == seek ->						% we're looking for an else
		asserta('$lgt_pp_cc_mode_'(ignore))		% so ignore this if ... endif 
	;	% Value == skip ->
		(	catch(Goal, Error, '$lgt_compiler_error_handler'(Error)) ->
			asserta('$lgt_pp_cc_mode_'(skip))
		;	asserta('$lgt_pp_cc_mode_'(seek)),
			retractall('$lgt_pp_cc_skipping_'),
			assertz('$lgt_pp_cc_skipping_')
		)
	).

'$lgt_tr_directive'(if(Goal), _) :-
	!,
	asserta('$lgt_pp_cc_if_found_'(Goal)),
	(	call(Goal) ->
		asserta('$lgt_pp_cc_mode_'(skip))
	;	asserta('$lgt_pp_cc_mode_'(seek)),
		retractall('$lgt_pp_cc_skipping_'),
		assertz('$lgt_pp_cc_skipping_')
	).

'$lgt_tr_directive'(elif(Goal), _) :-
	var(Goal),
	throw(error(instantiantion_error, directive(elif(Goal)))).

'$lgt_tr_directive'(elif(Goal), _) :-
	\+ callable(Goal),
	throw(error(type_error(callable, Goal), directive(elif(Goal)))).

'$lgt_tr_directive'(elif(Goal), _) :-
	\+ '$lgt_pp_cc_if_found_'(_),
	throw(error(unmatched_directive, directive(elif(Goal)))).

'$lgt_tr_directive'(elif(Goal), Ctx) :-
	'$lgt_comp_ctx_mode'(Ctx, compile(_)),			% only expand goals when compiling a source file
	'$lgt_tr_expand_goal'(Goal, EGoal),
	!,
	'$lgt_tr_directive'(elif(EGoal), Ctx).

'$lgt_tr_directive'(elif(predicate_property(Pred, Prop)), Ctx) :-
	!,	% workaround lack of standardization of the predicate_property/2 predicate
	'$lgt_tr_directive'(elif('$lgt_predicate_property'(Pred, Prop)), Ctx).

'$lgt_tr_directive'(elif(Goal), _) :-
	retract('$lgt_pp_cc_mode_'(Value)),
	(	Value == ignore ->						% we're inside an if ... endif 
		asserta('$lgt_pp_cc_mode_'(ignore))		% that we're ignoring
	;	Value == skip ->						% the corresponding if is true
		retractall('$lgt_pp_cc_skipping_'),		% so we must skip this elif
		assertz('$lgt_pp_cc_skipping_'),
		asserta('$lgt_pp_cc_mode_'(skip))
	;	% Value == seek ->						% the corresponding if is false
		(	catch(Goal, Error, '$lgt_compiler_error_handler'(Error)) ->
			retract('$lgt_pp_cc_skipping_'),
			asserta('$lgt_pp_cc_mode_'(skip))
		;	asserta('$lgt_pp_cc_mode_'(seek))
		)
	),
	!.

'$lgt_tr_directive'(else, _) :-
	\+ '$lgt_pp_cc_if_found_'(_),
	throw(error(unmatched_directive, directive(else))).

'$lgt_tr_directive'(else, _) :-
	'$lgt_pp_cc_mode_'(Value),
	(	Value == ignore ->						% we're inside an if ... endif 
		true									% that we're ignoring
	;	Value == skip ->						% the corresponding if is true
		retractall('$lgt_pp_cc_skipping_'),		% so we must skip this else
		assertz('$lgt_pp_cc_skipping_')
	;	% Value == seek ->						% the corresponding if is false
		retract('$lgt_pp_cc_skipping_')
	),
	!.

'$lgt_tr_directive'(endif, _) :-
	\+ '$lgt_pp_cc_if_found_'(_),
	throw(error(unmatched_directive, directive(endif))).

'$lgt_tr_directive'(endif, _) :-
	retract('$lgt_pp_cc_if_found_'(_)),
	retract('$lgt_pp_cc_mode_'(Value)),
	(	Value == ignore ->
		true
	;	retractall('$lgt_pp_cc_skipping_')
	),
	!.


% remaining directives

'$lgt_tr_directive'(Dir, _) :-							% closing entity directive occurs before the opening
	\+ '$lgt_pp_entity'(_, _, _, _, _),					% entity directive; the opening directive is probably
	functor(Dir, Functor, Arity),						% missing or misspelt
	'$lgt_lgt_closing_directive'(Functor, Arity),
	throw(error(unmatched_directive, directive(Dir))).

'$lgt_tr_directive'(Dir, Ctx) :-
	\+ '$lgt_pp_entity'(_, _, _, _, _),					% directive occurs before opening entity directive
	functor(Dir, Functor, Arity),
	\+ '$lgt_lgt_opening_directive'(Functor, Arity),
	!,
	'$lgt_tr_file_directive'(Dir, Ctx).					% translate it as a source file-level directive

'$lgt_tr_directive'(Dir, Ctx) :-						% entity closing directive
	functor(Dir, Functor, Arity),
	'$lgt_lgt_closing_directive'(Functor, Arity),
	Dir =.. [Functor| Args],
	catch(
		'$lgt_tr_directive'(Functor, Args, Ctx),
		Error,
		(	'$lgt_pp_entity'(Type, Entity, _, _, _) ->
			throw(error(Error, entity(Type, Entity)))
		;	throw(error(Error, directive(Dir)))
		)),
	!.

'$lgt_tr_directive'(Dir, Ctx) :-						% entity opening directive or entity directive
	functor(Dir, Functor, Arity),
	'$lgt_lgt_directive'(Functor, Arity),
	Dir =.. [Functor| Args],
	catch(
		'$lgt_tr_directive'(Functor, Args, Ctx),
		Error,
		throw(error(Error, directive(Dir)))),
	!.

'$lgt_tr_directive'(Dir, Ctx) :-
	'$lgt_term_template'(Dir, Meta),
	'$lgt_pl_meta_directive'(Meta),						% defined in the Prolog config files
	!,
	(	'$lgt_compiler_flag'(portability, warning),
		\+ '$lgt_compiler_flag'(report, off) ->
		'$lgt_report_warning_in_new_line',
		'$lgt_inc_compile_warnings_counter',
		write('%         WARNING!  Compiling proprietary Prolog directive: '), writeq(Dir), nl,
		'$lgt_pp_entity'(Type, Entity, _, _, _),
		'$lgt_report_warning_full_context'(Type, Entity)
	;	true
	),
	Dir =.. [Functor| Args],
	Meta =.. [Functor| MArgs],
	'$lgt_pp_entity'(_, Entity, Prefix, _, _),
	'$lgt_comp_ctx'(Ctx, _, Entity, Entity, Entity, Prefix, [], _, _, _, _),	% MetaVars = [] as we're compiling a local call
	'$lgt_tr_meta_args'(Args, MArgs, Ctx, TArgs, DArgs),
	(	'$lgt_compiler_flag'(debug, on) ->
		TDir =.. [Functor| DArgs]
	;	TDir =.. [Functor| TArgs]
	),
	assertz('$lgt_pp_directive_'(TDir)).

'$lgt_tr_directive'(Dir, Ctx) :-
	'$lgt_pp_module_'(_),								% we're compiling a module as an object
	(	functor(Dir, Functor, Arity), '$lgt_pp_defines_predicate_'(Functor, Arity)
	;	'$lgt_pp_uses_predicate_'(_, _, Dir)			% directive is a query for a locally defined predicate
 	;	'$lgt_pp_use_module_predicate_'(_, _, Dir)		% or a predicate referenced in a use_module/2 directive
	),
	!,													% translate query as an initialization goal
	(	'$lgt_compiler_flag'(portability, warning),
		\+ '$lgt_compiler_flag'(report, off) ->
		'$lgt_report_warning_in_new_line',
		'$lgt_inc_compile_warnings_counter',
		write('%         WARNING!  Compiling query as an initialization goal: '), writeq(Dir), nl,
		'$lgt_pp_entity'(Type, Entity, _, _, _),
		'$lgt_report_warning_full_context'(Type, Entity)
	;	true
	),
	'$lgt_tr_directive'(initialization, [Dir], Ctx).

'$lgt_tr_directive'(Dir, _) :-
	functor(Dir, Functor, Arity),
	throw(error(domain_error(directive, Functor/Arity), directive(Dir))).



% '$lgt_tr_file_directive'(@nonvar, +ctx)
%
% translates file-level directives, i.e. directives that are not encapsulated in a Logtalk entity
% error-checking is delegated in most cases to the back-end Prolog compiler

'$lgt_tr_file_directive'(encoding(_), _) :-					% the encoding/1 directive is already processed 
	!.

'$lgt_tr_file_directive'(ensure_loaded(File), _) :-
	!,
	ensure_loaded(File),									% assume that ensure_loaded/1 is also a built-in predicate
	'$lgt_pp_term_location'(Location),
	assertz('$lgt_pp_prolog_term_'((:- ensure_loaded(File)), Location)).

'$lgt_tr_file_directive'(initialization(Goal), Ctx) :-
	'$lgt_comp_ctx_mode'(Ctx, compile(_)),					% only expand goals when compiling a source file
	'$lgt_tr_expand_goal'(Goal, EGoal),
	!,
	'$lgt_tr_file_directive'(initialization(EGoal), Ctx).

'$lgt_tr_file_directive'(initialization(Goal), _) :-
	!,
	'$lgt_must_be'(callable, Goal),
	assertz('$lgt_pp_file_init_'(Goal)).

'$lgt_tr_file_directive'(op(Priority, Spec, Operators), _) :-
	!,
	'$lgt_check_op_directive_args'(Priority, Spec, Operators),
	op(Priority, Spec, Operators),							% op/3 directives must be used during entity compilation
	'$lgt_pp_term_location'(Location),
	assertz('$lgt_pp_prolog_term_'((:- op(Priority, Spec, Operators)), Location)),
	(   atom(Operators) ->
		assertz('$lgt_pp_file_op_'(Priority, Spec, Operators))
	;	forall('$lgt_member'(Operator, Operators), assertz('$lgt_pp_file_op_'(Priority, Spec, Operator)))
	).

'$lgt_tr_file_directive'(set_logtalk_flag(Flag, Value), _) :-
	!,
	Option =.. [Flag, Value],
	'$lgt_set_compiler_flags'([Option]).					% local scope (restricted to the source file being compiled)

'$lgt_tr_file_directive'(set_prolog_flag(Flag, Value), _) :-
	!,
	set_prolog_flag(Flag, Value),
	'$lgt_pp_term_location'(Location),
	assertz('$lgt_pp_prolog_term_'((:- set_prolog_flag(Flag, Value)), Location)).

'$lgt_tr_file_directive'(Dir, _) :-
	'$lgt_pp_term_location'(Location),
	assertz('$lgt_pp_prolog_term_'((:- Dir), Location)).	% directive will be copied to the generated Prolog file



% '$lgt_tr_directive'(+atom, +list, +ctx)
%
% translates a directive and its (possibly empty) list of arguments

'$lgt_tr_directive'(object, [Obj| _], _) :-
	(	var(Obj),
		throw(instantiation_error)
	;	\+ callable(Obj),
		throw(type_error(object_identifier, Obj))
	;	'$lgt_built_in_object'(Obj),
		throw(permission_error(modify, object, Obj))
	;	'$lgt_built_in_protocol'(Obj),
		throw(permission_error(modify, protocol, Obj))
	;	'$lgt_built_in_category'(Obj),
		throw(permission_error(modify, category, Obj))
	;	'$lgt_pp_file_relation_clause_'('$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, _)),
		throw(permission_error(modify, object, Obj))
	;	'$lgt_pp_file_relation_clause_'('$lgt_current_protocol_'(Obj, _, _, _, _)),
		throw(permission_error(modify, protocol, Obj))
	;	'$lgt_pp_file_relation_clause_'('$lgt_current_category_'(Obj, _, _, _, _, _)),
		throw(permission_error(modify, category, Obj))
	).

'$lgt_tr_directive'(object, [Obj| Rels], _) :-
	'$lgt_report_compiling_entity'(object, Obj),
	'$lgt_add_entity_file_properties'(start, Obj),
	'$lgt_save_file_op_table',
	'$lgt_tr_object_identifier'(Obj, static),			% assume static object
	'$lgt_tr_object_relations'(Rels, Obj).

'$lgt_tr_directive'(end_object, [], _) :-
	(	'$lgt_pp_object_'(Obj, _, _, _, _, _, _, _, _, _, _) ->
		'$lgt_add_entity_file_properties'(end, Obj),
		'$lgt_tr_entity'(object, Obj),
		'$lgt_restore_file_op_table',
		'$lgt_report_compiled_entity'(object, Obj)
	;	throw(closing_directive_mismatch)
	).

'$lgt_tr_directive'(protocol, [Ptc| _], _) :-
	(	var(Ptc),
		throw(instantiation_error)
	;	\+ atom(Ptc),
		throw(type_error(protocol_identifier, Ptc))
	;	'$lgt_built_in_object'(Ptc),
		throw(permission_error(modify, object, Ptc))
	;	'$lgt_built_in_protocol'(Ptc),
		throw(permission_error(modify, protocol, Ptc))
	;	'$lgt_built_in_category'(Ptc),
		throw(permission_error(modify, category, Ptc))
	;	'$lgt_pp_file_relation_clause_'('$lgt_current_object_'(Ptc, _, _, _, _, _, _, _, _, _, _)),
		throw(permission_error(modify, object, Ptc))
	;	'$lgt_pp_file_relation_clause_'('$lgt_current_protocol_'(Ptc, _, _, _, _)),
		throw(permission_error(modify, protocol, Ptc))
	;	'$lgt_pp_file_relation_clause_'('$lgt_current_category_'(Ptc, _, _, _, _, _)),
		throw(permission_error(modify, category, Ptc))
	).

'$lgt_tr_directive'(protocol, [Ptc| Rels], _) :-
	'$lgt_report_compiling_entity'(protocol, Ptc),
	'$lgt_add_entity_file_properties'(start, Ptc),
	'$lgt_save_file_op_table',
	'$lgt_tr_protocol_identifier'(Ptc, static),			% assume static protocol
	'$lgt_tr_protocol_relations'(Rels, Ptc).

'$lgt_tr_directive'(end_protocol, [], _) :-
	(   '$lgt_pp_protocol_'(Ptc, _, _, _, _) ->
		'$lgt_add_entity_file_properties'(end, Ptc),
		'$lgt_tr_entity'(protocol, Ptc),
		'$lgt_restore_file_op_table',
		'$lgt_report_compiled_entity'(protocol, Ptc)
	;	throw(closing_directive_mismatch)
	).


'$lgt_tr_directive'(category, [Ctg| _], _) :-
	(	var(Ctg),
		throw(instantiation_error)
	;	\+ callable(Ctg),
		throw(type_error(category_identifier, Ctg))
	;	'$lgt_built_in_object'(Ctg),
		throw(permission_error(modify, object, Ctg))
	;	'$lgt_built_in_protocol'(Ctg),
		throw(permission_error(modify, protocol, Ctg))
	;	'$lgt_built_in_category'(Ctg),
		throw(permission_error(modify, category, Ctg))
	;	'$lgt_pp_file_relation_clause_'('$lgt_current_object_'(Ctg, _, _, _, _, _, _, _, _, _, _)),
		throw(permission_error(modify, object, Ctg))
	;	'$lgt_pp_file_relation_clause_'('$lgt_current_protocol_'(Ctg, _, _, _, _)),
		throw(permission_error(modify, protocol, Ctg))
	;	'$lgt_pp_file_relation_clause_'('$lgt_current_category_'(Ctg, _, _, _, _, _)),
		throw(permission_error(modify, category, Ctg))
	).

'$lgt_tr_directive'(category, [Ctg| Rels], _) :-
	'$lgt_report_compiling_entity'(category, Ctg),
	'$lgt_add_entity_file_properties'(start, Ctg),
	'$lgt_save_file_op_table',
	'$lgt_tr_category_identifier'(Ctg, static),			% assume static category
	'$lgt_tr_category_relations'(Rels, Ctg).

'$lgt_tr_directive'(end_category, [], _) :-
	(	'$lgt_pp_category_'(Ctg, _, _, _, _, _) ->
		'$lgt_add_entity_file_properties'(end, Ctg),
		'$lgt_tr_entity'(category, Ctg),
		'$lgt_restore_file_op_table',
		'$lgt_report_compiled_entity'(category, Ctg)
	;	throw(closing_directive_mismatch)
	).


% compile modules as objects

'$lgt_tr_directive'(module, [Module], Ctx) :-
	!,
	'$lgt_tr_directive'(module, [Module, []], Ctx).		% empty export list

'$lgt_tr_directive'(module, [Module, Exports], Ctx) :-
	'$lgt_must_be'(atom, Module),
	'$lgt_must_be'(list, Exports),
	assertz('$lgt_pp_module_'(Module)),					% remeber we are compiling a module
	'$lgt_report_compiling_entity'(module, Module),
	'$lgt_tr_object_identifier'(Module, static),		% assume static module/object
	'$lgt_split_operators_and_predicates'(Exports, Preds, Ops),
	forall(
		'$lgt_member'(Op, Ops),
		'$lgt_tr_file_directive'(Op, Ctx)),
	'$lgt_tr_directive'((public), Preds, Ctx),			% make the export list public predicates
	'$lgt_save_file_op_table'.


% set_logtalk_flag/1 entity directive

'$lgt_tr_directive'(set_logtalk_flag, [Flag, Value], _) :-
	'$lgt_check_compiler_flag'(Flag, Value),
	retractall('$lgt_pp_entity_compiler_flag_'(Flag, _)),
	assertz('$lgt_pp_entity_compiler_flag_'(Flag, Value)).


% create a message queue at object initialization

'$lgt_tr_directive'(threaded, [], _) :-
	\+ '$lgt_compiler_flag'(threads, supported),
	throw(error(resource_error(threads), threaded/0)).

'$lgt_tr_directive'(threaded, [], _) :-
	!,
	(	'$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, _, _) ->
		assertz('$lgt_pp_threaded_')
	;	throw(domain_error(object_directive, threaded/0))
	).


% make all object (or category) predicates synchronized using the same mutex

'$lgt_tr_directive'(synchronized, [], _) :-
	\+ '$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, _, _),
	\+ '$lgt_pp_category_'(_, _, _, _, _, _),
	throw(domain_error(directive, synchronized/0)).

'$lgt_tr_directive'(synchronized, [], _) :-
	!,
	(	'$lgt_compiler_flag'(threads, supported) ->
		'$lgt_pp_entity'(_, _, Prefix, _, _),
		atom_concat(Prefix, 'mutex_', Mutex),
		assertz('$lgt_pp_synchronized_'),
		assertz('$lgt_pp_synchronized_'(_, Mutex))
	;	true
	).


% dynamic/0 entity directive
%
% (entities are static by default but can be declared dynamic using this directive)

'$lgt_tr_directive'((dynamic), [], _) :-
	!,
	% update entity compilation mode to "dynamic":
	(   retract('$lgt_pp_object_'(Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm, Flags0)) ->
		Flags is Flags0 \/ 2,
		assertz('$lgt_pp_object_'(Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm, Flags))
	;   retract('$lgt_pp_protocol_'(Ptc, Prefix, Dcl, Rnm, Flags0)) ->
		Flags is Flags0 \/ 2,
		assertz('$lgt_pp_protocol_'(Ptc, Prefix, Dcl, Rnm, Flags))
	;   retract('$lgt_pp_category_'(Ctg, Prefix, Dcl, Def, Rnm, Flags0)),
		Flags is Flags0 \/ 2,
		assertz('$lgt_pp_category_'(Ctg, Prefix, Dcl, Def, Rnm, Flags))
	).


% initialization/1 entity directive

'$lgt_tr_directive'(initialization, [Goal], Ctx) :-
	'$lgt_must_be'(callable, Goal),
	'$lgt_pp_entity'(_, Entity, Prefix, _, _),
	'$lgt_comp_ctx'(Ctx, _, Entity, Entity, Entity, Prefix, [], _, ExCtx, _, _),
	'$lgt_exec_ctx'(ExCtx, Entity, Entity, Entity, [], _),		% MetaVars = [] as we're compiling a local call
	'$lgt_tr_body'(Goal, TGoal, DGoal, Ctx),
	(	'$lgt_compiler_flag'(debug, on) ->
		assertz('$lgt_pp_entity_init_'(DGoal))
	;	assertz('$lgt_pp_entity_init_'(TGoal))
	).


% op/3 entity directive (operators are local to entities)

'$lgt_tr_directive'(op, [Priority, Spec, Operators], _) :-
	'$lgt_check_op_directive_args'(Priority, Spec, Operators),
	op(Priority, Spec, Operators),
	'$lgt_assert_entity_operators'(Priority, Spec, Operators, (local)).


% uses/2 entity directive

'$lgt_tr_directive'(uses, [Obj, Resources], Ctx) :-
	!,
	'$lgt_must_be'(object_identifier, Obj),
	'$lgt_must_be'(list, Resources),
	'$lgt_add_referenced_object'(Obj),
	assertz('$lgt_pp_uses_'(Obj)),
	'$lgt_split_operators_and_predicates'(Resources, Preds, Ops),
	forall('$lgt_member'(Op, Ops), '$lgt_tr_directive'(Op, Ctx)),
	'$lgt_tr_uses_directive'(Preds, Obj, Ctx).


% uses/1 entity directive

'$lgt_tr_directive'(uses, [Obj], _) :-
	'$lgt_must_be'(object_identifier, Obj),
	'$lgt_add_referenced_object'(Obj),
	assertz('$lgt_pp_uses_'(Obj)).


% use_module/2 module directive

'$lgt_tr_directive'(use_module, [Module, _], _) :-
	var(Module),
	throw(instantiation_error).

'$lgt_tr_directive'(use_module, [Module, _], _) :-
	\+ callable(Module),
	throw(type_error(atom, Module)).

'$lgt_tr_directive'(use_module, [Module, Imports], Ctx) :-
	'$lgt_must_be'(list, Imports),
	atom(Module),	% fail if using library notation in order to use the config files to find the module name
	'$lgt_split_operators_and_predicates'(Imports, Preds, Ops),
	forall('$lgt_member'(Op, Ops), '$lgt_tr_directive'(Op, Ctx)),
	(	'$lgt_pp_module_'(_) ->
		% we're compiling a module as an object; assume referenced modules are also compiled as objects
 		'$lgt_tr_directive'(uses, [Module, Preds], Ctx)
	;	% we're calling module predicates within an object or a category
		'$lgt_tr_use_module_directive'(Preds, Module, Ctx)
	).


% reexport/2 module directive

'$lgt_tr_directive'(reexport, [Module, _], _) :-
	var(Module),
	throw(instantiation_error).

'$lgt_tr_directive'(reexport, [Module, _], _) :-
	\+ callable(Module),
	throw(type_error(atom, Module)).

'$lgt_tr_directive'(reexport, [Module, Exports], Ctx) :-
	% we must be compiling a module as an object
	'$lgt_pp_module_'(_),
	% fail if using library notation in order to use the config files to find the module name
	atom(Module),
	% we're compiling a module as an object; assume referenced modules are also compiled as objects
	(	atom(Module) ->
		Name = Module
	;	% assume library notation and that the file name is also the module name
		arg(1, Module, Name),
		atom(Name)
	),
	'$lgt_split_operators_and_predicates'(Exports, Preds, Ops),
	forall(
		'$lgt_member'(Op, Ops),
		'$lgt_tr_file_directive'(Op, Ctx)),
	'$lgt_tr_reexport_directive'(Preds, Name, Ctx).


% calls/1 entity directive

'$lgt_tr_directive'(calls, Ptcs, _) :-
	'$lgt_flatten_list'(Ptcs, Ptcs2),
	'$lgt_tr_calls_directive'(Ptcs2).


% info/1 entity directive

'$lgt_tr_directive'(info, [List], _) :-
	!,
	(	'$lgt_valid_entity_info_list'(List) ->
		(	retract('$lgt_pp_info_'(Previous)) ->
			'$lgt_append'(Previous, List, Info)
		;	Info = List
		),
		assertz('$lgt_pp_info_'(Info)),
		'$lgt_pp_entity'(_, Entity, _, _, _),
		(	compound(Entity) ->
			(	'$lgt_member'(parnames is Names, Info) ->
				true
			;	'$lgt_member'(parameters is Parameters, Info) ->
				findall(Name, '$lgt_member'(Name - _, Parameters), Names)
			;	Entity =.. [_| Names],
				'$lgt_vars_to_underscore'(Names)
			),
			assertz('$lgt_pp_relation_clause_'('$lgt_entity_property_'(Entity, parameter_names(Names))))
		;	true
		)
	;	throw(type_error(entity_info_list, List))
	).


% info/2 predicate directive

'$lgt_tr_directive'(info, [Pred, List], _) :-
	(	nonvar(Pred) ->
		(	'$lgt_valid_predicate_or_non_terminal_indicator'(Pred, Functor, Arity) ->
			'$lgt_tr_pred_info_list'(List, Functor, Arity),
			(	retract('$lgt_pp_info_'(Pred, Previous)) ->
				'$lgt_append'(Previous, List, Info)
			;	Info = List
			),
			assertz('$lgt_pp_info_'(Pred, Info))
		;	throw(type_error(predicate_indicator, Pred))
		)
	;	throw(instantiation_error)
	).


% synchronized/1 predicate directive

'$lgt_tr_directive'(synchronized, Preds, _) :-
	(	'$lgt_compiler_flag'(threads, supported) ->
		(	'$lgt_pp_synchronized_' ->
			(	'$lgt_compiler_flag'(report, off) ->
				true
			;	'$lgt_pp_entity'(Type, Entity, _, _, _),
				'$lgt_report_warning_in_new_line',
				'$lgt_inc_compile_warnings_counter',
				write('%         WARNING!  Ignoring synchronized predicate directive: '), nl,
				write('%                       '), write(Type), write(' already declared as synchronized!'), nl,
				'$lgt_report_warning_full_context'(Type, Entity)
			)
		;	'$lgt_flatten_list'(Preds, Preds2),
			'$lgt_tr_synchronized_directive'(Preds2)
		)
	;	true
	).


% scope directives

'$lgt_tr_directive'((public), Resources, _) :-
	'$lgt_flatten_list'(Resources, ResourcesFlatted),
	'$lgt_tr_public_directive'(ResourcesFlatted).

'$lgt_tr_directive'(protected, Resources, _) :-
	'$lgt_flatten_list'(Resources, ResourcesFlatted),
	'$lgt_tr_protected_directive'(ResourcesFlatted).

'$lgt_tr_directive'(private, Resources, _) :-
	'$lgt_flatten_list'(Resources, ResourcesFlatted),
	'$lgt_tr_private_directive'(ResourcesFlatted).


% export/1 module directive
%
% module exported predicates are compiled as object public predicates

'$lgt_tr_directive'((export), Exports, Ctx) :-
	% we must be compiling a module as an object
	'$lgt_pp_module_'(_),
	'$lgt_flatten_list'(Exports, ExportsFlatted),
	'$lgt_split_operators_and_predicates'(ExportsFlatted, Preds, Ops),
	forall(
		'$lgt_member'(Op, Ops),
		'$lgt_tr_file_directive'(Op, Ctx)),
	'$lgt_tr_public_directive'(Preds).


'$lgt_tr_directive'((dynamic), Preds, _) :-
	'$lgt_flatten_list'(Preds, Preds2),
	'$lgt_tr_dynamic_directive'(Preds2).


'$lgt_tr_directive'((discontiguous), Preds, _) :-
	'$lgt_flatten_list'(Preds, Preds2),
	'$lgt_tr_discontiguous_directive'(Preds2).


'$lgt_tr_directive'(metapredicate, Preds, Ctx) :-	% deprecated directive name
	(	'$lgt_compiler_flag'(report, off) ->
		true
	;	'$lgt_report_warning_in_new_line',
		'$lgt_inc_compile_warnings_counter',
		write('%         WARNING!  Deprecated directive: metapredicate/1 (use meta_predicate/1 instead)'), nl,
		'$lgt_pp_entity'(Type, Entity, _, _, _),
		'$lgt_report_warning_full_context'(Type, Entity)
	),
	'$lgt_tr_directive'((meta_predicate), Preds, Ctx).

'$lgt_tr_directive'((meta_predicate), Preds, _) :-
	'$lgt_flatten_list'(Preds, Preds2),
	(	'$lgt_pp_module_'(_) ->
		% we're compiling a module as an object
		'$lgt_tr_module_meta_predicate_directives'(Preds2, Preds3)
	;	% we're compiling a Logtalk entity
		Preds2 = Preds3
	),
	'$lgt_tr_meta_predicate_directive'(Preds3).


'$lgt_tr_directive'(annotation, Annotations, _) :-
	'$lgt_flatten_list'(Annotations, Annotations2),
	'$lgt_tr_annotation_directive'(Annotations2).


'$lgt_tr_directive'((mode), [Mode, Solutions], _) :-
	(var(Mode); var(Solutions)),
	throw(instantiation_error).

'$lgt_tr_directive'((mode), [Mode, _], _) :-
	\+ '$lgt_valid_mode_template'(Mode),
	throw(type_error(mode_term, Mode)).

'$lgt_tr_directive'((mode), [_, Solutions], _) :-
	\+ '$lgt_valid_number_of_solutions'(Solutions),
	throw(type_error(number_of_solutions, Solutions)).

'$lgt_tr_directive'((mode), [Mode, Solutions], _) :-
	assertz('$lgt_pp_mode_'(Mode, Solutions)).


'$lgt_tr_directive'((multifile), Preds, _) :-
	'$lgt_flatten_list'(Preds, Preds2),
	'$lgt_tr_multifile_directive'(Preds2).


'$lgt_tr_directive'((coinductive), Preds, Ctx) :-
	'$lgt_flatten_list'(Preds, Preds2),
	'$lgt_tr_coinductive_directive'(Preds2, Ctx).


'$lgt_tr_directive'(alias, [Entity, Pred, Alias], _) :-
	'$lgt_tr_pred_alias_directive'(Entity, Pred, Alias).



'$lgt_tr_pred_alias_directive'(Entity, _, _) :-
	var(Entity),
	throw(instantiation_error).

'$lgt_tr_pred_alias_directive'(_, Pred, _) :-
	var(Pred),
	throw(instantiation_error).

'$lgt_tr_pred_alias_directive'(_, _, Alias) :-
	var(Alias),
	throw(instantiation_error).

'$lgt_tr_pred_alias_directive'(Entity, _, _) :-
	\+ callable(Entity),
	throw(type_error(entity_identifier, Entity)).

'$lgt_tr_pred_alias_directive'(Entity, _, _) :-
	\+ '$lgt_pp_extended_protocol_'(Entity, _, _, _),
	\+ '$lgt_pp_implemented_protocol_'(Entity, _, _, _),
	\+ '$lgt_pp_extended_category_'(Entity, _, _, _, _),
	\+ '$lgt_pp_imported_category_'(Entity, _, _, _, _),
	\+ '$lgt_pp_extended_object_'(Entity, _, _, _, _, _, _, _, _, _),
	\+ '$lgt_pp_instantiated_class_'(Entity, _, _, _, _, _, _, _, _, _),
	\+ '$lgt_pp_specialized_class_'(Entity, _, _, _, _, _, _, _, _, _),
	\+ '$lgt_pp_complemented_object_'(Entity),
	throw(reference_error(entity_identifier, Entity)).

'$lgt_tr_pred_alias_directive'(_, Pred, _) :-
	\+ '$lgt_valid_predicate_indicator'(Pred, _, _),
	\+ '$lgt_valid_non_terminal_indicator'(Pred, _, _, _),
	throw(type_error(predicate_indicator, Pred)).

'$lgt_tr_pred_alias_directive'(_, _, Alias) :-
	\+ '$lgt_valid_predicate_indicator'(Alias, _, _),
	\+ '$lgt_valid_non_terminal_indicator'(Alias, _, _, _),
	throw(type_error(predicate_indicator, Alias)).

'$lgt_tr_pred_alias_directive'(_, Functor1//Arity1, Functor2//Arity2) :-
	Arity1 =\= Arity2,
	throw(domain_error(arity_mismatch, Functor1//Arity1, Functor2//Arity2)).

'$lgt_tr_pred_alias_directive'(_, Functor1/Arity1, Functor2/Arity2) :-
	Arity1 =\= Arity2,
	throw(domain_error(arity_mismatch, Functor1/Arity1, Functor2/Arity2)).

'$lgt_tr_pred_alias_directive'(_, Functor1/Arity1, Functor2//Arity2) :-
	throw(domain_error(indicator_mismatch, Functor1/Arity1, Functor2//Arity2)).

'$lgt_tr_pred_alias_directive'(_, Functor1//Arity1, Functor2/Arity2) :-
	throw(domain_error(indicator_mismatch, Functor1//Arity1, Functor2/Arity2)).

'$lgt_tr_pred_alias_directive'(Entity, Functor1/Arity, Functor2/Arity) :-
	!,
	functor(Pred, Functor1, Arity),
	Pred =.. [_| Args],
	Alias =.. [Functor2| Args],
	assertz('$lgt_pp_predicate_alias_'(Entity, Pred, Alias)).

'$lgt_tr_pred_alias_directive'(Entity, Functor1//Arity, Functor2//Arity) :-
	ExtArity is Arity + 2,
	functor(Pred, Functor1, ExtArity),
	Pred =.. [_| Args],
	Alias =.. [Functor2| Args],
	assertz('$lgt_pp_predicate_alias_'(Entity, Pred, Alias)).



'$lgt_tr_calls_directive'([]).

'$lgt_tr_calls_directive'([Ptc| Ptcs]) :-
	'$lgt_must_be'(protocol_identifier, Ptc),
	'$lgt_add_referenced_protocol'(Ptc),
	assertz('$lgt_pp_calls_'(Ptc)),
	'$lgt_tr_calls_directive'(Ptcs).



% '$lgt_tr_synchronized_directive'(+list)
%
% auxiliary predicate for translating synchronized/1 directives

'$lgt_tr_synchronized_directive'(Preds) :-
	'$lgt_gen_pred_mutex'(Mutex),
	'$lgt_tr_synchronized_directive'(Preds, Mutex).


'$lgt_gen_pred_mutex'(Mutex) :-
	'$lgt_pp_entity'(_, _, Prefix, _, _),
	retract('$lgt_pp_predicate_mutex_counter_'(Old)),
	New is Old + 1,
	asserta('$lgt_pp_predicate_mutex_counter_'(New)),
	number_codes(New, Codes),
	atom_codes(Atom, Codes),
	atom_concat(Prefix, 'pred_mutex_', Aux),
	atom_concat(Aux, Atom, Mutex).


'$lgt_tr_synchronized_directive'([], _).

'$lgt_tr_synchronized_directive'([Pred| _], _) :-
	var(Pred),
	throw(instantiation_error).

'$lgt_tr_synchronized_directive'([Pred| Preds], Mutex) :-
	'$lgt_valid_predicate_indicator'(Pred, Functor, Arity),
	!,
	(	'$lgt_pp_dynamic_'(Functor, Arity) ->
		throw(permission_error(modify, dynamic_predicate, Functor/Arity))
	;	'$lgt_pp_calls_predicate_'(Functor, Arity, _, _) ->
		throw(permission_error(modify, predicate_interpretation, Functor/Arity))
	;	functor(Head, Functor, Arity),
		assertz('$lgt_pp_synchronized_'(Head, Mutex)),
		'$lgt_tr_synchronized_directive'(Preds, Mutex)
	).

'$lgt_tr_synchronized_directive'([Pred| Preds], Mutex) :-
	'$lgt_valid_non_terminal_indicator'(Pred, Functor, Arity, ExtArity),
	!,
	(	'$lgt_pp_dynamic_'(Functor, ExtArity) ->
		throw(permission_error(modify, dynamic_non_terminal, Functor//Arity))
	;	'$lgt_pp_calls_non_terminal_'(Functor, Arity) ->
		throw(permission_error(modify, non_terminal_interpretation, Functor//Arity))
	;	functor(Head, Functor, ExtArity),
		assertz('$lgt_pp_synchronized_'(Head, Mutex)),
		'$lgt_tr_synchronized_directive'(Preds, Mutex)
	).

'$lgt_tr_synchronized_directive'([Pred| _], _) :-
	throw(type_error(predicate_indicator, Pred)).



% '$lgt_tr_public_directive'(+list)
%
% auxiliary predicate for translating public/1 directives

'$lgt_tr_public_directive'([]).

'$lgt_tr_public_directive'([Pred| _]) :-
	var(Pred),
	throw(instantiation_error).

'$lgt_tr_public_directive'([op(Priority, Spec, Operators)| Preds]) :-
	!,
	'$lgt_check_op_directive_args'(Priority, Spec, Operators),
	'$lgt_check_for_duplicated_scope_directives'(op(Priority, Spec, Operators)),
	'$lgt_assert_entity_operators'(Priority, Spec, Operators, (public)),
	'$lgt_tr_public_directive'(Preds).

'$lgt_tr_public_directive'([Pred| Preds]) :-
	'$lgt_valid_predicate_indicator'(Pred, Functor, Arity),
	!,
	'$lgt_check_for_duplicated_scope_directives'(Functor/Arity),
	assertz('$lgt_pp_public_'(Functor, Arity)),
	'$lgt_tr_public_directive'(Preds).

'$lgt_tr_public_directive'([Pred| Preds]) :-
	'$lgt_valid_non_terminal_indicator'(Pred, Functor, Arity, ExtArity),
	!,
	'$lgt_check_for_duplicated_scope_directives'(Functor//Arity+ExtArity),
	assertz('$lgt_pp_non_terminal_'(Functor, Arity, ExtArity)),
	assertz('$lgt_pp_public_'(Functor, ExtArity)),
	'$lgt_tr_public_directive'(Preds).

'$lgt_tr_public_directive'([Pred| _]) :-
	throw(type_error(predicate_indicator, Pred)).



% '$lgt_tr_protected_directive'(+list)
%
% auxiliary predicate for translating protected/1 directives

'$lgt_tr_protected_directive'([]).

'$lgt_tr_protected_directive'([Pred| _]) :-
	var(Pred),
	throw(instantiation_error).

'$lgt_tr_protected_directive'([op(Priority, Spec, Operators)| Preds]) :-
	!,
	'$lgt_check_op_directive_args'(Priority, Spec, Operators),
	'$lgt_check_for_duplicated_scope_directives'(op(Priority, Spec, Operators)),
	'$lgt_assert_entity_operators'(Priority, Spec, Operators, protected),
	'$lgt_tr_protected_directive'(Preds).

'$lgt_tr_protected_directive'([Pred| Preds]) :-
	'$lgt_valid_predicate_indicator'(Pred, Functor, Arity),
	!,
	'$lgt_check_for_duplicated_scope_directives'(Functor/Arity),
	assertz('$lgt_pp_protected_'(Functor, Arity)),
	'$lgt_tr_protected_directive'(Preds).

'$lgt_tr_protected_directive'([Pred| Preds]) :-
	'$lgt_valid_non_terminal_indicator'(Pred, Functor, Arity, ExtArity),
	!,
	'$lgt_check_for_duplicated_scope_directives'(Functor//Arity+ExtArity),
	assertz('$lgt_pp_non_terminal_'(Functor, Arity, ExtArity)),
	assertz('$lgt_pp_protected_'(Functor, ExtArity)),
	'$lgt_tr_protected_directive'(Preds).

'$lgt_tr_protected_directive'([Pred| _]) :-
	throw(type_error(predicate_indicator, Pred)).



% '$lgt_tr_private_directive'(+list)
%
% auxiliary predicate for translating private/1 directives

'$lgt_tr_private_directive'([]).

'$lgt_tr_private_directive'([Pred| _]) :-
	var(Pred),
	throw(instantiation_error).

'$lgt_tr_private_directive'([op(Priority, Spec, Operators)| Preds]) :-
	!,
	'$lgt_check_op_directive_args'(Priority, Spec, Operators),
	'$lgt_check_for_duplicated_scope_directives'(op(Priority, Spec, Operators)),
	'$lgt_assert_entity_operators'(Priority, Spec, Operators, private),
	'$lgt_tr_private_directive'(Preds).

'$lgt_tr_private_directive'([Pred| Preds]) :-
	'$lgt_valid_predicate_indicator'(Pred, Functor, Arity),
	!,
	'$lgt_check_for_duplicated_scope_directives'(Functor/Arity),
	assertz('$lgt_pp_private_'(Functor, Arity)),
	'$lgt_tr_private_directive'(Preds).

'$lgt_tr_private_directive'([Pred| Preds]) :-
	'$lgt_valid_non_terminal_indicator'(Pred, Functor, Arity, ExtArity),
	!,
	'$lgt_check_for_duplicated_scope_directives'(Functor//Arity+ExtArity),
	assertz('$lgt_pp_non_terminal_'(Functor, Arity, ExtArity)),
	assertz('$lgt_pp_private_'(Functor, ExtArity)),
	'$lgt_tr_private_directive'(Preds).

'$lgt_tr_private_directive'([Pred| _]) :-
	throw(type_error(predicate_indicator, Pred)).



'$lgt_check_for_duplicated_scope_directives'(op(_, _, [])) :-
	!.

'$lgt_check_for_duplicated_scope_directives'(op(Priority, Spec, [Operator| Operators])) :-
	!,
	(	'$lgt_pp_entity_op_'(Priority, Spec, Operator, _) ->
		throw(permission_error(modify, operator_scope, op(Priority, Spec, Operator)))
	;	'$lgt_check_for_duplicated_scope_directives'(op(Priority, Spec, Operators))
	).

'$lgt_check_for_duplicated_scope_directives'(op(Priority, Spec, Operator)) :-
	(	'$lgt_pp_entity_op_'(Priority, Spec, Operator, _) ->
		throw(permission_error(modify, predicate_scope, op(Priority, Spec, Operator)))
	;	true
	).

'$lgt_check_for_duplicated_scope_directives'(Functor/Arity) :-
	(	(	'$lgt_pp_public_'(Functor, Arity)
		;	'$lgt_pp_protected_'(Functor, Arity)
		;	'$lgt_pp_private_'(Functor, Arity)
		) ->
		throw(permission_error(modify, predicate_scope, Functor/Arity))
	;	true
	).

'$lgt_check_for_duplicated_scope_directives'(Functor//Arity+ExtArity) :-
	(	(	'$lgt_pp_public_'(Functor, ExtArity)
		;	'$lgt_pp_protected_'(Functor, ExtArity)
		;	'$lgt_pp_private_'(Functor, ExtArity)
		) ->
		throw(permission_error(modify, non_terminal_scope, Functor//Arity))
	;	true
	).



% '$lgt_tr_dynamic_directive'(+list)
%
% auxiliary predicate for translating dynamic/1 directives

'$lgt_tr_dynamic_directive'([]).

'$lgt_tr_dynamic_directive'([Pred| _]) :-
	var(Pred),
	throw(instantiation_error).

'$lgt_tr_dynamic_directive'([Entity::_| _]) :-
	var(Entity),
	throw(instantiation_error).

'$lgt_tr_dynamic_directive'([':'(Module, _)| _]) :-
	var(Module),
	throw(instantiation_error).

'$lgt_tr_dynamic_directive'([_::Pred| _]) :-
	var(Pred),
	throw(instantiation_error).

'$lgt_tr_dynamic_directive'([':'(_, Pred)| _]) :-
	var(Pred),
	throw(instantiation_error).

'$lgt_tr_dynamic_directive'([Entity::_| _]) :-
	\+ callable(Entity),
	throw(type_error(entity_identifier, Entity)).

'$lgt_tr_dynamic_directive'([':'(Module, _)| _]) :-
	\+ atom(Module),
	throw(type_error(atom, Module)).

'$lgt_tr_dynamic_directive'([Entity::Pred| Preds]) :-
	'$lgt_valid_predicate_indicator'(Pred, Functor, Arity),
	!,
	(	Entity == user ->
		assertz('$lgt_pp_directive_'(dynamic(Functor/Arity)))
	;	'$lgt_construct_entity_prefix'(Entity, _, Prefix),
		'$lgt_construct_predicate_indicator'(Prefix, Functor/Arity, TFunctor/TArity),
		assertz('$lgt_pp_directive_'(dynamic(TFunctor/TArity)))
	),
	'$lgt_tr_dynamic_directive'(Preds).

'$lgt_tr_dynamic_directive'([':'(Module, Pred)| Preds]) :-
	'$lgt_valid_predicate_indicator'(Pred, Functor, Arity),
	!,
	(	Module == user ->
		assertz('$lgt_pp_directive_'(dynamic(Functor/Arity)))
	;	assertz('$lgt_pp_directive_'(dynamic(':'(Module, Functor/Arity))))	
	),
	'$lgt_tr_dynamic_directive'(Preds).

'$lgt_tr_dynamic_directive'([Pred| Preds]) :-
	'$lgt_valid_predicate_indicator'(Pred, Functor, Arity),
	!,
	(	functor(Head, Functor, Arity),
		'$lgt_pp_synchronized_'(Head, _) ->
		throw(permission_error(modify, synchronized_predicate, Functor/Arity))
	;	assertz('$lgt_pp_dynamic_'(Functor, Arity)),
		'$lgt_tr_dynamic_directive'(Preds)
	).

'$lgt_tr_dynamic_directive'([Entity::Pred| Preds]) :-
	'$lgt_valid_non_terminal_indicator'(Pred, Functor, _, ExtArity),
	!,
	(	Entity == user ->
		assertz('$lgt_pp_directive_'(dynamic(Functor/ExtArity)))
	;	'$lgt_construct_entity_prefix'(Entity, _, Prefix),
		'$lgt_construct_predicate_indicator'(Prefix, Functor/ExtArity, TFunctor/TArity),
		assertz('$lgt_pp_directive_'(dynamic(TFunctor/TArity)))
	),
	'$lgt_tr_dynamic_directive'(Preds).

'$lgt_tr_dynamic_directive'([':'(Module, Pred)| Preds]) :-
	'$lgt_valid_non_terminal_indicator'(Pred, Functor, _, ExtArity),
	!,
	(	Module == user ->
		assertz('$lgt_pp_directive_'(dynamic(Functor/ExtArity)))
	;	assertz('$lgt_pp_directive_'(dynamic(':'(Module, Functor/ExtArity))))
	),
	'$lgt_tr_dynamic_directive'(Preds).

'$lgt_tr_dynamic_directive'([Pred| Preds]) :-
	'$lgt_valid_non_terminal_indicator'(Pred, Functor, Arity, ExtArity),
	!,
	(	functor(Head, Functor, ExtArity),
		'$lgt_pp_synchronized_'(Head, _) ->
		throw(permission_error(modify, synchronized_non_terminal, Functor//Arity))
	;	'$lgt_pp_calls_non_terminal_'(Functor, Arity) ->
		throw(permission_error(modify, predicate_interpretation, Functor//Arity))
	;	assertz('$lgt_pp_dynamic_'(Functor, ExtArity)),
		'$lgt_tr_dynamic_directive'(Preds)
	).

'$lgt_tr_dynamic_directive'([Pred| _]) :-
	throw(type_error(predicate_indicator, Pred)).



% '$lgt_tr_discontiguous_directive'(+list)
%
% auxiliary predicate for translating discontiguous/1 directives

'$lgt_tr_discontiguous_directive'([]).

'$lgt_tr_discontiguous_directive'([Pred| _]) :-
	var(Pred),
	throw(instantiation_error).

'$lgt_tr_discontiguous_directive'([Entity::_| _]) :-
	var(Entity),
	throw(instantiation_error).

'$lgt_tr_discontiguous_directive'([':'(Module, _)| _]) :-
	var(Module),
	throw(instantiation_error).

'$lgt_tr_discontiguous_directive'([_::Pred| _]) :-
	var(Pred),
	throw(instantiation_error).

'$lgt_tr_discontiguous_directive'([':'(_, Pred)| _]) :-
	var(Pred),
	throw(instantiation_error).

'$lgt_tr_discontiguous_directive'([Entity::_| _]) :-
	\+ callable(Entity),
	throw(type_error(entity_identifier, Entity)).

'$lgt_tr_discontiguous_directive'([':'(Module, _)| _]) :-
	\+ atom(Module),
	throw(type_error(atom, Module)).

'$lgt_tr_discontiguous_directive'([Entity::Pred| Preds]) :-
	'$lgt_valid_predicate_indicator'(Pred, Functor, Arity),
	!,
	(	Entity == user ->
		assertz('$lgt_pp_directive_'(discontiguous(Functor/Arity)))
	;	'$lgt_construct_entity_prefix'(Entity, _, Prefix),
		'$lgt_construct_predicate_indicator'(Prefix, Functor/Arity, TFunctor/TArity),
		assertz('$lgt_pp_directive_'(discontiguous(TFunctor/TArity)))
	),
	'$lgt_tr_discontiguous_directive'(Preds).

'$lgt_tr_discontiguous_directive'([':'(Module, Pred)| Preds]) :-
	'$lgt_valid_predicate_indicator'(Pred, Functor, Arity),
	!,
	(	Module == user ->
		assertz('$lgt_pp_directive_'(discontiguous(Functor/Arity)))
	;	assertz('$lgt_pp_directive_'(discontiguous(':'(Module, Functor/Arity))))	
	),
	'$lgt_tr_discontiguous_directive'(Preds).

'$lgt_tr_discontiguous_directive'([Pred| Preds]) :-
	'$lgt_valid_predicate_indicator'(Pred, Functor, Arity),
	!,
	assertz('$lgt_pp_discontiguous_'(Functor, Arity)),
	'$lgt_tr_discontiguous_directive'(Preds).

'$lgt_tr_discontiguous_directive'([Entity::Pred| Preds]) :-
	'$lgt_valid_non_terminal_indicator'(Pred, Functor, _, ExtArity),
	!,
	(	Entity == user ->
		assertz('$lgt_pp_directive_'(discontiguous(Functor/ExtArity)))
	;	'$lgt_construct_entity_prefix'(Entity, _, Prefix),
		'$lgt_construct_predicate_indicator'(Prefix, Functor/ExtArity, TFunctor/TArity),
		assertz('$lgt_pp_directive_'(discontiguous(TFunctor/TArity)))
	),
	'$lgt_tr_discontiguous_directive'(Preds).

'$lgt_tr_discontiguous_directive'([':'(Module, Pred)| Preds]) :-
	'$lgt_valid_non_terminal_indicator'(Pred, Functor, _, ExtArity),
	!,
	(	Module == user ->
		assertz('$lgt_pp_directive_'(discontiguous(Functor/ExtArity)))
	;	assertz('$lgt_pp_directive_'(discontiguous(':'(Module, Functor/ExtArity))))
	),
	'$lgt_tr_discontiguous_directive'(Preds).

'$lgt_tr_discontiguous_directive'([Pred| Preds]) :-
	'$lgt_valid_non_terminal_indicator'(Pred, Functor, _, ExtArity),
	!,
	assertz('$lgt_pp_discontiguous_'(Functor, ExtArity)),
	'$lgt_tr_discontiguous_directive'(Preds).

'$lgt_tr_discontiguous_directive'([Pred| _]) :-
	throw(type_error(predicate_indicator, Pred)).



% '$lgt_tr_meta_predicate_directive'(+list)
%
% auxiliary predicate for translating meta_predicate/1 directives

'$lgt_tr_meta_predicate_directive'([]).

'$lgt_tr_meta_predicate_directive'([Pred| _]) :-
	var(Pred),
	throw(instantiation_error).

'$lgt_tr_meta_predicate_directive'([Pred| _]) :-
	\+ callable(Pred),
	throw(type_error(callable, Pred)).

'$lgt_tr_meta_predicate_directive'([Entity::_| _]) :-
	var(Entity),
	throw(instantiation_error).

'$lgt_tr_meta_predicate_directive'([':'(Module, _)| _]) :-
	var(Module),
	throw(instantiation_error).

'$lgt_tr_meta_predicate_directive'([_::Pred| _]) :-
	var(Pred),
	throw(instantiation_error).

'$lgt_tr_meta_predicate_directive'([':'(_, Pred)| _]) :-
	var(Pred),
	throw(instantiation_error).

'$lgt_tr_meta_predicate_directive'([Entity::_| _]) :-
	\+ callable(Entity),
	throw(type_error(entity_identifier, Entity)).

'$lgt_tr_meta_predicate_directive'([':'(Module, _)| _]) :-
	\+ atom(Module),
	throw(type_error(atom, Module)).

'$lgt_tr_meta_predicate_directive'([Entity::Pred| Preds]) :-
	'$lgt_valid_meta_predicate_template'(Pred),
	!,
	assertz('$lgt_pp_meta_predicate_'(Entity::Pred)),
	'$lgt_tr_meta_predicate_directive'(Preds).

'$lgt_tr_meta_predicate_directive'([':'(Module, Pred)| Preds]) :-
	'$lgt_valid_meta_predicate_template'(Pred),
	!,
	assertz('$lgt_pp_meta_predicate_'(':'(Module, Pred))),
	'$lgt_tr_meta_predicate_directive'(Preds).

'$lgt_tr_meta_predicate_directive'([Pred| Preds]) :-
	'$lgt_valid_meta_predicate_template'(Pred),
	!,
	functor(Pred, Functor, Arity),
	'$lgt_check_for_directive_after_call'(Functor/Arity),
	assertz('$lgt_pp_meta_predicate_'(Pred)),
	'$lgt_tr_meta_predicate_directive'(Preds).

'$lgt_tr_meta_predicate_directive'([Pred| _]) :-
	throw(type_error(meta_predicate_template, Pred)).



'$lgt_check_for_directive_after_call'(Functor/Arity) :-
	(	'$lgt_pp_calls_predicate_'(Functor, Arity, _, _) ->
		throw(permission_error(modify, predicate_interpretation, Functor/Arity))
	;	true
	).



% '$lgt_tr_annotation_directive'(+list)
%
% auxiliary predicate for translating annotation/1 directives

'$lgt_tr_annotation_directive'([]).

'$lgt_tr_annotation_directive'([Annotation| _]) :-
	var(Annotation),
	throw(instantiation_error).

'$lgt_tr_annotation_directive'([Annotation| _]) :-
	\+ callable(Annotation),
	throw(type_error(callable, Annotation)).

'$lgt_tr_annotation_directive'([Annotation| _]) :-
	\+ '$lgt_valid_annotation_template'(Annotation),
	throw(domain_error(annotation_template, Annotation)).

'$lgt_tr_annotation_directive'([Annotation| Annotations]) :-
	functor(Annotation, Functor, Arity),
	functor(GAnnotation, Functor, Arity),
	(	arg(1, Annotation, (*)), arg(2, Annotation, 0) ->
		arg(1, GAnnotation, Value),
		arg(2, GAnnotation, Goal),
		(	'$lgt_pp_value_annotation_'(GAnnotation, Functor, Value, Goal) ->
			true
		;	assertz('$lgt_pp_value_annotation_'(GAnnotation, Functor, Value, Goal))
		)
	;	arg(1, Annotation, 0), arg(2, Annotation, (*)) ->
		arg(1, GAnnotation, Goal),
		arg(2, GAnnotation, Value),
		(	'$lgt_pp_value_annotation_'(GAnnotation, Functor, Value, Goal) ->
			true
		;	assertz('$lgt_pp_value_annotation_'(GAnnotation, Functor, Value, Goal))
		)
	;	arg(1, Annotation, 0), arg(2, Annotation, 0),
		arg(1, GAnnotation, Left),
		arg(2, GAnnotation, Right),
		(	'$lgt_pp_goal_annotation_'(GAnnotation, Functor, Left, Right) ->
			true
		;	assertz('$lgt_pp_goal_annotation_'(GAnnotation, Functor, Left, Right))
		)
	),
	'$lgt_tr_annotation_directive'(Annotations).



% '$lgt_tr_multifile_directive'(+list)
%
% auxiliary predicate for translating multifile/1 directives

'$lgt_tr_multifile_directive'([]).

'$lgt_tr_multifile_directive'([Pred| _]) :-
	var(Pred),
	throw(instantiation_error).

'$lgt_tr_multifile_directive'([Entity::_| _]) :-
	var(Entity),
	throw(instantiation_error).

'$lgt_tr_multifile_directive'([':'(Module, _)| _]) :-
	var(Module),
	throw(instantiation_error).

'$lgt_tr_multifile_directive'([_::Pred| _]) :-
	var(Pred),
	throw(instantiation_error).

'$lgt_tr_multifile_directive'([':'(_, Pred)| _]) :-
	var(Pred),
	throw(instantiation_error).

'$lgt_tr_multifile_directive'([Entity::_| _]) :-
	\+ callable(Entity),
	throw(type_error(entity_identifier, Entity)).

'$lgt_tr_multifile_directive'([':'(Module, _)| _]) :-
	\+ atom(Module),
	throw(type_error(atom, Module)).

'$lgt_tr_multifile_directive'([Entity::Pred| Preds]) :-
	'$lgt_valid_predicate_indicator'(Pred, Functor, Arity),
	!,
	(	Entity == user ->
		assertz('$lgt_pp_directive_'(multifile(Functor/Arity)))
	;	functor(Template, Functor, Arity),
		'$lgt_predicate_property'(Entity, Template, (public), Entity, p(p(p))),
		'$lgt_predicate_property'(Entity, Template, (multifile), Entity, p(p(p))) ->
		'$lgt_construct_entity_prefix'(Entity, _, Prefix),
		'$lgt_construct_predicate_indicator'(Prefix, Functor/Arity, TFunctor/TArity),
		assertz('$lgt_pp_directive_'(multifile(TFunctor/TArity)))
	;	throw(permission_error(modify, predicate_declaration, Pred))
	),
	'$lgt_tr_multifile_directive'(Preds).

'$lgt_tr_multifile_directive'([Entity::Pred| Preds]) :-
	'$lgt_valid_non_terminal_indicator'(Pred, Functor, _, ExtArity),
	!,
	(	Entity == user ->
		assertz('$lgt_pp_directive_'(multifile(Functor/ExtArity)))
	;	functor(Template, Functor, ExtArity),
		'$lgt_predicate_property'(Entity, Template, (public), Entity, p(p(p))),
		'$lgt_predicate_property'(Entity, Template, (multifile), Entity, p(p(p))) ->
		'$lgt_construct_entity_prefix'(Entity, _, Prefix),
		'$lgt_construct_predicate_indicator'(Prefix, Functor/ExtArity, TFunctor/TArity),
		assertz('$lgt_pp_directive_'(multifile(TFunctor/TArity)))
	;	throw(permission_error(modify, non_terminal_declaration, Pred))
	),
	'$lgt_tr_multifile_directive'(Preds).

'$lgt_tr_multifile_directive'([':'(Module, Pred)| Preds]) :-
	'$lgt_valid_predicate_indicator'(Pred, Functor, Arity),
	!,
	(	Module == user ->
		assertz('$lgt_pp_directive_'(multifile(Functor/Arity)))
	;	assertz('$lgt_pp_directive_'(multifile(':'(Module, Functor/Arity))))
	),
	'$lgt_tr_multifile_directive'(Preds).

'$lgt_tr_multifile_directive'([':'(Module, Pred)| Preds]) :-
	'$lgt_valid_non_terminal_indicator'(Pred, Functor, _, ExtArity),
	!,
	(	Module == user ->
		assertz('$lgt_pp_directive_'(multifile(Functor/ExtArity)))
	;	assertz('$lgt_pp_directive_'(multifile(':'(Module, Functor/ExtArity))))
	),
	'$lgt_tr_multifile_directive'(Preds).

'$lgt_tr_multifile_directive'([Pred| Preds]) :-
	'$lgt_valid_predicate_indicator'(Pred, Functor, Arity),
	!,
	assertz('$lgt_pp_multifile_'(Functor, Arity)),
	'$lgt_pp_entity'(_, _, Prefix, _, _),
	'$lgt_construct_predicate_indicator'(Prefix, Functor/Arity, TFunctor/TArity),
	assertz('$lgt_pp_directive_'(multifile(TFunctor/TArity))),
	'$lgt_tr_multifile_directive'(Preds).

'$lgt_tr_multifile_directive'([Pred| Preds]) :-
	'$lgt_valid_non_terminal_indicator'(Pred, Functor, _, ExtArity),
	!,
	assertz('$lgt_pp_multifile_'(Functor, ExtArity)),
	'$lgt_pp_entity'(_, _, Prefix, _, _),
	'$lgt_construct_predicate_indicator'(Prefix, Functor/ExtArity, TFunctor/TArity),
	assertz('$lgt_pp_directive_'(multifile(TFunctor/TArity))),
	'$lgt_tr_multifile_directive'(Preds).

'$lgt_tr_multifile_directive'([Pred| _]) :-
	throw(type_error(predicate_indicator, Pred)).



% '$lgt_tr_coinductive_directive'(+list, +ctx)
%
% auxiliary predicate for translating coinductive/1 directives

'$lgt_tr_coinductive_directive'([], _).

'$lgt_tr_coinductive_directive'([Pred| _], _) :-
	var(Pred),
	throw(instantiation_error).

'$lgt_tr_coinductive_directive'([Pred| Preds], Ctx) :-
	'$lgt_valid_predicate_indicator'(Pred, Functor, Arity),
	!,
	functor(Head, Functor, Arity),
	atom_concat('_coinductive_', Functor, CoinductiveFunctor),
	functor(CoinductiveHead, CoinductiveFunctor, Arity),
	'$lgt_unify_head_thead_args'(Arity, Head, CoinductiveHead),
	'$lgt_comp_ctx_mode'(Ctx, Mode),
	'$lgt_comp_ctx_mode'(HeadCtx, Mode),
	'$lgt_comp_ctx_mode'(BodyCtx, Mode),
	'$lgt_comp_ctx_exec_ctx'(HeadCtx, HeadExCtx),
	'$lgt_exec_ctx'(HeadExCtx, Sender, This, Self, MetaCallCtx, _),
	'$lgt_comp_ctx_exec_ctx'(BodyCtx, BodyExCtx),
	% when calling a coinductive predicate, we start with an empty stack of ancestor goals:
	'$lgt_exec_ctx'(BodyExCtx, Sender, This, Self, MetaCallCtx, []),
	% add a linking clause from the original predicate to the predicate generated to implement coinduction:
	'$lgt_tr_clause'((Head :- CoinductiveHead), HeadCtx, BodyCtx),
	assertz('$lgt_pp_coinductive_'(Head, CoinductiveHead)),
	assertz('$lgt_pp_coinductive_flag_'(Head)),
	'$lgt_tr_coinductive_directive'(Preds, Ctx).

'$lgt_tr_coinductive_directive'([Pred| _], _) :-
	throw(type_error(predicate_indicator, Pred)).



% '$lgt_tr_uses_directive'(+list, +object_identifier, +ctx)
%
% auxiliary predicate for translating uses/2 directives

'$lgt_tr_uses_directive'([], _, _).

'$lgt_tr_uses_directive'([Pred| _], _, _) :-
	var(Pred),
	throw(instantiation_error).

'$lgt_tr_uses_directive'([Original::Alias| _], _, _) :-
	(var(Original); var(Alias)),
	throw(instantiation_error).

'$lgt_tr_uses_directive'([Original::Alias| Preds], Obj, Ctx) :-
	'$lgt_valid_predicate_indicator'(Original, OFunctor, OArity),
	'$lgt_valid_predicate_indicator'(Alias, AFunctor, AArity),
	!,
	(	OArity =:= AArity ->
		'$lgt_tr_uses_directive_predicate_arg'(OFunctor, AFunctor, OArity, Obj, Ctx)
	;	throw(domain_error(arity_mismatch(Original, Alias)))
	),
	'$lgt_tr_uses_directive'(Preds, Obj, Ctx).

'$lgt_tr_uses_directive'([Original::Alias| Preds], Obj, Ctx) :-
	'$lgt_valid_non_terminal_indicator'(Original, OFunctor, OArity, OExtArity),
	'$lgt_valid_non_terminal_indicator'(Alias, AFunctor, AArity, _),
	!,
	(	OArity =:= AArity ->
		'$lgt_tr_uses_directive_non_terminal_arg'(OFunctor, AFunctor, OArity, OExtArity, Obj, Ctx)
	;	throw(domain_error(arity_mismatch(Original, Alias)))
	),
	'$lgt_tr_uses_directive'(Preds, Obj, Ctx).

'$lgt_tr_uses_directive'([Pred| Preds], Obj, Ctx) :-
	'$lgt_valid_predicate_indicator'(Pred, Functor, Arity),
	!,
	'$lgt_tr_uses_directive_predicate_arg'(Functor, Functor, Arity, Obj, Ctx),
	'$lgt_tr_uses_directive'(Preds, Obj, Ctx).

'$lgt_tr_uses_directive'([NonTerminal| Preds], Obj, Ctx) :-
	'$lgt_valid_non_terminal_indicator'(NonTerminal, Functor, Arity, ExtArity),
	!,
	'$lgt_tr_uses_directive_non_terminal_arg'(Functor, Functor, Arity, ExtArity, Obj, Ctx),
	'$lgt_tr_uses_directive'(Preds, Obj, Ctx).

'$lgt_tr_uses_directive'([Original::_| _], _, _) :-
	\+ '$lgt_valid_predicate_indicator'(Original, _, _),
	\+ '$lgt_valid_non_terminal_indicator'(Original, _, _, _),
	throw(type_error(predicate_indicator, Original)).

'$lgt_tr_uses_directive'([_::Alias| _], _, _) :-
	\+ '$lgt_valid_predicate_indicator'(Alias, _, _),
	\+ '$lgt_valid_non_terminal_indicator'(Alias, _, _, _),
	throw(type_error(predicate_indicator, Alias)).

'$lgt_tr_uses_directive'([Pred| _], _, _) :-
	throw(type_error(predicate_indicator, Pred)).


'$lgt_tr_uses_directive_predicate_arg'(OFunctor, AFunctor, Arity, Obj, Ctx) :-
	functor(TOriginal, OFunctor, Arity),
	functor(TAlias, AFunctor, Arity),
	Arity2 is Arity - 2,
	(	Arity2 >= 0 ->
		functor(TNonTerminal, AFunctor, Arity2),
		\+ '$lgt_pp_uses_non_terminal_'(_, _, TNonTerminal),
		\+ '$lgt_pp_use_module_non_terminal_'(_, _, TNonTerminal)
	;	true
	),
	\+ '$lgt_pp_uses_predicate_'(_, _, TAlias),
 	\+ '$lgt_pp_use_module_predicate_'(_, _, TAlias),
	!,
	TOriginal =.. [_| Args], TAlias =.. [_| Args],									% unify args of TOriginal and TAlias
	'$lgt_tr_clause'((TAlias :- Obj::TOriginal), Ctx),								% allow for runtime use
	assertz('$lgt_pp_uses_predicate_'(Obj, TOriginal, TAlias)).

'$lgt_tr_uses_directive_predicate_arg'(_, AFunctor, Arity, _, _) :-
	throw(permission_error(modify, uses_object_predicate, AFunctor/Arity)).


'$lgt_tr_uses_directive_non_terminal_arg'(OFunctor, AFunctor, Arity, ExtArity, Obj, Ctx) :-
	functor(TOriginal, OFunctor, Arity),
	functor(TAlias, AFunctor, Arity),
	functor(TPred, AFunctor, ExtArity),
	(	\+ '$lgt_pp_uses_non_terminal_'(_, _, TOriginal),
	 	\+ '$lgt_pp_use_module_non_terminal_'(_, _, TOriginal),
	 	\+ '$lgt_pp_uses_predicate_'(_, _, TPred),
	 	\+ '$lgt_pp_use_module_predicate_'(_, _, TPred) ->
		TOriginal =.. [_| Args], TAlias =.. [_| Args],						% unify args of TOriginal and TAlias
		'$lgt_dcg_rule_to_clause'((TAlias --> Obj::TOriginal), Clause),		% allow for runtime use
		'$lgt_tr_clause'(Clause, Ctx),
		assertz('$lgt_pp_uses_non_terminal_'(Obj, TOriginal, TAlias))
	;	throw(permission_error(modify, uses_object_non_terminal, AFunctor//Arity))
	).



% '$lgt_tr_use_module_directive'(+list, +atom, +ctx)
%
% auxiliary predicate for translating use_module/2 directives in objects or categories
% the predicate renaming operator as/2 found on SWI-Prolog and YAP is also supported

'$lgt_tr_use_module_directive'([], _, _).

'$lgt_tr_use_module_directive'([Pred| _], _, _) :-
	var(Pred),
	throw(instantiation_error).

'$lgt_tr_use_module_directive'([':'(Original, Alias)| _], _, _) :-
	(var(Original); var(Alias)),
	throw(instantiation_error).

'$lgt_tr_use_module_directive'([':'(Original, Alias)| Preds], Module, Ctx) :-
	'$lgt_valid_predicate_indicator'(Original, OFunctor, OArity),
	'$lgt_valid_predicate_indicator'(Alias, AFunctor, AArity),
	!,
	(	OArity =:= AArity ->
		'$lgt_tr_use_module_directive_predicate_arg'(OFunctor, AFunctor, OArity, Module, Ctx)
	;	throw(domain_error(arity_mismatch(Original, Alias)))
	),
	'$lgt_tr_use_module_directive'(Preds, Module, Ctx).

'$lgt_tr_use_module_directive'([':'(Original, Alias)| Preds], Module, Ctx) :-
	'$lgt_valid_non_terminal_indicator'(Original, OFunctor, OArity, OExtArity),
	'$lgt_valid_non_terminal_indicator'(Alias, AFunctor, AArity, _),
	!,
	(	OArity =:= AArity ->
		'$lgt_tr_use_module_directive_non_terminal_arg'(OFunctor, AFunctor, OArity, OExtArity, Module, Ctx)
	;	throw(domain_error(arity_mismatch(Original, Alias)))
	),
	'$lgt_tr_use_module_directive'(Preds, Module, Ctx).

% only accept the as/2 renaming operator (found e.g. on SWI-Prolog, XSB and YAP) when compiling
% modules as objects:

'$lgt_tr_use_module_directive'([as(Original, AFunctor)| Preds], Module, Ctx) :-
	'$lgt_pp_module_'(_),
	'$lgt_valid_predicate_indicator'(Original, OFunctor, OArity),
	atom(AFunctor),
	!,
	'$lgt_tr_use_module_directive_predicate_arg'(OFunctor, AFunctor, OArity, Module, Ctx),
	'$lgt_tr_use_module_directive'(Preds, Module, Ctx).

'$lgt_tr_use_module_directive'([as(Original, AFunctor)| Preds], Module, Ctx) :-
	'$lgt_pp_module_'(_),
	'$lgt_valid_non_terminal_indicator'(Original, OFunctor, OArity, OExtArity),
	atom(AFunctor),
	!,
	'$lgt_tr_use_module_directive_non_terminal_arg'(OFunctor, AFunctor, OArity, OExtArity, Module, Ctx),
	'$lgt_tr_use_module_directive'(Preds, Module, Ctx).

'$lgt_tr_use_module_directive'([Pred| Preds], Module, Ctx) :-
	'$lgt_valid_predicate_indicator'(Pred, Functor, Arity),
	!,
	'$lgt_tr_use_module_directive_predicate_arg'(Functor, Functor, Arity, Module, Ctx),
	'$lgt_tr_use_module_directive'(Preds, Module, Ctx).

'$lgt_tr_use_module_directive'([NonTerminal| Preds], Module, Ctx) :-
	'$lgt_valid_non_terminal_indicator'(NonTerminal, Functor, Arity, ExtArity),
	!,
	'$lgt_tr_use_module_directive_non_terminal_arg'(Functor, Functor, Arity, ExtArity, Module, Ctx),
	'$lgt_tr_use_module_directive'(Preds, Module, Ctx).

'$lgt_tr_use_module_directive'([':'(Original, _)| _], _, _) :-
	\+ '$lgt_valid_predicate_indicator'(Original, _, _),
	\+ '$lgt_valid_non_terminal_indicator'(Original, _, _, _),
	throw(type_error(predicate_indicator, Original)).

'$lgt_tr_use_module_directive'([':'(_, Alias)| _], _, _) :-
	\+ '$lgt_valid_predicate_indicator'(Alias, _, _),
	\+ '$lgt_valid_non_terminal_indicator'(Alias, _, _, _),
	throw(type_error(predicate_indicator, Alias)).

'$lgt_tr_use_module_directive'([Pred| _], _, _) :-
	throw(type_error(predicate_indicator, Pred)).


'$lgt_tr_use_module_directive_predicate_arg'(OFunctor, AFunctor, Arity, Module, Ctx) :-
	functor(TOriginal, OFunctor, Arity),
	functor(TAlias, AFunctor, Arity),
	Arity2 is Arity - 2,
	(	Arity2 >= 0 ->
		functor(TNonTerminal, AFunctor, Arity2),
		\+ '$lgt_pp_uses_non_terminal_'(_, _, TNonTerminal),
		\+ '$lgt_pp_use_module_non_terminal_'(_, _, TNonTerminal)
	;	true
	),
	\+ '$lgt_pp_uses_predicate_'(_, _, TAlias),
	\+ '$lgt_pp_use_module_predicate_'(_, _, TAlias),
	!,
	TOriginal =.. [_| Args], TAlias =.. [_| Args],									% unify args of TOriginal and TAlias
	'$lgt_tr_clause'((TAlias :- ':'(Module, TOriginal)), Ctx),						% allow for runtime use
	assertz('$lgt_pp_use_module_predicate_'(Module, TOriginal, TAlias)).

'$lgt_tr_use_module_directive_predicate_arg'(_, AFunctor, Arity, _, _) :-
	throw(permission_error(modify, uses_module_predicate, AFunctor/Arity)).


'$lgt_tr_use_module_directive_non_terminal_arg'(OFunctor, AFunctor, Arity, ExtArity, Module, Ctx) :-
	functor(TOriginal, OFunctor, Arity),
	functor(TAlias, AFunctor, Arity),
	functor(TPred, AFunctor, ExtArity),
	(	\+ '$lgt_pp_uses_non_terminal_'(_, _, TOriginal),
	 	\+ '$lgt_pp_use_module_non_terminal_'(_, _, TOriginal),
	 	\+ '$lgt_pp_uses_predicate_'(_, _, TPred),
	 	\+ '$lgt_pp_use_module_predicate_'(_, _, TPred) ->
		TOriginal =.. [_| Args], TAlias =.. [_| Args],								% unify args of TOriginal and TAlias
		'$lgt_dcg_rule_to_clause'((TAlias --> ':'(Module, TOriginal)), Clause),		% allow for runtime use
		'$lgt_tr_clause'(Clause, Ctx),
		assertz('$lgt_pp_use_module_non_terminal_'(Module, TOriginal, TAlias))
	;	throw(permission_error(modify, uses_module_non_terminal, AFunctor//Arity))
	).



% '$lgt_tr_reexport_directive'(+list, +atom, +ctx)
%
% auxiliary predicate for translating module reexport/2 directives;
% the predicate renaming operator as/2 found on SWI-Prolog and YAP
% is also supported (iff we're compiling a module as an object)

'$lgt_tr_reexport_directive'([], _, _).

'$lgt_tr_reexport_directive'([Pred| _], _, _) :-
	var(Pred),
	throw(instantiation_error).

'$lgt_tr_reexport_directive'([as(Pred, NewFunctor)| Preds], Module, Ctx) :-
	'$lgt_valid_predicate_indicator'(Pred, Functor, Arity),
	atom(NewFunctor),
	!,
	'$lgt_tr_directive'((public), [NewFunctor/Arity], Ctx),
 	'$lgt_tr_directive'(uses, [Module, [Pred]], Ctx),
	functor(NewHead, NewFunctor, Arity),
	functor(Head, Functor, Arity),
	'$lgt_tr_clause'((NewHead :- Module::Head), Ctx),
	'$lgt_tr_reexport_directive'(Preds, Module, Ctx).

'$lgt_tr_reexport_directive'([Pred| Preds], Module, Ctx) :-
	'$lgt_valid_predicate_indicator'(Pred, Functor, Arity),
	!,
	'$lgt_tr_directive'((public), [Pred], Ctx),
	functor(Head, Functor, Arity),
	'$lgt_tr_clause'((Head :- Module::Head), Ctx),
	'$lgt_tr_reexport_directive'(Preds, Module, Ctx).

'$lgt_tr_reexport_directive'([as(NonTerminal, NewFunctor)| Preds], Module, Ctx) :-
	'$lgt_valid_non_terminal_indicator'(NonTerminal, Functor, Arity, _),
	atom(NewFunctor),
	!,
	'$lgt_tr_directive'((public), [NewFunctor//Arity], Ctx),
 	'$lgt_tr_directive'(uses, [Module, [NonTerminal]], Ctx),
	functor(NewHead, NewFunctor, Arity),
	functor(Head, Functor, Arity),
	'$lgt_tr_grammar_rule'((NewHead --> Module::Head), Ctx),
	'$lgt_tr_reexport_directive'(Preds, Module, Ctx).

'$lgt_tr_reexport_directive'([NonTerminal| Preds], Module, Ctx) :-
	'$lgt_valid_non_terminal_indicator'(NonTerminal, Functor, Arity, _),
	!,
	'$lgt_tr_directive'((public), [NonTerminal], Ctx),
	functor(Head, Functor, Arity),
	'$lgt_tr_grammar_rule'((Head --> Module::Head), Ctx),
	'$lgt_tr_reexport_directive'(Preds, Module, Ctx).

'$lgt_tr_reexport_directive'([Pred| _], _, _) :-
	throw(type_error(predicate_indicator, Pred)).



% auxiliary predicate for translating module's meta predicate directives into 
% Logtalk ones by translating the argument modes (e.g. (:) -> (::))

'$lgt_tr_module_meta_predicate_directives'([], []).

'$lgt_tr_module_meta_predicate_directives'([Template| Templates], [ConvertedTemplate| ConvertedTemplates]) :-
	Template =.. [Functor| Args],
	'$lgt_tr_module_meta_predicate_directives_args'(Args, ConvertedArgs),
	ConvertedTemplate =.. [Functor| ConvertedArgs],
	'$lgt_tr_module_meta_predicate_directives'(Templates, ConvertedTemplates).


'$lgt_tr_module_meta_predicate_directives_args'([], []).

'$lgt_tr_module_meta_predicate_directives_args'([Arg| Args], [TArg| TArgs]) :-
	(	Arg == (:) -> TArg = (::)				% Prolog to Logtalk notation; this is fragile due to the lack of standardization
	;	Arg == (::) -> TArg = (::)				% mixed-up notation or overriding meta-predicate template being used
	;	integer(Arg) -> TArg = Arg				% goals and closures are denoted by integers >= 0
	;	Arg == (/) -> TArg = Arg				% predicate indicator
	;	Arg = [N], integer(N) -> TArg = Arg		% list of goals/closures
	;	Arg == [/] -> TArg = Arg				% list of predicate indicators
	;	TArg = (*)								% non meta-arguments (e.g. instantiation modes) to Logtalk notation
	),
	'$lgt_tr_module_meta_predicate_directives_args'(Args, TArgs).



% '$lgt_tr_object_relations'(+list, +term)
%
% translates the relations of an object with other entities

'$lgt_tr_object_relations'([], _).

'$lgt_tr_object_relations'([Relation| Relations], Obj) :-
	(	var(Relation) ->
		throw(instantiation_error)
	;	Relation =.. [Functor| Args],
		'$lgt_flatten_list'(Args, FlattenedArgs),
		'$lgt_tr_object_relation'(Functor, FlattenedArgs, Obj) ->
		true
	;	functor(Relation, Functor, Arity),
		throw(domain_error(object_relation, Functor/Arity))
	),
	'$lgt_tr_object_relations'(Relations, Obj).



% '$lgt_tr_object_relation'(+atom, +list, +term)
%
% translates a relation between an object (the last argument) with other entities

'$lgt_tr_object_relation'(implements, Ptcs, Obj) :-
	'$lgt_tr_implements_protocol'(Ptcs, Obj).

'$lgt_tr_object_relation'(imports, Ctgs, Obj) :-
	'$lgt_tr_imports_category'(Ctgs, Obj).

'$lgt_tr_object_relation'(instantiates, Classes, Instance) :-
	'$lgt_tr_instantiates_class'(Classes, Instance).

'$lgt_tr_object_relation'(specializes, Superclasses, Class) :-
	'$lgt_tr_specializes_class'(Superclasses, Class).

'$lgt_tr_object_relation'(extends, Parents, Prototype) :-
	'$lgt_tr_extends_object'(Parents, Prototype).



% '$lgt_tr_protocol_relations'(+list, +term)
%
% translates the relations of a protocol with other entities

'$lgt_tr_protocol_relations'([], _).

'$lgt_tr_protocol_relations'([Relation| Relations], Ptc) :-
	(	var(Relation) ->
		throw(instantiation_error)
	;	Relation =.. [Functor| Args],
		'$lgt_flatten_list'(Args, FlattenedArgs),
		'$lgt_tr_protocol_relation'(Functor, FlattenedArgs, Ptc) ->
		true
	;	functor(Relation, Functor, Arity),
		throw(domain_error(protocol_relation, Functor/Arity))
	),
	'$lgt_tr_protocol_relations'(Relations, Ptc).



% '$lgt_tr_protocol_relation'(+atom, +list, +term)
%
% translates a relation between a protocol (the last argument) with other entities

'$lgt_tr_protocol_relation'(extends, Ptcs, Ptc) :-
	'$lgt_tr_extends_protocol'(Ptcs, Ptc).



% '$lgt_tr_category_relations'(+list, +category_identifier)
%
% translates the relations of a category with other entities

'$lgt_tr_category_relations'([], _).

'$lgt_tr_category_relations'([Relation| Relations], Ctg) :-
	(	var(Relation) ->
		throw(instantiation_error)
	;	Relation =.. [Functor| Args],
		'$lgt_flatten_list'(Args, FlattenedArgs),
		'$lgt_tr_category_relation'(Functor, FlattenedArgs, Ctg) ->
		true
	;	functor(Relation, Functor, Arity),
		throw(domain_error(category_relation, Functor/Arity))
	),
	'$lgt_tr_category_relations'(Relations, Ctg).



% '$lgt_tr_category_relation'(+atom, +list, +category_identifier)
%
% translates a relation between a category (the last argument) with other entities

'$lgt_tr_category_relation'(implements, Ptcs, Ctg) :-
	'$lgt_tr_implements_protocol'(Ptcs, Ctg).

'$lgt_tr_category_relation'(extends, Ctgs, Ctg) :-
	'$lgt_tr_extends_category'(Ctgs, Ctg).

'$lgt_tr_category_relation'(complements, Objs, Ctg) :-
	'$lgt_tr_complements_category'(Objs, Ctg).



% '$lgt_valid_entity_info_list'(@list)
%
% true if the argument is a list of valid key-value pairs

'$lgt_valid_entity_info_list'(List) :-
	var(List),
	throw(instantiation_error). 

'$lgt_valid_entity_info_list'(List) :-
	\+ '$lgt_is_proper_list'(List),
	throw(type_error(list, List)).

'$lgt_valid_entity_info_list'([]).

'$lgt_valid_entity_info_list'([Head| _]) :-
	var(Head),
	throw(instantiation_error). 

'$lgt_valid_entity_info_list'([Head| _]) :-
	Head \= (_ is _),
	throw(type_error(key_value_info_pair, Head)).

'$lgt_valid_entity_info_list'([Key is Value| _]) :-
	(var(Key); var(Value)),
	throw(instantiation_error). 

'$lgt_valid_entity_info_list'([Key is _| _]) :-
	\+ atom(Key),
	throw(type_error(atom, Key)). 

'$lgt_valid_entity_info_list'([Key is Value| Tail]) :-
	'$lgt_valid_entity_info_key_value'(Key, Value),
	'$lgt_valid_entity_info_list'(Tail).



% '$lgt_valid_entity_info_key_value'(+atom, @nonvar)
%
% true if the argument is a valid key-value pair

'$lgt_valid_entity_info_key_value'(author, Author) :-
	!,
	(	(atom(Author); nonvar(Author), Author = {EntityName}, atom(EntityName)) ->
		true
	;	throw(type_error(atom, Author))
	).

'$lgt_valid_entity_info_key_value'(comment, Comment) :-
	!,
	'$lgt_must_be'(atom, Comment).

'$lgt_valid_entity_info_key_value'(date, Date) :-
	!,
	(	Date = Year/Month/Day ->
		(	integer(Year) ->
			(	integer(Month) ->
				(	integer(Day) ->
					true
				;	throw(type_error(integer, Day))
				)
			;	throw(type_error(integer, Month))
			)
		;	throw(type_error(integer, Year))
		)
	;	throw(type_error(date, Date))
	).

'$lgt_valid_entity_info_key_value'(parameters, Parameters) :-
	!,
	(	'$lgt_is_proper_list'(Parameters) ->
		(	'$lgt_member'(Parameter, Parameters), \+ '$lgt_valid_entity_parameter'(Parameter) ->
			throw(type_error(parameter, Parameter))
		;	(	'$lgt_pp_entity'(_, Entity, _, _, _), \+ \+ Entity =.. [_| Parameters] ->
			 	true
			;	throw(length_error(parameters_list, Parameters))
			)
		)
	;	throw(type_error(list, Parameters))
	).

'$lgt_valid_entity_info_key_value'(parnames, Parnames) :-
	!,
	'$lgt_must_be'(list, Parnames),
	(	'$lgt_member'(Name, Parnames), \+ atom(Name) ->
		throw(type_error(atom, Name))
	;	(	'$lgt_pp_entity'(_, Entity, _, _, _), \+ \+ Entity =.. [_| Parnames] ->
		 	true
		;	throw(length_error(parnames_list, Parnames))
		)
	).

'$lgt_valid_entity_info_key_value'(version, Version) :-
	!,
	'$lgt_must_be'(atomic, Version).

'$lgt_valid_entity_info_key_value'(copyright, Copyright) :-
	!,
	(	(atom(Copyright); nonvar(Copyright), Copyright = {EntityName}, atom(EntityName)) ->
		true
	;	throw(type_error(atom, Copyright))
	).

'$lgt_valid_entity_info_key_value'(license, License) :-
	!,
	(	(atom(License); nonvar(License), License = {EntityName}, atom(EntityName)) ->
		true
	;	throw(type_error(atom, License))
	).

'$lgt_valid_entity_info_key_value'(_, _).



% '$lgt_tr_pred_info_list'(@list, +atom, +integer)
%
% true if the argument is a list of valid key-value pairs

'$lgt_tr_pred_info_list'(List, _, _) :-
	var(List),
	throw(instantiation_error). 

'$lgt_tr_pred_info_list'(List, _, _) :-
	\+ '$lgt_is_proper_list'(List),
	throw(type_error(list, List)).

'$lgt_tr_pred_info_list'([], _, _).

'$lgt_tr_pred_info_list'([Head| _], _, _) :-
	var(Head),
	throw(instantiation_error). 

'$lgt_tr_pred_info_list'([Head| _], _, _) :-
	Head \= (_ is _),
	throw(type_error(key_value_info_pair, Head)).

'$lgt_tr_pred_info_list'([Key is Value| _], _, _) :-
	(var(Key); var(Value)),
	throw(instantiation_error). 

'$lgt_tr_pred_info_list'([Key is _| _], _, _) :-
	\+ atom(Key),
	throw(type_error(atom, Key)). 

'$lgt_tr_pred_info_list'([Key is Value| Tail], Functor, Arity) :-
	'$lgt_tr_pred_info_key_value'(Key, Value, Functor, Arity),
	'$lgt_tr_pred_info_list'(Tail, Functor, Arity).



% '$lgt_tr_pred_info_key_value'(+atom, @nonvar, +atom, +integer)
%
% true if the argument is a valid key-value pair

'$lgt_tr_pred_info_key_value'(allocation, Allocation, _, _) :-
	!,
	'$lgt_must_be'(atom, Allocation),
	(	'$lgt_valid_predicate_allocation'(Allocation) ->
		true
	;	throw(domain_error(allocation, Allocation))
	).

'$lgt_tr_pred_info_key_value'(arguments, Arguments, Functor, Arity) :-
	!,
	'$lgt_must_be'(list, Arguments),
	(	'$lgt_member'(Argument, Arguments), \+ '$lgt_valid_predicate_argument'(Argument) ->
		throw(type_error(argument, Argument))
	;	(	functor(Pred, Functor, Arity), Pred =.. [_| Arguments] ->
		 	true
		; 	throw(length_error(arguments_list, Arguments))
		)
	).

'$lgt_tr_pred_info_key_value'(argnames, Argnames, Functor, Arity) :-
	!,
	'$lgt_must_be'(list, Argnames),
	(	'$lgt_member'(Name, Argnames), \+ atom(Name) ->
		throw(type_error(atom, Name))
	;	(	functor(Pred, Functor, Arity), Pred =.. [_| Argnames] ->
		 	true
		 ;	throw(length_error(argnames_list, Argnames))
		)
	).

'$lgt_tr_pred_info_key_value'(comment, Comment, _, _) :-
	!,
	'$lgt_must_be'(atom, Comment).

'$lgt_tr_pred_info_key_value'(exceptions, Exceptions, _, _) :-
	!,
	'$lgt_must_be'(list, Exceptions),
	(	'$lgt_member'(Exception, Exceptions), \+ '$lgt_valid_predicate_exception'(Exception) ->
		throw(type_error(exception, Exception))
	;	true
	).

'$lgt_tr_pred_info_key_value'(examples, Examples, Functor, Arity) :-
	!,
	'$lgt_must_be'(list, Examples),
	(	'$lgt_member'(Example, Examples), \+ '$lgt_valid_predicate_call_example'(Example, Functor, Arity) ->
		throw(type_error(example, Example))
	;	true
	).

'$lgt_tr_pred_info_key_value'(redefinition, Redefinition, _, _) :-
	!,
	'$lgt_must_be'(atom, Redefinition),
	(	'$lgt_valid_predicate_redefinition'(Redefinition) ->
		true
	;	throw(domain_error(redefinition, Redefinition))
	).

'$lgt_tr_pred_info_key_value'(_, _, _, _).



% '$lgt_split_operators_and_predicates'(+list, -list, -list).
%
% module/2 exports list and use_module/2 imports list may contain both operator declarations and predicate indicators

'$lgt_split_operators_and_predicates'([], [], []).

'$lgt_split_operators_and_predicates'([Element| _], _, _) :-
	var(Element),
	throw(instantiation_error).

'$lgt_split_operators_and_predicates'([op(Priority, Spec, Operator)| Elements], Preds, [op(Priority, Spec, Operator)| Ops]) :-
	!,
	'$lgt_split_operators_and_predicates'(Elements, Preds, Ops).

'$lgt_split_operators_and_predicates'([Pred| Elements], [Pred| Preds], Ops) :-
	'$lgt_split_operators_and_predicates'(Elements, Preds, Ops).



% '$lgt_tr_grammar_rules'(+list, @stream)

'$lgt_tr_grammar_rules'([], _).

'$lgt_tr_grammar_rules'([GrammarRule| GrammarRules], Ctx) :-
	'$lgt_tr_grammar_rule'(GrammarRule, Ctx),
	'$lgt_tr_grammar_rules'(GrammarRules, Ctx).



% '$lgt_tr_grammar_rule'(+grammar_rule, +atom, +position, @stream)

'$lgt_tr_grammar_rule'(GrammarRule, Ctx) :-
	'$lgt_dcg_rule_to_clause'(GrammarRule, Clause),
	'$lgt_tr_clause'(Clause, Ctx).



% '$lgt_tr_clauses'(+list, +ctx)

'$lgt_tr_clauses'([], _).

'$lgt_tr_clauses'([Clause| Clauses], Ctx) :-
	'$lgt_comp_ctx_mode'(Ctx, Mode),			% only the compilation context mode should
	'$lgt_comp_ctx_mode'(NewCtx, Mode),			% be shared between different clauses
	'$lgt_tr_clause'(Clause, NewCtx),
	'$lgt_tr_clauses'(Clauses, Ctx).



% '$lgt_tr_clause'(+clause, +ctx)

'$lgt_tr_clause'(Clause, Ctx) :-
	'$lgt_comp_ctx_mode'(Ctx, Mode),			% only the compilation context mode should
	'$lgt_comp_ctx_mode'(NewCtx, Mode),			% be shared between different clauses
	'$lgt_tr_clause'(Clause, NewCtx, NewCtx).	% assume for now that the head and the body compilation contexts are the same



% '$lgt_tr_clause'(+clause, +ctx, +ctx)
%
% translates a clause using different compilation contexts for the head and for the body;
% this is required in order to correctly compile clauses for e.g. coinductive predicates

'$lgt_tr_clause'(Clause, _, _) :-
	\+ '$lgt_pp_entity'(_, _, _, _, _),			% all clauses occuring before an opening entity directive
	!,
	'$lgt_pp_term_location'(Location),
	assertz('$lgt_pp_prolog_term_'(Clause, Location)).	% are copied unchanged to the generated Prolog file

'$lgt_tr_clause'(Clause, HeadCtx, BodyCtx) :-
	'$lgt_pp_entity'(Type, Entity, Prefix, _, _),
	(	Type == object, compound(Entity) ->		% if the entity is a parametric object we need
		'$lgt_comp_ctx_this'(HeadCtx, Entity),	% "this" for inline compilation of parameter/2
		'$lgt_comp_ctx_this'(BodyCtx, Entity)
	;	true
	),
	'$lgt_comp_ctx_prefix'(HeadCtx, Prefix),
	'$lgt_comp_ctx_prefix'(BodyCtx, Prefix),
	catch(
		'$lgt_tr_clause'(Clause, TClause, DClause, HeadCtx, BodyCtx),
		Error,
		throw(error(Error, clause(Clause)))),
	(	'$lgt_compiler_flag'(debug, on) ->
		(	'$lgt_comp_ctx_mode'(HeadCtx, compile(aux)) ->
			assertz('$lgt_pp_entity_aux_clause_'(DClause))
		;	'$lgt_pp_term_location'(Location),
			assertz('$lgt_pp_entity_clause_'(DClause, Location))
		)
	;	(	'$lgt_comp_ctx_mode'(HeadCtx, compile(aux)) ->
			assertz('$lgt_pp_entity_aux_clause_'(TClause))
		;	'$lgt_pp_term_location'(Location),
			assertz('$lgt_pp_entity_clause_'(TClause, Location))
		)
	),
	!.

'$lgt_tr_clause'(Clause, _, _) :-
	throw(error(unknown_error, clause(Clause))).



% '$lgt_tr_clause'(+clause, -clause, -clause, +ctx, +ctx)
%
% translates a clause (using different compilation contexts for the
%  head and for the body) into a normal clause and a debug clause

'$lgt_tr_clause'(Clause, _, _, _, _) :-
	var(Clause),
	throw(instantiation_error).

'$lgt_tr_clause'((Head:-_), _, _, _, _) :-
	var(Head),
	throw(instantiation_error).

'$lgt_tr_clause'((Head:-_), _, _, _, _) :-
	\+ callable(Head),
	throw(type_error(callable, Head)).

'$lgt_tr_clause'((_:-Body), _, _, _, _) :-
	nonvar(Body),
	\+ callable(Body),
	throw(type_error(callable, Body)).

'$lgt_tr_clause'((Annotation:-Body), TClause, DClause, HeadCtx, BodyCtx) :-
	'$lgt_pp_value_annotation_'(Annotation, Functor, Value, Head),
	!,
	'$lgt_tr_clause'((Head:-Body), TClause0, DClause, HeadCtx, BodyCtx),
	functor(TAnnotation, Functor, 2),
	(	TClause0 = (THead :- TBody) ->
		'$lgt_pp_value_annotation_'(TAnnotation, Functor, Value, THead),
		TClause = (TAnnotation :- TBody)
	;	TClause0 = THead,
		'$lgt_pp_value_annotation_'(TAnnotation, Functor, Value, THead),
		TClause = TAnnotation
	).

'$lgt_tr_clause'((Head:-Body), TClause, DClause, HeadCtx, BodyCtx) :-
	'$lgt_pp_coinductive_'(Head, CoinductiveHead),
	!,
	functor(Head, Functor, Arity),
	functor(HeadTemplate, Functor, Arity),
	functor(CoinductiveHead, CoinductiveFunctor, Arity),
	functor(CoinductiveHeadTemplate, CoinductiveFunctor, Arity),
	'$lgt_unify_head_thead_args'(Arity, HeadTemplate, CoinductiveHeadTemplate),
	'$lgt_comp_ctx_stack'(HeadCtx, HeadStack),
	'$lgt_comp_ctx_exec_ctx'(HeadCtx, ExCtx),
	'$lgt_exec_ctx'(ExCtx, Sender, This, Self, MetaCallCtx, HeadStack),
	(	retract('$lgt_pp_coinductive_flag_'(Head)) ->
		% the first clause for a coinductive predicate checks the stack of coinductive hypothesis for a match:
		'$lgt_tr_clause'((CoinductiveHeadTemplate :- {'$lgt_member'(HeadTemplate, HeadStack)}), HeadCtx, BodyCtx)
	;	% first clause already generated:
		true
	),
	'$lgt_comp_ctx_stack_new_stack'(BodyCtx, BodyStack, NewBodyCtx),
	'$lgt_comp_ctx_exec_ctx'(NewBodyCtx, BodyExCtx),
	'$lgt_exec_ctx'(BodyExCtx, Sender, This, Self, MetaCallCtx, BodyStack),
	% the remaining clauses for a coinductive predicate are modified in order to add the new ancestor goal to the coinductive hypothesis stack:
	'$lgt_tr_clause'((CoinductiveHead :- \+ {'$lgt_member'(Head, HeadStack)}, BodyStack = [Head| HeadStack], Body), TClause, DClause, HeadCtx, NewBodyCtx).

'$lgt_tr_clause'((Head:-Body), (THead:-'$lgt_nop'(Body), SBody), (THead:-'$lgt_nop'(Body),'$lgt_debugger.head'(Head, 0, ExCtx),DBody), HeadCtx, BodyCtx) :-
	functor(Head, Functor, Arity),
	'$lgt_pp_dynamic_'(Functor, Arity),
	!,
	'$lgt_pred_meta_vars'(Head, MetaVars),
	'$lgt_comp_ctx_meta_vars'(HeadCtx, MetaVars),
	'$lgt_tr_head'(Head, THead, HeadCtx),
	'$lgt_comp_ctx_meta_vars'(BodyCtx, MetaVars),
	'$lgt_tr_body'(Body, TBody, DBody, BodyCtx),
	(	'$lgt_compiler_flag'(optimize, on) ->
		'$lgt_simplify_body'(TBody, SBody)
	;	SBody = TBody
	),
	'$lgt_comp_ctx_exec_ctx'(HeadCtx, ExCtx).

'$lgt_tr_clause'((Head:-Body), TClause, (THead:-'$lgt_debugger.head'(Head, N, ExCtx),DBody), HeadCtx, BodyCtx) :-
	!,
	'$lgt_pred_meta_vars'(Head, MetaVars),
	'$lgt_comp_ctx_meta_vars'(HeadCtx, MetaVars),
	'$lgt_tr_head'(Head, THead, HeadCtx),
	'$lgt_comp_ctx_meta_vars'(BodyCtx, MetaVars),
	'$lgt_tr_body'(Body, TBody, DBody, BodyCtx),
	(	'$lgt_compiler_flag'(optimize, on) ->
		'$lgt_simplify_body'(TBody, SBody),
		(	SBody == true ->
			TClause = THead
		;	TClause = (THead:-SBody)
		)
	;	TClause = (THead:-TBody)
	),
	'$lgt_comp_ctx_exec_ctx'(HeadCtx, ExCtx),
	'$lgt_clause_number'(THead, N).

'$lgt_tr_clause'(Fact, _, _, _, _) :-
	\+ callable(Fact),
	throw(type_error(callable, Fact)).

'$lgt_tr_clause'(Annotation, TFact, DFact, _, BodyCtx) :-
	'$lgt_pp_value_annotation_'(Annotation, Functor, Value, Body),
	!,
	'$lgt_comp_ctx_meta_vars'(BodyCtx, []),
	'$lgt_tr_body'(Body, TBody, DBody, BodyCtx),
	functor(TFact, Functor, 2),
	'$lgt_pp_value_annotation_'(TFact, Functor, Value, TBody),
	functor(DFact, Functor, 2),
	'$lgt_pp_value_annotation_'(DFact, Functor, Value, DBody).

'$lgt_tr_clause'(Annotation, TFact, DFact, _, BodyCtx) :-
	'$lgt_pp_goal_annotation_'(Annotation, Functor, Left, Right),
	!,
	'$lgt_comp_ctx_meta_vars'(BodyCtx, []),
	'$lgt_tr_body'(Left, TLeft, DLeft, BodyCtx),
	'$lgt_tr_body'(Right, TRight, DRight, BodyCtx),
	functor(TFact, Functor, 2),
	'$lgt_pp_goal_annotation_'(TFact, Functor, TLeft, TRight),
	functor(DFact, Functor, 2),
	'$lgt_pp_goal_annotation_'(DFact, Functor, DLeft, DRight).

'$lgt_tr_clause'(Fact, TFact, DFact, HeadCtx, BodyCtx) :-
	'$lgt_pp_coinductive_'(Fact, CoinductiveFact),
	!,
	functor(Fact, Functor, Arity),
	functor(FactTemplate, Functor, Arity),
	functor(CoinductiveFact, CoinductiveFunctor, Arity),
	functor(CoinductiveFactTemplate, CoinductiveFunctor, Arity),
	'$lgt_unify_head_thead_args'(Arity, FactTemplate, CoinductiveFactTemplate),
	'$lgt_comp_ctx_stack'(HeadCtx, HeadStack),
	'$lgt_comp_ctx_exec_ctx'(HeadCtx, ExCtx),
	'$lgt_exec_ctx'(ExCtx, _, _, _, _, HeadStack),
	(	retract('$lgt_pp_coinductive_flag_'(Fact)) ->
		% the first clause for a coinductive predicate checks the stack of coinductive hypothesis for a match:
		'$lgt_tr_clause'((CoinductiveFactTemplate :- {'$lgt_member'(FactTemplate, HeadStack)}), HeadCtx, BodyCtx)
	;	% first clause already generated:
		true
	),
	'$lgt_tr_clause'(CoinductiveFact, TFact, DFact, HeadCtx, BodyCtx).

'$lgt_tr_clause'(Fact, TFact, (TFact:-'$lgt_debugger.fact'(Fact, N, ExCtx)), HeadCtx, _) :-
	'$lgt_tr_head'(Fact, TFact, HeadCtx),
	'$lgt_comp_ctx_exec_ctx'(HeadCtx, ExCtx),
	'$lgt_clause_number'(TFact, N).


'$lgt_clause_number'(THead, N) :-
	'$lgt_term_template'(THead, Template), 
	findall(1, ('$lgt_pp_entity_clause_'(Template, _); '$lgt_pp_entity_clause_'((Template :- _), _)), List),
	'$lgt_length'(List, 1, N).

'$lgt_length'([], Length, Length).
'$lgt_length'([_| Tail], Length0, Length) :-
	Length1 is Length0 + 1,
	'$lgt_length'(Tail, Length1, Length).



% '$lgt_tr_head'(+callable, -callable, +Ctx)
%
% translates an entity clause head


% pre-compiled clause head (we only check for some basic errors)

'$lgt_tr_head'({Head}, Head, _) :-
	!,
	'$lgt_must_be'(callable, Head).


% definition of dynamic predicates inside categories

'$lgt_tr_head'(Head, _, _) :-
	'$lgt_pp_category_'(_, _, _, _, _, _),
	functor(Head, Functor, Arity), 
	'$lgt_pp_dynamic_'(Functor, Arity),
	throw(permission_error(define, dynamic_predicate, Functor/Arity)).


% redefinition of Logtalk message sending and remaining control constructs
% (note that ::/2 can be used in a clause head when defining multifile predicates
% and that {}/1 can be used to represent a pre-compiled clause head)

'$lgt_tr_head'(::_, _, _) :-
	throw(permission_error(modify, control_construct, (::)/1)).

'$lgt_tr_head'(^^_, _, _) :-
	throw(permission_error(modify, control_construct, (^^)/1)).

'$lgt_tr_head'(_<<_, _, _) :-
	throw(permission_error(modify, control_construct, (<<)/2)).

'$lgt_tr_head'(_>>_, _, _) :-
	throw(permission_error(modify, control_construct, (>>)/2)).

'$lgt_tr_head'(':'(_), _, _) :-
	throw(permission_error(modify, control_construct, (:)/1)).

'$lgt_tr_head'((_, _), _, _) :-
	throw(permission_error(modify, control_construct, (',')/2)).

'$lgt_tr_head'((_; _), _, _) :-
	throw(permission_error(modify, control_construct, (;)/2)).

'$lgt_tr_head'((_ -> _), _, _) :-
	throw(permission_error(modify, control_construct, (->)/2)).

'$lgt_tr_head'(!, _, _) :-
	throw(permission_error(modify, control_construct, !/0)).


% redefinition of Logtalk built-in methods

'$lgt_tr_head'(Head, _, _) :-
	'$lgt_built_in_method'(Head, _, _, Flags),
	Flags /\ 2 =\= 2,	% static built-in predicate
	functor(Head, Functor, Arity), 
	throw(permission_error(modify, built_in_method, Functor/Arity)).


% conflict with a predicate specified in a uses/2 directive

'$lgt_tr_head'(Alias, _, _) :-
	'$lgt_pp_uses_predicate_'(_, _, Alias),
	functor(Alias, Functor, Arity),
	throw(permission_error(modify, uses_object_predicate, Functor/Arity)).


% conflict with a predicate specified in a use_module/2 directive

'$lgt_tr_head'(Alias, _, _) :-
	'$lgt_pp_use_module_predicate_'(_, _, Alias),
	functor(Alias, Functor, Arity),
	throw(permission_error(modify, uses_module_predicate, Functor/Arity)).


% non-variable meta-argument in clause head of a user-defined meta-predicate

'$lgt_tr_head'(Head, _, _) :-
	'$lgt_term_template'(Head, Meta),
	'$lgt_pp_meta_predicate_'(Meta),
	Head =.. [_| Args],
	Meta =.. [_| MArgs],
	'$lgt_nonvar_meta_arg'(Args, MArgs, Arg),
	throw(type_error(variable, Arg)).


% redefinition of Logtalk built-in predicates

'$lgt_tr_head'(Head, _, _) :-
	'$lgt_lgt_built_in'(Head),
	\+ functor(Head, '::', 2),							% workaround for the nasty habit of using multifile entity predicates
	'$lgt_compiler_flag'(lgtredef, warning),
	\+ '$lgt_compiler_flag'(report, off),
	\+ '$lgt_pp_redefined_built_in_'(Head, _, _),		% not already reported?
	functor(Head, Functor, Arity),
	'$lgt_report_warning_in_new_line',
	'$lgt_inc_compile_warnings_counter',
	write('%         WARNING!  Redefining a Logtalk built-in predicate: '),
	writeq(Functor/Arity), nl,
	'$lgt_pp_entity'(Type, Entity, _, _, _),
	'$lgt_report_warning_full_context'(Type, Entity),
	fail.


% redefinition of Prolog built-in predicates

'$lgt_tr_head'(Head, _, _) :-
	'$lgt_pl_built_in'(Head),
	\+ functor(Head, ':', 2),							% workaround for the nasty habit of using multifile module predicates
	'$lgt_compiler_flag'(plredef, warning),
	\+ '$lgt_compiler_flag'(report, off),
	\+ '$lgt_pp_redefined_built_in_'(Head, _, _),		% not already reported?
	functor(Head, Functor, Arity),
	'$lgt_report_warning_in_new_line',
	'$lgt_inc_compile_warnings_counter',
	write('%         WARNING!  Redefining a Prolog built-in predicate: '),
	writeq(Functor/Arity), nl,
	'$lgt_pp_entity'(Type, Entity, _, _, _),
	'$lgt_report_warning_full_context'(Type, Entity),
	fail.


% definition of event handlers without reference to the "monitoring" built-in protocol

'$lgt_tr_head'(Head, _, _) :-
	\+ '$lgt_pp_module_'(_),
	functor(Head, Functor, 3),
	once((Functor == before; Functor == after)),
	\+ '$lgt_pp_implemented_protocol_'(monitoring, _, _, _),
	\+ '$lgt_compiler_flag'(report, off),
	'$lgt_report_warning_in_new_line',
	'$lgt_inc_compile_warnings_counter',
	write('%         WARNING!  Missing reference to the "monitoring" built-in protocol: '),
	writeq(Functor/3), nl,
	'$lgt_pp_entity'(Type, Entity, _, _, _),
	'$lgt_report_warning_full_context'(Type, Entity),
	fail.


% definition of term and goal expansion predicates without reference to the "expanding" built-in protocol

'$lgt_tr_head'(Head, _, _) :-
	\+ '$lgt_pp_module_'(_),
	functor(Head, Functor, 2),
	once((Functor == term_expansion; Functor == goal_expansion)),
	\+ '$lgt_pp_implemented_protocol_'(expanding, _, _, _),
	\+ '$lgt_compiler_flag'(report, off),
	'$lgt_report_warning_in_new_line',
	'$lgt_inc_compile_warnings_counter',
	write('%         WARNING!  Missing reference to the "expanding" built-in protocol: '),
	writeq(Functor/2), nl,
	'$lgt_pp_entity'(Type, Entity, _, _, _),
	'$lgt_report_warning_full_context'(Type, Entity),
	fail.


% translate the head of a clause of another entity predicate (which we assume declared multifile)

'$lgt_tr_head'(Other::_, _, _) :-
	var(Other),
	throw(instantiation_error).

'$lgt_tr_head'(_::Head, _, _) :-
	var(Head),
	throw(instantiation_error).

'$lgt_tr_head'(Other::_, _, _) :-
	\+ callable(Other),
	throw(type_error(entity_identifier, Other)).

'$lgt_tr_head'(_::Head, _, _) :-
	\+ callable(Head),
	throw(type_error(callable, Head)).

'$lgt_tr_head'(user::Head, Head, Ctx) :-
	!,
	functor(Head, Functor, Arity),
	(	'$lgt_pp_directive_'(multifile(Functor/Arity)) ->
		true
	;	'$lgt_compiler_flag'(report, off) ->
		true
	;	'$lgt_compiler_flag'(missing_directives, warning) ->
		'$lgt_report_warning_in_new_line',
		'$lgt_inc_compile_warnings_counter',
		write('%         WARNING!  Missing multifile directive for the predicate: '),
		writeq(user::Functor/Arity), nl,
		'$lgt_pp_entity'(Type, Entity, _, _, _),
		'$lgt_report_warning_full_context'(Type, Entity)
	;	true
	),
	'$lgt_comp_ctx_head'(Ctx, user::Head).

'$lgt_tr_head'(Other::Head, THead, Ctx) :-
	!,
	functor(Head, Functor, Arity),
	'$lgt_construct_entity_prefix'(Other, _, Prefix),
	'$lgt_construct_predicate_indicator'(Prefix, Functor/Arity, TFunctor/TArity),
	(	'$lgt_pp_directive_'(multifile(TFunctor/TArity)) ->
		true
	;	throw(existence_error(multifile_declaration, Other::Head))
	),
	functor(THead, TFunctor, TArity),
	'$lgt_unify_head_thead_args'(Arity, Head, THead),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	arg(TArity, THead, ExCtx),
	'$lgt_comp_ctx_head'(Ctx, Other::Head).

% translate the head of a clause of a module predicate (which we assume declared multifile)

'$lgt_tr_head'(':'(Module, Head), THead, Ctx) :-
	'$lgt_pl_built_in'(':'(_, _)),
	!,
	'$lgt_must_be'(atom, Module),
	'$lgt_must_be'(callable, Head),
	functor(Head, Functor, Arity),
	(	Module == user ->
		THead = Head
	;	THead = ':'(Module, Head)
	),
	(	Module == user, '$lgt_pp_directive_'(multifile(Functor/Arity)) ->
		true
	;	'$lgt_pp_directive_'(multifile(':'(Module, Functor/Arity))) ->
		true
	;	'$lgt_compiler_flag'(report, off) ->
		true
	;	'$lgt_compiler_flag'(missing_directives, warning) ->
		'$lgt_report_warning_in_new_line',
		'$lgt_inc_compile_warnings_counter',
		write('%         WARNING!  Missing multifile directive for the predicate: '),
		writeq(':'(Module,Functor/Arity)), nl,
		'$lgt_pp_entity'(Type, Entity, _, _, _),
		'$lgt_report_warning_full_context'(Type, Entity)
	;	true
	),
	'$lgt_comp_ctx_head'(Ctx, ':'(Module, Head)).


% translate the head of a clause of a user defined predicate

'$lgt_tr_head'(Head, THead, Ctx) :-
	'$lgt_comp_ctx_head'(Ctx, Head),
	functor(Head, Functor, Arity),
	(	'$lgt_pp_dynamic_'(Functor, Arity),
		\+ '$lgt_pp_public_'(Functor, Arity),
		\+ '$lgt_pp_protected_'(Functor, Arity),
		\+ '$lgt_pp_private_'(Functor, Arity) ->
		'$lgt_add_ddef_clause'(Head, Functor, Arity, THead, Ctx)
	;	'$lgt_add_def_clause'(Head, Functor, Arity, THead, Ctx)
	).



% look for a non-variable meta-argument
% (used when checking meta-predicate clause heads for meta-argument unification errors)

'$lgt_nonvar_meta_arg'([Arg| _], [N| _], Arg) :-
	integer(N),
	nonvar(Arg).

'$lgt_nonvar_meta_arg'([_| Args], [_| MArgs], Arg) :-
	'$lgt_nonvar_meta_arg'(Args, MArgs, Arg).



% '$lgt_tr_body'(+callable, -callable, -callable, +term)
%
% translates an entity clause body


% calls in the context of the pseudo-object "user"

'$lgt_tr_body'(Pred, Pred, '$lgt_debugger.goal'(Pred, Pred, ExCtx), Ctx) :-
	'$lgt_comp_ctx_this'(Ctx, This),
	This == user,
	!,
 	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx).


% meta-calls

'$lgt_tr_body'(Pred, TPred, '$lgt_debugger.goal'(Pred, TPred, ExCtx), Ctx) :-
	var(Pred),
	!,
	'$lgt_comp_ctx'(Ctx, _, Sender, This, Self, _, MetaVars, MetaCallCtx, ExCtx, _, _),
	(	'$lgt_member_var'(Pred, MetaVars) ->
		% we're compiling a clause for a meta-predicate; therefore, we need
		% to connect the execution context and the meta-call context arguments
		'$lgt_exec_ctx'(ExCtx, Sender, This, Self, MetaCallCtx, _),
		TPred = '$lgt_metacall'(Pred, MetaCallCtx, Sender, This, Self)
	;	% we're either compiling a clause for a normal predicate (i.e. MetaVars == [])
		% or the meta-call should be local as it corresponds to a non meta-argument
		'$lgt_exec_ctx'(ExCtx, Sender, This, Self, _, _),
		TPred = '$lgt_metacall'(Pred, [], Sender, This, Self)
	).


% pre-processor bypass (call of external code)

'$lgt_tr_body'({Pred}, _, _, _) :-
	nonvar(Pred),
	\+ callable(Pred),
	throw(type_error(callable, Pred)).

'$lgt_tr_body'({Pred}, call(Pred), '$lgt_debugger.goal'({Pred}, call(Pred), ExCtx), Ctx) :-
	var(Pred),
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx).

'$lgt_tr_body'({Pred}, Pred, '$lgt_debugger.goal'({Pred}, Pred, ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx).


% goal expansion (only applied at compile time)

'$lgt_tr_body'(Pred, TPred, DPred, Ctx) :-
	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
	'$lgt_tr_expand_goal'(Pred, EPred),
	!,
	'$lgt_tr_body'(EPred, TPred, DPred, Ctx).


% bagof/3 and setof/3 existential quantifiers

'$lgt_tr_body'(Var^Pred, Var^TPred, Var^DPred, Ctx) :-
	!,
	'$lgt_tr_body'(Pred, TPred, DPred, Ctx).


% control constructs

'$lgt_tr_body'((Pred1, Pred2), (TPred1, TPred2), (DPred1, DPred2), Ctx) :-
	!,
	'$lgt_tr_body'(Pred1, TPred1, DPred1, Ctx),
	'$lgt_tr_body'(Pred2, TPred2, DPred2, Ctx).

'$lgt_tr_body'((Pred1; Pred2), (TPred1; TPred2), (DPred1; DPred2), Ctx) :-
	!,
	'$lgt_tr_body'(Pred1, TPred1, DPred1, Ctx),
	'$lgt_tr_body'(Pred2, TPred2, DPred2, Ctx).

'$lgt_tr_body'((Pred1 -> Pred2), (TPred1 -> TPred2), (DPred1 -> DPred2), Ctx) :-
	!,
	'$lgt_tr_body'(Pred1, TPred1, DPred1, Ctx),
	'$lgt_tr_body'(Pred2, TPred2, DPred2, Ctx).

'$lgt_tr_body'(\+ Pred, \+ TPred, '$lgt_debugger.goal'(\+ Pred, \+ DPred, ExCtx), Ctx) :-
	!,
	'$lgt_tr_body'(Pred, TPred, DPred, Ctx),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx).

'$lgt_tr_body'(!, !, ('$lgt_debugger.goal'(!, true, ExCtx), !), Ctx) :-
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx).

'$lgt_tr_body'(true, true, '$lgt_debugger.goal'(true, true, ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx).

'$lgt_tr_body'(fail, fail, '$lgt_debugger.goal'(fail, fail, ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx).

'$lgt_tr_body'(repeat, repeat, '$lgt_debugger.goal'(repeat, repeat, ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx).

'$lgt_tr_body'(call(Pred), call(TPred), '$lgt_debugger.goal'(call(Pred), call(DPred), ExCtx), Ctx) :-
	!,		% we must keep the call/1 wrapper in order to preserve cut semantics
	'$lgt_tr_body'(Pred, TPred, DPred, Ctx),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx).

'$lgt_tr_body'(CallN, TPred, DPred, Ctx) :-
	CallN =.. [call, Closure| ExtraArgs],
	!,
	'$lgt_check_closure'(Closure, Ctx),
	'$lgt_tr_body'('$lgt_callN'(Closure, ExtraArgs), TPred, DPred, Ctx).

'$lgt_tr_body'('$lgt_callN'(Closure, ExtraArgs), _, _, Ctx) :-
	var(Closure),
	'$lgt_comp_ctx'(Ctx, Head, _, _, _, _, MetaVars, _, _, _, _),
	nonvar(Head),
	Head \= ':'(_, _),	% ignore multifile
	Head \= _::_,		% predicates
	'$lgt_term_template'(Head, Meta),
	'$lgt_pp_meta_predicate_'(Meta),			% if we're compiling a clause for a meta-predicate and
	once('$lgt_member_var'(Closure, MetaVars)),	% our closure is a meta-argument then check that the
	'$lgt_length'(ExtraArgs, 0, NExtraArgs),	% call/N call complies with the meta-predicate declaration
	Meta =.. [_| MetaArgs],
	\+ '$lgt_same_meta_arg_extra_args'(MetaArgs, MetaVars, Closure, NExtraArgs),
	CallN =.. [call, Closure| ExtraArgs],
	throw(arity_mismatch(closure, CallN, Meta)).

'$lgt_tr_body'('$lgt_callN'(Closure, ExtraArgs), TPred, DPred, Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, Sender, This, Self, _, MetaVars, MetaCallCtx, ExCtx, _, _),
	(	var(Closure), '$lgt_member_var'(Closure, MetaVars) ->
		% we're compiling a clause for a meta-predicate; therefore, we need
		% to connect the execution context and the meta-call context arguments
		'$lgt_exec_ctx'(ExCtx, Sender, This, Self, MetaCallCtx, _),
		TPred = '$lgt_metacall'(Closure, ExtraArgs, MetaCallCtx, Sender, This, Self)
	;	% we're either compiling a clause for a normal predicate (i.e. MetaVars == [])
		% or the meta-call should be local as it corresponds to a non meta-argument
		% or the meta-call is an explicitly qualifed call (::/2, ::/1, :/2) or a lambda expression (>>/2, (/)/2)
		'$lgt_exec_ctx'(ExCtx, Sender, This, Self, _, _),
		TPred = '$lgt_metacall'(Closure, ExtraArgs, [], Sender, This, Self)
	),
	CallN =.. [call, Closure| ExtraArgs],
	DPred = '$lgt_debugger.goal'(CallN, TPred, ExCtx).

'$lgt_tr_body'(once(Pred), (TPred -> true; fail), '$lgt_debugger.goal'(once(Pred), (DPred -> true; fail), ExCtx), Ctx) :-
	!,
	'$lgt_tr_body'(Pred, TPred, DPred, Ctx),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx).

'$lgt_tr_body'(ignore(Pred), (TPred -> true; true), '$lgt_debugger.goal'(ignore(Pred), (DPred -> true; true), ExCtx), Ctx) :-
	!,
	'$lgt_tr_body'(Pred, TPred, DPred, Ctx),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx).

'$lgt_tr_body'(catch(Goal, Catcher, Recovery), catch(TGoal, Catcher, TRecovery), '$lgt_debugger.goal'(catch(Goal, Catcher, Recovery), catch(DGoal, Catcher, DRecovery), ExCtx), Ctx) :-
	!,
	'$lgt_tr_body'(Goal, TGoal, DGoal, Ctx),
	'$lgt_tr_body'(Recovery, TRecovery, DRecovery, Ctx),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx).

'$lgt_tr_body'(throw(Error), throw(Error), '$lgt_debugger.goal'(throw(Error), throw(Error), ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx).


% lambda expressions support predicates

'$lgt_tr_body'(Parameters>>Goal, _, _, Ctx) :-
	'$lgt_check_lambda_expression'(Parameters>>Goal, Ctx),
	fail.

'$lgt_tr_body'(Free/Parameters>>Goal, TPred, DPred, Ctx) :-
	nonvar(Parameters),
	!,
	(	Parameters == [] ->
		'$lgt_tr_body'(Free/Goal, TPred, DPred, Ctx)
	;	throw(representation_error(lambda_parameters))
	).

'$lgt_tr_body'(Free/Parameters>>Goal, TPred, DPred, Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, Sender, This, Self, _, MetaVars, MetaCallCtx, ExCtx, _, _),
	(	var(Goal), '$lgt_member_var'(Goal, MetaVars) ->
		% we're compiling a clause for a meta-predicate; therefore, we need
		% to connect the execution context and the meta-call context arguments
		'$lgt_exec_ctx'(ExCtx, Sender, This, Self, MetaCallCtx, _),
		TPred = '$lgt_metacall'(Free/Parameters>>Goal, [], MetaCallCtx, Sender, This, Self)
	;	% we're either compiling a clause for a normal predicate (i.e. MetaVars == [])
		% or the meta-call should be local as it corresponds to a non meta-argument
		% or the meta-call is an explicitly qualifed call (::/2, ::/1, :/2)
		'$lgt_exec_ctx'(ExCtx, Sender, This, Self, _, _),
		TPred = '$lgt_metacall'(Free/Parameters>>Goal, [], [], Sender, This, Self)
	),
	DPred = '$lgt_debugger.goal'(Free/Parameters>>Goal, TPred, ExCtx).

'$lgt_tr_body'(Parameters>>Goal, TPred, DPred, Ctx) :-
	nonvar(Parameters),
	!,
	(	Parameters == [] ->
		'$lgt_tr_body'(Goal, TPred, DPred, Ctx)
	;	throw(representation_error(lambda_parameters))
	).

'$lgt_tr_body'(Parameters>>Goal, TPred, DPred, Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, Sender, This, Self, _, MetaVars, MetaCallCtx, ExCtx, _, _),
	(	var(Goal), '$lgt_member_var'(Goal, MetaVars) ->
		% we're compiling a clause for a meta-predicate; therefore, we need
		% to connect the execution context and the meta-call context arguments
		'$lgt_exec_ctx'(ExCtx, Sender, This, Self, MetaCallCtx, _),
		TPred = '$lgt_metacall'(Parameters>>Goal, [], MetaCallCtx, Sender, This, Self)
	;	% we're either compiling a clause for a normal predicate (i.e. MetaVars == [])
		% or the meta-call should be local as it corresponds to a non meta-argument
		% or the meta-call is an explicitly qualifed call (::/2, ::/1, :/2)
		'$lgt_exec_ctx'(ExCtx, Sender, This, Self, _, _),
		TPred = '$lgt_metacall'(Parameters>>Goal, [], [], Sender, This, Self)
	),
	DPred = '$lgt_debugger.goal'(Parameters>>Goal, TPred, ExCtx).

'$lgt_tr_body'(Free/Goal, _, _, Ctx) :-
	'$lgt_check_lambda_expression'(Free/Goal, Ctx),
	fail.

'$lgt_tr_body'(Free/Goal, TPred, DPred, Ctx) :-
	nonvar(Free),
	nonvar(Goal),
	!,
	(	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
	 	'$lgt_comp_ctx_meta_vars'(Ctx, []) ->
		% generate an auxiliary predicate to replace the lambda expression
		'$lgt_gen_aux_predicate_functor'('_lambda_', Functor),
		(	Free = {Terms} ->
			'$lgt_conjunction_to_list'(Terms, Args)
		;	Args = []
		),
		Head =.. [Functor| Args],
		'$lgt_compile_aux_clauses'([(Head :- Goal)]),
		'$lgt_tr_body'(Head, TPred, DPred, Ctx)
	;	% either runtime traslation or the lambda expression appears in the
		% body of a meta-predicate clause
		'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
		'$lgt_tr_body'(Goal, TGoal, DGoal, Ctx),
		TPred = '$lgt_lambda'(Free, TGoal),
		DPred = '$lgt_debugger.goal'(Free/Goal, '$lgt_lambda'(Free, DGoal), ExCtx)
	).

'$lgt_tr_body'(Free/Goal, TPred, DPred, Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, Sender, This, Self, _, MetaVars, MetaCallCtx, ExCtx, _, _),
	(	var(Goal), '$lgt_member_var'(Goal, MetaVars) ->
		% we're compiling a clause for a meta-predicate; therefore, we need
		% to connect the execution context and the meta-call context arguments
		'$lgt_exec_ctx'(ExCtx, Sender, This, Self, MetaCallCtx, _),
		TPred = '$lgt_metacall'(Free/Goal, [], MetaCallCtx, Sender, This, Self)
	;	% we're either compiling a clause for a normal predicate (i.e. MetaVars == [])
		% or the meta-call should be local as it corresponds to a non meta-argument
		% or the meta-call is an explicitly qualifed call (::/2, ::/1, :/2)
		'$lgt_exec_ctx'(ExCtx, Sender, This, Self, _, _),
		TPred = '$lgt_metacall'(Free/Goal, [], [], Sender, This, Self)
	),
	DPred = '$lgt_debugger.goal'(Free/Goal, TPred, ExCtx).


% built-in meta-predicates

'$lgt_tr_body'(bagof(Term, Pred, List), bagof(Term, TPred, List), '$lgt_debugger.goal'(bagof(Term, Pred, List), bagof(Term, DPred, List), ExCtx), Ctx) :-
	!,
	'$lgt_tr_body'(Pred, TPred, DPred, Ctx),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx).

'$lgt_tr_body'(findall(Term, Pred, List), findall(Term, TPred, List), '$lgt_debugger.goal'(findall(Term, Pred, List), findall(Term, DPred, List), ExCtx), Ctx) :-
	!,
	'$lgt_tr_body'(Pred, TPred, DPred, Ctx),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx).

'$lgt_tr_body'(forall(Gen, Test), \+ (TGen, \+ TTest), '$lgt_debugger.goal'(forall(Gen, Test), \+ (DGen, \+ DTest), ExCtx), Ctx) :-
	!,
	'$lgt_tr_body'(Gen, TGen, DGen, Ctx),
	'$lgt_tr_body'(Test, TTest, DTest, Ctx),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx).

'$lgt_tr_body'(setof(Term, Pred, List), setof(Term, TPred, List), '$lgt_debugger.goal'(setof(Term, Pred, List), setof(Term, DPred, List), ExCtx), Ctx) :-
	!,
	'$lgt_tr_body'(Pred, TPred, DPred, Ctx),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx).


% multi-threading meta-predicates

'$lgt_tr_body'(threaded(_), _, _, _) :-
	\+ '$lgt_pp_threaded_',
	'$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, _, _),
	throw(resource_error(threads)).

'$lgt_tr_body'(threaded(Goals), _, _, _) :-
	nonvar(Goals),
	\+ callable(Goals),
	throw(type_error(callable, Goals)).

'$lgt_tr_body'(threaded(Goals), MTGoals, '$lgt_debugger.goal'(threaded(Goals), MDGoals, ExCtx), Ctx) :-
	!,
	'$lgt_tr_body'(Goals, TGoals, DGoals, Ctx),
	'$lgt_tr_threaded_call'(TGoals, MTGoals),
	'$lgt_tr_threaded_call'(DGoals, MDGoals),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx).


'$lgt_tr_body'(threaded_call(_, _), _, _, _) :-
	\+ '$lgt_pp_threaded_',
	'$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, _, _),
	throw(resource_error(threads)).

'$lgt_tr_body'(threaded_call(_, Tag), _, _, _) :-
	nonvar(Tag),
	throw(type_error(variable, Tag)).

'$lgt_tr_body'(threaded_call(Goal, _), _, _, _) :-
	nonvar(Goal),
	\+ callable(Goal),
	throw(type_error(callable, Goal)).

'$lgt_tr_body'(threaded_call(Goal, Tag), MTGoal, '$lgt_debugger.goal'(threaded_call(Goal, Tag), MDGoal, ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, _, This, Self, _, _, _, ExCtx, _, _),
	'$lgt_tr_body'(Goal, TGoal, DGoal, Ctx),
	(	'$lgt_pp_object_'(_, Prefix, _, _, _, _, _, _, _, _, _) ->
		MTGoal = '$lgt_threaded_call_tagged'(Prefix, TGoal, This, Self, Tag),
		MDGoal = '$lgt_threaded_call_tagged'(Prefix, DGoal, This, Self, Tag)
	;	% compiling a category
		MTGoal = '$lgt_threaded_call_tagged'(TGoal, This, Self, Tag),
		MDGoal = '$lgt_threaded_call_tagged'(DGoal, This, Self, Tag)
	),
	'$lgt_exec_ctx'(ExCtx, _, This, Self, _, _).


'$lgt_tr_body'(threaded_call(_), _, _, _) :-
	\+ '$lgt_pp_threaded_',
	'$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, _, _),
	throw(resource_error(threads)).

'$lgt_tr_body'(threaded_call(Goal), _, _, _) :-
	nonvar(Goal),
	\+ callable(Goal),
	throw(type_error(callable, Goal)).

'$lgt_tr_body'(threaded_call(Goal), MTGoal, '$lgt_debugger.goal'(threaded_call(Goal), MDGoal, ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, _, This, Self, _, _, _, ExCtx, _, _),
	'$lgt_tr_body'(Goal, TGoal, DGoal, Ctx),
	(	'$lgt_pp_object_'(_, Prefix, _, _, _, _, _, _, _, _, _) ->
		MTGoal = '$lgt_threaded_call'(Prefix, TGoal, This, Self),
		MDGoal = '$lgt_threaded_call'(Prefix, DGoal, This, Self)
	;	% compiling a category
		MTGoal = '$lgt_threaded_call'(TGoal, This, Self),
		MDGoal = '$lgt_threaded_call'(DGoal, This, Self)
	),
	'$lgt_exec_ctx'(ExCtx, _, This, Self, _, _).


'$lgt_tr_body'(threaded_once(_, _), _, _, _) :-
	\+ '$lgt_pp_threaded_',
	'$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, _, _),
	throw(resource_error(threads)).

'$lgt_tr_body'(threaded_once(_, Tag), _, _, _) :-
	nonvar(Tag),
	throw(type_error(variable, Tag)).

'$lgt_tr_body'(threaded_once(Goal, _), _, _, _) :-
	nonvar(Goal),
	\+ callable(Goal),
	throw(type_error(callable, Goal)).

'$lgt_tr_body'(threaded_once(Goal, Tag), MTGoal, '$lgt_debugger.goal'(threaded_once(Goal, Tag), MDGoal, ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, _, This, Self, _, _, _, ExCtx, _, _),
	'$lgt_tr_body'(Goal, TGoal, DGoal, Ctx),
	(	'$lgt_pp_object_'(_, Prefix, _, _, _, _, _, _, _, _, _) ->
		MTGoal = '$lgt_threaded_once_tagged'(Prefix, TGoal, This, Self, Tag),
		MDGoal = '$lgt_threaded_once_tagged'(Prefix, DGoal, This, Self, Tag)
	;	% compiling a category
		MTGoal = '$lgt_threaded_once_tagged'(TGoal, This, Self, Tag),
		MDGoal = '$lgt_threaded_once_tagged'(DGoal, This, Self, Tag)
	),
	'$lgt_exec_ctx'(ExCtx, _, This, Self, _, _).


'$lgt_tr_body'(threaded_once(_), _, _, _) :-
	\+ '$lgt_pp_threaded_',
	'$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, _, _),
	throw(resource_error(threads)).

'$lgt_tr_body'(threaded_once(Goal), _, _, _) :-
	nonvar(Goal),
	\+ callable(Goal),
	throw(type_error(callable, Goal)).

'$lgt_tr_body'(threaded_once(Goal), MTGoal, '$lgt_debugger.goal'(threaded_once(Goal), MDGoal, ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, _, This, Self, _, _, _, ExCtx, _, _),
	'$lgt_tr_body'(Goal, TGoal, DGoal, Ctx),
	(	'$lgt_pp_object_'(_, Prefix, _, _, _, _, _, _, _, _, _) ->
		MTGoal = '$lgt_threaded_once'(Prefix, TGoal, This, Self),
		MDGoal = '$lgt_threaded_once'(Prefix, DGoal, This, Self)
	;	% compiling a category
		MTGoal = '$lgt_threaded_once'(TGoal, This, Self),
		MDGoal = '$lgt_threaded_once'(DGoal, This, Self)
	),
	'$lgt_exec_ctx'(ExCtx, _, This, Self, _, _).


'$lgt_tr_body'(threaded_ignore(_), _, _, _) :-
	\+ '$lgt_pp_threaded_',
	'$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, _, _),
	throw(resource_error(threads)).

'$lgt_tr_body'(threaded_ignore(Goal), _, _, _) :-
	nonvar(Goal),
	\+ callable(Goal),
	throw(type_error(callable, Goal)).

'$lgt_tr_body'(threaded_ignore(Goal), MTGoal, '$lgt_debugger.goal'(threaded_ignore(Goal), MDGoal, ExCtx), Ctx) :-
	!,
	'$lgt_tr_body'(Goal, TGoal, DGoal, Ctx),
	MTGoal = '$lgt_threaded_ignore'(TGoal),
	MDGoal = '$lgt_threaded_ignore'(DGoal),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx).


'$lgt_tr_body'(threaded_exit(_, _), _, _, _) :-
	\+ '$lgt_pp_threaded_',
	'$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, _, _),
	throw(resource_error(threads)).

'$lgt_tr_body'(threaded_exit(Goal, _), _, _, _) :-
	nonvar(Goal),
	\+ callable(Goal),
	throw(type_error(callable, Goal)).

'$lgt_tr_body'(threaded_exit(Goal, Tag), MTGoal, '$lgt_debugger.goal'(threaded_exit(Goal, Tag), MDGoal, ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, Sender, This, Self, _, _, _, ExCtx, _, _),
	'$lgt_tr_body'(Goal, TGoal, DGoal, Ctx),
	(	'$lgt_pp_object_'(_, Prefix, _, _, _, _, _, _, _, _, _) ->
		MTGoal = '$lgt_threaded_exit_tagged'(Prefix, TGoal, Sender, This, Self, Tag),
		MDGoal = '$lgt_threaded_exit_tagged'(Prefix, DGoal, Sender, This, Self, Tag)
	;	% compiling a category
		MTGoal = '$lgt_threaded_exit_tagged'(TGoal, Sender, This, Self, Tag),
		MDGoal = '$lgt_threaded_exit_tagged'(DGoal, Sender, This, Self, Tag)
	),
	'$lgt_exec_ctx'(ExCtx, Sender, This, Self, _, _).


'$lgt_tr_body'(threaded_exit(_), _, _, _) :-
	\+ '$lgt_pp_threaded_',
	'$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, _, _),
	throw(resource_error(threads)).

'$lgt_tr_body'(threaded_exit(Goal), _, _, _) :-
	nonvar(Goal),
	\+ callable(Goal),
	throw(type_error(callable, Goal)).

'$lgt_tr_body'(threaded_exit(Goal), MTGoal, '$lgt_debugger.goal'(threaded_exit(Goal), MDGoal, ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, Sender, This, Self, _, _, _, ExCtx, _, _),
	'$lgt_tr_body'(Goal, TGoal, DGoal, Ctx),
	(	'$lgt_pp_object_'(_, Prefix, _, _, _, _, _, _, _, _, _) ->
		MTGoal = '$lgt_threaded_exit'(Prefix, TGoal, Sender, This, Self),
		MDGoal = '$lgt_threaded_exit'(Prefix, DGoal, Sender, This, Self)
	;	% compiling a category
		MTGoal = '$lgt_threaded_exit'(TGoal, Sender, This, Self),
		MDGoal = '$lgt_threaded_exit'(DGoal, Sender, This, Self)
	),
	'$lgt_exec_ctx'(ExCtx, Sender, This, Self, _, _).


'$lgt_tr_body'(threaded_peek(_, _), _, _, _) :-
	\+ '$lgt_pp_threaded_',
	'$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, _, _),
	throw(resource_error(threads)).

'$lgt_tr_body'(threaded_peek(Goal, _), _, _, _) :-
	nonvar(Goal),
	\+ callable(Goal),
	throw(type_error(callable, Goal)).

'$lgt_tr_body'(threaded_peek(Goal, Tag), MTGoal, '$lgt_debugger.goal'(threaded_peek(Goal, Tag), MDGoal, ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, Sender, This, Self, _, _, _, ExCtx, _, _),
	'$lgt_tr_body'(Goal, TGoal, DGoal, Ctx),
	(	'$lgt_pp_object_'(_, Prefix, _, _, _, _, _, _, _, _, _) ->
		MTGoal = '$lgt_threaded_peek_tagged'(Prefix, TGoal, Sender, This, Self, Tag),
		MDGoal = '$lgt_threaded_peek_tagged'(Prefix, DGoal, Sender, This, Self, Tag)
	;	% compiling a category
		MTGoal = '$lgt_threaded_peek_tagged'(TGoal, Sender, This, Self, Tag),
		MDGoal = '$lgt_threaded_peek_tagged'(DGoal, Sender, This, Self, Tag)
	),
	'$lgt_exec_ctx'(ExCtx, _, This, Self, _, _).


'$lgt_tr_body'(threaded_peek(_), _, _, _) :-
	\+ '$lgt_pp_threaded_',
	'$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, _, _),
	throw(resource_error(threads)).

'$lgt_tr_body'(threaded_peek(Goal), _, _, _) :-
	nonvar(Goal),
	\+ callable(Goal),
	throw(type_error(callable, Goal)).

'$lgt_tr_body'(threaded_peek(Goal), MTGoal, '$lgt_debugger.goal'(threaded_peek(Goal), MDGoal, ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx'(Ctx, _, Sender, This, Self, _, _, _, ExCtx, _, _),
	'$lgt_tr_body'(Goal, TGoal, DGoal, Ctx),
	(	'$lgt_pp_object_'(_, Prefix, _, _, _, _, _, _, _, _, _) ->
		MTGoal = '$lgt_threaded_peek'(Prefix, TGoal, Sender, This, Self),
		MDGoal = '$lgt_threaded_peek'(Prefix, DGoal, Sender, This, Self)
	;	% compiling a category
		MTGoal = '$lgt_threaded_peek'(TGoal, Sender, This, Self),
		MDGoal = '$lgt_threaded_peek'(DGoal, Sender, This, Self)
	),
	'$lgt_exec_ctx'(ExCtx, _, This, Self, _, _).


'$lgt_tr_body'(threaded_wait(_), _, _, _) :-
	\+ '$lgt_pp_threaded_',
	'$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, _, _),
	throw(resource_error(threads)).

'$lgt_tr_body'(threaded_wait(Msg), MTPred, '$lgt_debugger.goal'(threaded_wait(Msg), MTPred, ExCtx), Ctx) :-
	!,
	'$lgt_pp_entity'(Type, _, Prefix, _, _),
	'$lgt_comp_ctx'(Ctx, Head, _, _, _, Prefix, _, _, ExCtx, _, _),
	(	'$lgt_pp_synchronized_'(Head, Mutex) ->
		(	Type == object ->
			MTPred = '$lgt_threaded_wait_synch'(Mutex, Msg, Prefix)
		;	% we're compiling a category predicate
			'$lgt_comp_ctx_this'(Ctx, This),
			'$lgt_exec_ctx_this'(ExCtx, This),
			MTPred = '$lgt_threaded_wait_synch_ctg'(Mutex, Msg, This)
		)
	;	(	Type == object ->
			MTPred = '$lgt_threaded_wait'(Msg, Prefix)
		;	% we're compiling a category predicate
			'$lgt_comp_ctx_this'(Ctx, This),
			'$lgt_exec_ctx_this'(ExCtx, This),
			MTPred = '$lgt_threaded_wait_ctg'(Msg, This)
		)
	).


'$lgt_tr_body'(threaded_notify(_), _, _, _) :-
	\+ '$lgt_pp_threaded_',
	'$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, _, _),
	throw(resource_error(threads)).

'$lgt_tr_body'(threaded_notify(Msg), MTPred, '$lgt_debugger.goal'(threaded_notify(Msg), MTPred, ExCtx), Ctx) :-
	!,
	'$lgt_pp_entity'(Type, _, Prefix, _, _),
	'$lgt_comp_ctx'(Ctx, _, _, _, _, Prefix, _, _, ExCtx, _, _),
	(	Type == object ->
		MTPred = '$lgt_threaded_notify'(Msg, Prefix)
	;	% we're compiling a category predicate
		'$lgt_comp_ctx_this'(Ctx, This),
		'$lgt_exec_ctx_this'(ExCtx, This),
		MTPred = '$lgt_threaded_notify_ctg'(Msg, This)
	).


% message sending

'$lgt_tr_body'(Obj::Pred, TPred, '$lgt_debugger.goal'(Obj::Pred, TPred, ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx_this'(Ctx, This),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_exec_ctx_this'(ExCtx, This),
	'$lgt_tr_msg'(Pred, Obj, TPred, This).

'$lgt_tr_body'(::Pred, TPred, '$lgt_debugger.goal'(::Pred, TPred, ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx_this'(Ctx, This),
	'$lgt_comp_ctx_self'(Ctx, Self),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_exec_ctx'(ExCtx, _, This, Self, _, _),
	'$lgt_tr_self_msg'(Pred, TPred, This, Self).

'$lgt_tr_body'(^^Pred, TPred, '$lgt_debugger.goal'(^^Pred, TPred, ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_tr_super_call'(Pred, TPred, Ctx).


% context-switching

'$lgt_tr_body'(Obj<<Pred, TPred, '$lgt_debugger.goal'(Obj<<Pred, TPred, ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx_this'(Ctx, This),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_exec_ctx_this'(ExCtx, This),
	'$lgt_tr_ctx_call'(Obj, Pred, TPred, This).


% calling category predicates directly

'$lgt_tr_body'(':'(Pred), TPred, '$lgt_debugger.goal'(':'(Pred), TPred, ExCtx), Ctx) :-
	'$lgt_comp_ctx_mode'(Ctx, runtime),
	'$lgt_comp_ctx_this'(Ctx, This),
	'$lgt_current_object_'(This, _, Dcl, _, _, IDcl, _, _, _, _, _),
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	(	\+ '$lgt_instantiates_class_'(_, _, _),
	 	\+ '$lgt_specializes_class_'(_, _, _) ->
		TPred = '$lgt_ctg_call'(Dcl, Pred, ExCtx)
	;	TPred = '$lgt_ctg_call'(IDcl, Pred, ExCtx)
	).

'$lgt_tr_body'(':'(Pred), TPred, '$lgt_debugger.goal'(':'(Pred), TPred, ExCtx), Ctx) :-
	var(Pred),
	'$lgt_pp_object_'(_, _, Dcl, _, _, IDcl, _, _, _, _, _),
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	(	\+ '$lgt_pp_instantiated_class_'(_, _, _, _, _, _, _, _, _, _),
	 	\+ '$lgt_pp_specialized_class_'(_, _, _, _, _, _, _, _, _, _) ->
		TPred = '$lgt_ctg_call'(Dcl, Pred, ExCtx)
	;	TPred = '$lgt_ctg_call'(IDcl, Pred, ExCtx)
	).

'$lgt_tr_body'(':'(Pred), _, _, _) :-
	\+ callable(Pred),
	throw(type_error(callable, Pred)).

'$lgt_tr_body'(':'(Pred), _, _, _) :-
	\+ '$lgt_pp_imported_category_'(_, _, _, _, _),
	throw(existence_error(procedure, Pred)).

'$lgt_tr_body'(':'(Alias), TPred, '$lgt_debugger.goal'(':'(Alias), TPred, ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	(	% ensure that all imported categories are "static binding" entities
		forall('$lgt_pp_imported_category_'(Ctg, _, _, _, _), '$lgt_static_binding_entity_'(Ctg)),
		% find the first category in left-to-right import order that defines the predicate
		'$lgt_pp_imported_category_'(Ctg, _, _, _, _),
		(	'$lgt_pp_predicate_alias_'(Ctg, Pred, Alias) ->
			true
		;	Pred = Alias
		),
		'$lgt_ctg_static_binding_cache'(Ctg, Pred, ExCtx, TPred) ->
		true
	;	% must resort to dynamic binding
		'$lgt_pp_object_'(_, _, Dcl, _, _, IDcl, _, _, _, _, _),
		(	\+ '$lgt_pp_instantiated_class_'(_, _, _, _, _, _, _, _, _, _),
		 	\+ '$lgt_pp_specialized_class_'(_, _, _, _, _, _, _, _, _, _) ->
			TPred = '$lgt_ctg_call_'(Dcl, Alias, ExCtx)
		;	TPred = '$lgt_ctg_call_'(IDcl, Alias, ExCtx)
		)
	).


% calling explicitly qualified module predicates

'$lgt_tr_body'(':'(Module, Pred), TPred, DPred, Ctx) :-
	'$lgt_pl_built_in'(':'(_, _)),						% back-end Prolog compiler supports modules
	!,
	(	'$lgt_pp_module_'(_) ->
		% we're compiling a module as an object; assume referenced modules are also compiled as objects
		'$lgt_tr_body'(Module::Pred, TPred, DPred, Ctx)
	;	catch('$lgt_predicate_property'(':'(Module, Pred), meta_predicate(OriginalMeta)), _, fail) ->
		% we're compiling a call to a module meta-predicate:
		'$lgt_term_template'(Pred, OverridingMeta),
		(	'$lgt_pp_meta_predicate_'(':'(Module, OverridingMeta)) ->
			% we're overriding the original meta-predicate template:
			Meta = OverridingMeta
		;	Meta = OriginalMeta
		),
		Pred =.. [Functor| Args],
		Meta =.. [Functor| MArgs],
		(	'$lgt_member'(MArg, MArgs), integer(MArg), MArg =\= 0 ->
			% module meta-predicates that take closures are not supported:
			throw(domain_error(closure, Meta))
		;	'$lgt_member'(MArg, MArgs), MArg == (':') ->
			% the meta-argument specifier ':' is ambiguous:
			throw(domain_error(meta_argument_specifier, Meta))
		;	'$lgt_tr_module_meta_predicate_directives_args'(MArgs, CMArgs),
			'$lgt_tr_module_meta_args'(Args, CMArgs, Ctx, TArgs, DArgs),
			TPred =.. [Functor| TArgs],
			DPred =.. [Functor| DArgs]
		)
	;	% we're compiling a call to a module predicate
		'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
		TPred = ':'(Module, Pred),
		DPred = '$lgt_debugger.goal'(':'(Module, Pred), ':'(Module, Pred), ExCtx)
	).


% "reflection" built-in predicates

'$lgt_tr_body'(current_predicate(Term), TPred, DPred, Ctx) :-
	nonvar(Term),
	Term = ':'(Module, Pred),
	'$lgt_pl_built_in'(':'(_, _)),						% back-end Prolog compiler supports modules
	!,
	(	'$lgt_pp_module_'(_) ->
		% we're compiling a module as an object; assume referenced modules are also compiled as objects
		'$lgt_tr_body'(Module::current_predicate(Pred), TPred, DPred, Ctx)
	;	% we're using modules together with objects
		'$lgt_tr_body'(':'(Module, current_predicate(Pred)), TPred, DPred, Ctx)
	).

'$lgt_tr_body'(current_predicate(Pred), '$lgt_current_predicate'(This, Pred, This, p(_)), '$lgt_debugger.goal'(current_predicate(Pred), '$lgt_current_predicate'(This, Pred, This, p(_)), ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx_this'(Ctx, This),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_exec_ctx_this'(ExCtx, This).

'$lgt_tr_body'(predicate_property(Term, Prop), TPred, DPred, Ctx) :-
	nonvar(Term),
	Term = ':'(Module, Pred),
	'$lgt_pl_built_in'(':'(_, _)),						% back-end Prolog compiler supports modules
	!,
	(	'$lgt_pp_module_'(_) ->
		% we're compiling a module as an object; assume referenced modules are also compiled as objects
		'$lgt_tr_body'(Module::predicate_property(Pred, Prop), TPred, DPred, Ctx)
	;	% we're using modules together with objects
		'$lgt_tr_body'(':'(Module, predicate_property(Pred, Prop)), TPred, DPred, Ctx)
	).

'$lgt_tr_body'(predicate_property(Pred, Prop), '$lgt_predicate_property'(This, Pred, Prop, This, p(_)), '$lgt_debugger.goal'(predicate_property(Pred, Prop), '$lgt_predicate_property'(This, Pred, Prop, This, p(_)), ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx_this'(Ctx, This),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_exec_ctx_this'(ExCtx, This).


% database handling built-in predicates

'$lgt_tr_body'(abolish(Term), _, _, Ctx) :-
	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
	'$lgt_check_dynamic_directive'(Term),
	fail.

'$lgt_tr_body'(abolish(Term), TCond, DCond, Ctx) :-
	nonvar(Term),
	Term = ':'(Module, Pred),
	'$lgt_pl_built_in'(':'(_, _)),						% back-end Prolog compiler supports modules
	!,
	(	'$lgt_pp_module_'(_) ->
		% we're compiling a module as an object; assume referenced modules are also compiled as objects
		'$lgt_tr_body'(Module::abolish(Pred), TCond, DCond, Ctx)
	;	% we're using modules together with objects
		'$lgt_tr_body'(':'(Module, abolish(Pred)), TCond, DCond, Ctx)
	).

'$lgt_tr_body'(abolish(Pred), TCond, DCond, Ctx) :-
	!,
	'$lgt_comp_ctx_this'(Ctx, This),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_exec_ctx_this'(ExCtx, This),
	(	'$lgt_runtime_db_pred_ind_chk'(Pred) ->
		TCond = '$lgt_abolish'(This, Pred, This, p(_))
	;	'$lgt_must_be'(predicate_indicator, Pred),
		TCond = '$lgt_abolish_chk'(This, Pred, This, p(_))
	),
	DCond = '$lgt_debugger.goal'(abolish(Pred), TCond, ExCtx).

'$lgt_tr_body'(assert(Term), _, _, Ctx) :-
	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
	'$lgt_check_dynamic_directive'(Term),
	fail.

'$lgt_tr_body'(assert(Term), TCond, DCond, Ctx) :-
	!,
	(	'$lgt_pp_non_portable_call_'(assert, 1) ->
		true
	;	assertz('$lgt_pp_non_portable_call_'(assert, 1))
	),
	'$lgt_tr_body'(assertz(Term), TCond, DCond, Ctx).

'$lgt_tr_body'(asserta(Term), _, _, Ctx) :-
	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
	'$lgt_check_dynamic_directive'(Term),
	fail.

'$lgt_tr_body'(asserta(Term), TCond, DCond, Ctx) :-
	nonvar(Term),
	Term = ':'(Module, Pred),
	'$lgt_pl_built_in'(':'(_, _)),						% back-end Prolog compiler supports modules
	!,
	(	'$lgt_pp_module_'(_) ->
		% we're compiling a module as an object; assume referenced modules are also compiled as objects
		'$lgt_tr_body'(Module::asserta(Pred), TCond, DCond, Ctx)
	;	% we're using modules together with objects
		'$lgt_tr_body'(':'(Module, asserta(Pred)), TCond, DCond, Ctx)
	).

'$lgt_tr_body'(asserta(Pred), TCond, DCond, Ctx) :-
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	(	'$lgt_optimizable_local_db_call'(Pred, TPred) ->
		TCond = asserta(TPred)
	;	'$lgt_comp_ctx_this'(Ctx, This),
		'$lgt_exec_ctx_this'(ExCtx, This),
		(	'$lgt_runtime_db_clause_chk'(Pred) ->
			TCond = '$lgt_asserta'(This, Pred, This, p(_), p)
		;	'$lgt_compiler_db_clause_chk'(Pred),
			(	Pred = (Head :- Body) ->
				(	Body == true ->
					TCond = '$lgt_asserta_fact_chk'(This, Head, This, p(_), p)
				;	TCond = '$lgt_asserta_rule_chk'(This, Pred, This, p(_), p)
				)
			;	TCond = '$lgt_asserta_fact_chk'(This, Pred, This, p(_), p)
			)
		)
	),
	DCond = '$lgt_debugger.goal'(asserta(Pred), TCond, ExCtx).

'$lgt_tr_body'(assertz(Term), _, _, Ctx) :-
	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
	'$lgt_check_dynamic_directive'(Term),
	fail.

'$lgt_tr_body'(assertz(Term), TCond, DCond, Ctx) :-
	nonvar(Term),
	Term = ':'(Module, Pred),
	'$lgt_pl_built_in'(':'(_, _)),						% back-end Prolog compiler supports modules
	!,
	(	'$lgt_pp_module_'(_) ->
		% we're compiling a module as an object; assume referenced modules are also compiled as objects
		'$lgt_tr_body'(Module::assertz(Pred), TCond, DCond, Ctx)
	;	% we're using modules together with objects
		'$lgt_tr_body'(':'(Module, assertz(Pred)), TCond, DCond, Ctx)
	).

'$lgt_tr_body'(assertz(Pred), TCond, DCond, Ctx) :-
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	(	'$lgt_optimizable_local_db_call'(Pred, TPred) ->
		TCond = assertz(TPred)
	;	'$lgt_comp_ctx_this'(Ctx, This),
		'$lgt_exec_ctx_this'(ExCtx, This),
		(	'$lgt_runtime_db_clause_chk'(Pred) ->
			TCond = '$lgt_assertz'(This, Pred, This, p(_), p)
		;	'$lgt_compiler_db_clause_chk'(Pred),
			(	Pred = (Head :- Body) ->
				(	Body == true ->
					TCond = '$lgt_assertz_fact_chk'(This, Head, This, p(_), p)
				;	TCond = '$lgt_assertz_rule_chk'(This, Pred, This, p(_), p)
				)
			;	TCond = '$lgt_assertz_fact_chk'(This, Pred, This, p(_), p)
			)
		)
	),
	DCond = '$lgt_debugger.goal'(assertz(Pred), TCond, ExCtx).

'$lgt_tr_body'(clause(Term, _), _, _, Ctx) :-
	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
	'$lgt_check_dynamic_directive'(Term),
	fail.

'$lgt_tr_body'(clause(Term, Body), TCond, DCond, Ctx) :-
	nonvar(Term),
	Term = ':'(Module, Head),
	'$lgt_pl_built_in'(':'(_, _)),						% back-end Prolog compiler supports modules
	!,
	(	'$lgt_pp_module_'(_) ->
		% we're compiling a module as an object; assume referenced modules are also compiled as objects
		'$lgt_tr_body'(Module::clause(Head, Body), TCond, DCond, Ctx)
	;	% we're using modules together with objects
		'$lgt_tr_body'(':'(Module, clause(Head, Body)), TCond, DCond, Ctx)
	).

'$lgt_tr_body'(clause(Head, Body), TCond, DCond, Ctx) :-
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	(	'$lgt_optimizable_local_db_call'(Head, THead) ->
		TCond = (clause(THead, TBody), (TBody = ('$lgt_nop'(Body), _) -> true; TBody = Body))
	;	'$lgt_comp_ctx_this'(Ctx, This),
		'$lgt_exec_ctx_this'(ExCtx, This),
		(	'$lgt_runtime_db_clause_chk'((Head :- Body)) ->
			TCond = '$lgt_clause'(This, Head, Body, This, p(_))
		;	'$lgt_compiler_db_clause_chk'((Head :- Body)),
			TCond = '$lgt_clause_chk'(This, Head, Body, This, p(_))
		)
	),
	DCond = '$lgt_debugger.goal'(clause(Head, Body), TCond, ExCtx).

'$lgt_tr_body'(retract(Term), _, _, Ctx) :-
	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
	'$lgt_check_dynamic_directive'(Term),
	fail.

'$lgt_tr_body'(retract(Term), TCond, DCond, Ctx) :-
	nonvar(Term),
	Term = ':'(Module, Pred),
	'$lgt_pl_built_in'(':'(_, _)),						% back-end Prolog compiler supports modules
	!,
	(	'$lgt_pp_module_'(_) ->
		% we're compiling a module as an object; assume referenced modules are also compiled as objects
		'$lgt_tr_body'(Module::retract(Pred), TCond, DCond, Ctx)
	;	% we're using modules together with objects
		'$lgt_tr_body'(':'(Module, retract(Pred)), TCond, DCond, Ctx)
	).

'$lgt_tr_body'(retract(Pred), TCond, DCond, Ctx) :-
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	(	'$lgt_optimizable_local_db_call'(Pred, TPred) ->
		TCond = retract(TPred)
	;	'$lgt_comp_ctx_this'(Ctx, This),
		'$lgt_exec_ctx_this'(ExCtx, This),
		(	'$lgt_runtime_db_clause_chk'(Pred) ->
			TCond = '$lgt_retract'(This, Pred, This, p(_))
		;	'$lgt_compiler_db_clause_chk'(Pred),
			(	Pred = (Head :- Body) ->
				(	var(Body) ->
					'$lgt_retract_var_body_chk'(This, Pred, This, p(_))
				;	Body == true ->
					TCond = '$lgt_retract_fact_chk'(This, Head, This, p(_))
				;	TCond = '$lgt_retract_rule_chk'(This, Pred, This, p(_))
				)
			;	TCond = '$lgt_retract_fact_chk'(This, Pred, This, p(_))
			)
		)
	),
	DCond = '$lgt_debugger.goal'(retract(Pred), TCond, ExCtx).

'$lgt_tr_body'(retractall(Term), _, _, Ctx) :-
	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
	'$lgt_check_dynamic_directive'(Term),
	fail.

'$lgt_tr_body'(retractall(Term), TCond, DCond, Ctx) :-
	nonvar(Term),
	Term = ':'(Module, Pred),
	'$lgt_pl_built_in'(':'(_, _)),						% back-end Prolog compiler supports modules
	!,
	(	'$lgt_pp_module_'(_) ->
		% we're compiling a module as an object; assume referenced modules are also compiled as objects
		'$lgt_tr_body'(Module::retractall(Pred), TCond, DCond, Ctx)
	;	% we're using modules together with objects
		'$lgt_tr_body'(':'(Module, retractall(Pred)), TCond, DCond, Ctx)
	).

'$lgt_tr_body'(retractall(Pred), TCond, DCond, Ctx) :-
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	(	'$lgt_optimizable_local_db_call'(Pred, TPred) ->
		TCond = retractall(TPred)
	;	'$lgt_comp_ctx_this'(Ctx, This),
		'$lgt_exec_ctx_this'(ExCtx, This),
		(	'$lgt_runtime_db_clause_chk'(Pred) ->
			TCond = '$lgt_retractall'(This, Pred, This, p(_))
		;	'$lgt_compiler_db_clause_chk'(Pred),
			TCond = '$lgt_retractall_chk'(This, Pred, This, p(_))
		)
	),
	DCond = '$lgt_debugger.goal'(retractall(Pred), TCond, ExCtx).


% term and goal expansion predicates

'$lgt_tr_body'(expand_term(Term, Clause), '$lgt_expand_term'(This, Term, Clause, This, p(_)), '$lgt_debugger.goal'(expand_term(Term, Clause), '$lgt_expand_term'(This, Term, Clause, This, p(_)), ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx_this'(Ctx, This),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_exec_ctx_this'(ExCtx, This).

'$lgt_tr_body'(expand_goal(Goal, EGoal), '$lgt_expand_goal'(This, Goal, EGoal, This, p(_)), '$lgt_debugger.goal'(expand_goal(Goal, EGoal), '$lgt_expand_goal'(This, Goal, EGoal, This, p(_)), ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx_this'(Ctx, This),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_exec_ctx_this'(ExCtx, This).


% DCG predicates

'$lgt_tr_body'(phrase(GRBody, Input), '$lgt_phrase'(GRBody, Input, ExCtx), '$lgt_debugger.goal'(phrase(GRBody, Input), '$lgt_phrase'(GRBody, Input, ExCtx), ExCtx), Ctx) :-
	var(GRBody),
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx).

'$lgt_tr_body'(phrase(GRBody, Input), TPred, '$lgt_debugger.goal'(phrase(GRBody, Input), DPred, ExCtx), Ctx) :-
	!,
	'$lgt_dcg_body'(GRBody, S0, S, Pred),
	TPred = (Input = S0, [] = S, TPred0),
	DPred = (Input = S0, [] = S, DPred0),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_tr_body'(Pred, TPred0, DPred0, Ctx).

'$lgt_tr_body'(phrase(GRBody, Input, Rest), '$lgt_phrase'(GRBody, Input, Rest, ExCtx), '$lgt_debugger.goal'(phrase(GRBody, Input, Rest), '$lgt_phrase'(GRBody, Input, Rest, ExCtx), ExCtx), Ctx) :-
	var(GRBody),
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx).

'$lgt_tr_body'(phrase(GRBody, Input, Rest), TPred, '$lgt_debugger.goal'(phrase(GRBody, Input, Rest), DPred, ExCtx), Ctx) :-
	!,
	'$lgt_dcg_body'(GRBody, S0, S, Pred),
	TPred = (Input = S0, Rest = S, TPred0),
	DPred = (Input = S0, Rest = S, DPred0),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_tr_body'(Pred, TPred0, DPred0, Ctx).

'$lgt_tr_body'('$lgt_gr_pair'(Pred), Pred, Pred, _) :-
	!.


% inline methods (usually translated to a single unification with the corresponding context argument)

'$lgt_tr_body'(sender(Sender), true, '$lgt_debugger.goal'(sender(Temp), Sender=Temp, ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx_sender'(Ctx, Sender),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_exec_ctx'(ExCtx, Sender, _, _, _, _).

'$lgt_tr_body'(this(This), true, '$lgt_debugger.goal'(this(Temp), This=Temp, ExCtx), Ctx) :-
	!,
	(	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
		'$lgt_comp_ctx_this'(Ctx, This),
	 	'$lgt_exec_ctx_this'(ExCtx, This) ->	% check for mismatches between the argument of
		true									% this/1 and the parametric object identifier
	;	throw(domain_error(object_identifier, This))
	).

'$lgt_tr_body'(self(Self), true, '$lgt_debugger.goal'(self(Temp), Self=Temp, ExCtx), Ctx) :-
	!,
	'$lgt_comp_ctx_self'(Ctx, Self),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_exec_ctx'(ExCtx, _, _, Self, _, _).

'$lgt_tr_body'(parameter(_, _), _, _, _) :-
	'$lgt_pp_entity'(_, Entity, _, _, _),
	\+ compound(Entity),
	throw(type_error(parametric_entity, Entity)).

'$lgt_tr_body'(parameter(Arg, _), _, _, _) :-
	var(Arg),
	throw(instantiation_error).

'$lgt_tr_body'(parameter(Arg, Value), TPred, '$lgt_debugger.goal'(parameter(Arg, Temp), DPred, ExCtx), Ctx) :-
	'$lgt_pp_entity'(object, _, _, _, _),
	!,
	'$lgt_comp_ctx_this'(Ctx, This),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_exec_ctx_this'(ExCtx, This),
	functor(This, _, Arity),
	(	1 =< Arg, Arg =< Arity ->
		arg(Arg, This, Value),
		TPred = true,
		DPred = (Temp=Value)
	;	throw(domain_error(out_of_range, Arg))
	).

'$lgt_tr_body'(parameter(Arg, Value), TPred, '$lgt_debugger.goal'(parameter(Arg, Temp), DPred, ExCtx), Ctx) :-
	'$lgt_pp_entity'(category, Ctg, _, _, _),
	!,
	'$lgt_comp_ctx_this'(Ctx, This),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_exec_ctx_this'(ExCtx, This),
	functor(Ctg, _, Arity),
	(	1 =< Arg, Arg =< Arity ->
		TPred = '$lgt_ctg_parameter'(This, Ctg, Arg, Value),
		DPred = (TPred, Temp=Value)
	;	throw(domain_error(out_of_range, Arg))
	).



% term input predicates that need to be operator aware

'$lgt_tr_body'(read_term(Stream, Term, Options), '$lgt_iso_read_term'(Stream, Term, Options, Ops), '$lgt_debugger.goal'(read_term(Stream, Term, Options), '$lgt_iso_read_term'(Stream, Term, Options, Ops), ExCtx), Ctx) :-
	bagof(op(Pr, Spec, Op), Scope^'$lgt_pp_entity_op_'(Pr, Spec, Op, Scope), Ops),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	!.

'$lgt_tr_body'(read_term(Term, Options), '$lgt_iso_read_term'(Term, Options, Ops), '$lgt_debugger.goal'(read_term(Term, Options), '$lgt_iso_read_term'(Term, Options, Ops), ExCtx), Ctx) :-
	bagof(op(Pr, Spec, Op), Scope^'$lgt_pp_entity_op_'(Pr, Spec, Op, Scope), Ops),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	!.

'$lgt_tr_body'(read(Stream, Term), '$lgt_iso_read'(Stream, Term, Ops), '$lgt_debugger.goal'(read(Stream, Term), '$lgt_iso_read'(Stream, Term, Ops), ExCtx), Ctx) :-
	bagof(op(Pr, Spec, Op), Scope^'$lgt_pp_entity_op_'(Pr, Spec, Op, Scope), Ops),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	!.

'$lgt_tr_body'(read(Term), '$lgt_iso_read'(Term, Ops), '$lgt_debugger.goal'(read(Term), '$lgt_iso_read'(Term, Ops), ExCtx), Ctx) :-
	bagof(op(Pr, Spec, Op), Scope^'$lgt_pp_entity_op_'(Pr, Spec, Op, Scope), Ops),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	!.


% term output predicates that need to be operator aware

'$lgt_tr_body'(write_term(Stream, Term, Options), '$lgt_iso_write_term'(Stream, Term, Options, Ops), '$lgt_debugger.goal'(write_term(Stream, Term, Options), '$lgt_iso_write_term'(Stream, Term, Options, Ops), ExCtx), Ctx) :-
	('$lgt_member'(ignore_ops(Value), Options) -> Value \== true; true),
	bagof(op(Pr, Spec, Op), Scope^'$lgt_pp_entity_op_'(Pr, Spec, Op, Scope), Ops),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	!.

'$lgt_tr_body'(write_term(Term, Options), '$lgt_iso_write_term'(Term, Options, Ops), '$lgt_debugger.goal'(write_term(Term, Options), '$lgt_iso_write_term'(Term, Options, Ops), ExCtx), Ctx) :-
	('$lgt_member'(ignore_ops(Value), Options) -> Value \== true; true),
	bagof(op(Pr, Spec, Op), Scope^'$lgt_pp_entity_op_'(Pr, Spec, Op, Scope), Ops),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	!.

'$lgt_tr_body'(write(Stream, Term), '$lgt_iso_write'(Stream, Term, Ops), '$lgt_debugger.goal'(write(Stream, Term), '$lgt_iso_write'(Stream, Term, Ops), ExCtx), Ctx) :-
	bagof(op(Pr, Spec, Op), Scope^'$lgt_pp_entity_op_'(Pr, Spec, Op, Scope), Ops),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	!.

'$lgt_tr_body'(write(Term), '$lgt_iso_write'(Term, Ops), '$lgt_debugger.goal'(write(Term), '$lgt_iso_write'(Term, Ops), ExCtx), Ctx) :-
	bagof(op(Pr, Spec, Op), Scope^'$lgt_pp_entity_op_'(Pr, Spec, Op, Scope), Ops),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	!.

'$lgt_tr_body'(writeq(Stream, Term), '$lgt_iso_writeq'(Stream, Term, Ops), '$lgt_debugger.goal'(writeq(Stream, Term), '$lgt_iso_writeq'(Stream, Term, Ops), ExCtx), Ctx) :-
	bagof(op(Pr, Spec, Op), Scope^'$lgt_pp_entity_op_'(Pr, Spec, Op, Scope), Ops),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	!.

'$lgt_tr_body'(writeq(Term), '$lgt_iso_writeq'(Term, Ops), '$lgt_debugger.goal'(writeq(Term), '$lgt_iso_writeq'(Term, Ops), ExCtx), Ctx) :-
	bagof(op(Pr, Spec, Op), Scope^'$lgt_pp_entity_op_'(Pr, Spec, Op, Scope), Ops),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	!.


% predicates specified in uses/2 directives

'$lgt_tr_body'(Alias, TPred, '$lgt_debugger.goal'(Alias, TPred, ExCtx), Ctx) :-
	'$lgt_pp_uses_predicate_'(Obj, Pred, Alias),
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_tr_body'(Obj::Pred, TPred, _, Ctx).


% call to a meta-predicate from a user-defined meta-predicate:
% must check the number of arguments for shared closures

'$lgt_tr_body'(Pred, _, _, Ctx) :-
	'$lgt_comp_ctx_meta_vars'(Ctx, MetaVars),
	MetaVars \= [],
	(	'$lgt_term_template'(Pred, Meta),
		'$lgt_pp_meta_predicate_'(Meta) ->
		% user-defined meta-predicate
		true
	;	'$lgt_pl_meta_predicate'(Pred, Meta, predicate) ->
		% proprietary built-in meta-predicates declared in the config files
		true
	;	% non-declared proprietary built-in meta-predicates (fragile hack
	 	% due to lack of standardization of meta-predicate specifications)
		catch('$lgt_predicate_property'(Pred, meta_predicate(Meta)), _, fail) ->
		true
	;	% meta-predicates specified in use_module/2 directives
		'$lgt_pp_use_module_predicate_'(Module, Original, Pred),
		catch('$lgt_predicate_property'(':'(Module, Original), meta_predicate(Meta)), _, fail) ->
		true
	;	fail
	),
	Pred =.. [_| PredArgs],
	Meta =.. [_| MetaArgs],
	'$lgt_comp_ctx_head'(Ctx, Head),
	nonvar(Head),
	Head \= ':'(_, _),	% ignore multifile
	Head \= _::_,		% predicates
	'$lgt_term_template'(Head, HeadMeta),
	'$lgt_pp_meta_predicate_'(HeadMeta),
	Head =.. [_| HeadArgs],
	HeadMeta =.. [_| HeadMetaArgs],
	'$lgt_same_number_of_closure_extra_args'(PredArgs, MetaArgs, HeadArgs, HeadMetaArgs),
	fail.


% predicates specified in use_module/2 directives

'$lgt_tr_body'(Alias, ':'(Module, TPred), ':'(Module, DPred), Ctx) :-	% meta-predicates
	'$lgt_pp_use_module_predicate_'(Module, Pred, Alias),
	catch('$lgt_predicate_property'(':'(Module, Pred), meta_predicate(OriginalMeta)), _, fail),
	'$lgt_term_template'(Pred, OverridingMeta),
	(	'$lgt_pp_meta_predicate_'(':'(Module, OverridingMeta)) ->
		% we're overriding the original meta-predicate template:
		Meta = OverridingMeta
	;	Meta = OriginalMeta
	),
	Pred =.. [Functor| Args],
	Meta =.. [Functor| MArgs],
	(	'$lgt_member'(MArg, MArgs), integer(MArg), MArg =\= 0 ->
		% module meta-predicates that take closures are not supported:
		throw(domain_error(closure, Meta))
	;	'$lgt_member'(MArg, MArgs), MArg == (':') ->
		% the meta-argument specifier ':' is ambiguous:
		throw(domain_error(meta_argument_specifier, Meta))
	;	'$lgt_tr_module_meta_predicate_directives_args'(MArgs, CMArgs),
		'$lgt_tr_module_meta_args'(Args, CMArgs, Ctx, TArgs, DArgs),
		TPred =.. [Functor| TArgs],
		DPred =.. [Functor| DArgs]
	),
	!.

'$lgt_tr_body'(Alias, ':'(Module, Pred), ':'(Module, Pred), _) :-		% normal predicates
	'$lgt_pp_use_module_predicate_'(Module, Pred, Alias),
	!.


% annotations

'$lgt_tr_body'(Annotation, TAnnotation, DAnnotation, Ctx) :-
	'$lgt_pp_value_annotation_'(Annotation, Functor, Value, Pred),
	!,
	'$lgt_tr_body'(Pred, TPred, DPred, Ctx),
	'$lgt_pp_value_annotation_'(TAnnotation, Functor, Value, TPred),
	'$lgt_pp_value_annotation_'(DAnnotation, Functor, Value, DPred).

'$lgt_tr_body'(Annotation, TAnnotation, DAnnotation, Ctx) :-
	'$lgt_pp_goal_annotation_'(Annotation, Functor, Pred1, Pred2),
	!,
	'$lgt_tr_body'(Pred1, TPred1, DPred1, Ctx),
	'$lgt_tr_body'(Pred2, TPred2, DPred2, Ctx),
	'$lgt_pp_goal_annotation_'(TAnnotation, Functor, TPred1, TPred2),
	'$lgt_pp_goal_annotation_'(DAnnotation, Functor, DPred1, DPred2).


% arithmetic predicates (portability checks)

'$lgt_tr_body'(_ is Exp, _, _, Ctx) :-
	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
	'$lgt_check_non_portable_functions'(Exp),
	fail.
'$lgt_tr_body'(Exp1 =:= Exp2, _, _, Ctx) :-
	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
	'$lgt_check_non_portable_functions'(Exp1),
	'$lgt_check_non_portable_functions'(Exp2),
	fail.
'$lgt_tr_body'(Exp1 =\= Exp2, _, _, Ctx) :-
	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
	'$lgt_check_non_portable_functions'(Exp1),
	'$lgt_check_non_portable_functions'(Exp2),
	fail.
'$lgt_tr_body'(Exp1 < Exp2, _, _, Ctx) :-
	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
	'$lgt_check_non_portable_functions'(Exp1),
	'$lgt_check_non_portable_functions'(Exp2),
	fail.
'$lgt_tr_body'(Exp1 =< Exp2, _, _, Ctx) :-
	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
	'$lgt_check_non_portable_functions'(Exp1),
	'$lgt_check_non_portable_functions'(Exp2),
	fail.
'$lgt_tr_body'(Exp1 > Exp2, _, _, Ctx) :-
	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
	'$lgt_check_non_portable_functions'(Exp1),
	'$lgt_check_non_portable_functions'(Exp2),
	fail.
'$lgt_tr_body'(Exp1 >= Exp2, _, _, Ctx) :-
	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
	'$lgt_check_non_portable_functions'(Exp1),
	'$lgt_check_non_portable_functions'(Exp2),
	fail.


% remember non-portable Prolog built-in predicate calls

'$lgt_tr_body'(Pred, _, _, Ctx) :-
	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
	'$lgt_compiler_flag'(portability, warning),
	'$lgt_pl_built_in'(Pred),
	\+ '$lgt_lgt_built_in'(Pred),
	\+ '$lgt_iso_spec_pred'(Pred),
	functor(Pred, Functor, Arity),
	\+ '$lgt_pp_public_'(Functor, Arity),		% not a
	\+ '$lgt_pp_protected_'(Functor, Arity),	% redefined
	\+ '$lgt_pp_private_'(Functor, Arity),		% built-in
	\+ '$lgt_pp_non_portable_call_'(Functor, Arity),
	assertz('$lgt_pp_non_portable_call_'(Functor, Arity)),
	fail.


% blackboard predicates (requires a back-end Prolog compiler natively supporting these built-in predicates)

'$lgt_tr_body'(bb_put(Key, Term), TPred, DPred, Ctx) :-
	'$lgt_pl_built_in'(bb_put(_, _)),
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_comp_ctx_prefix'(Ctx, Prefix),
	(	atomic(Key) ->
		'$lgt_tr_bb_key'(Key, Prefix, TKey),
		TPred = '$lgt_call_built_in'(bb_put(Key, Term), bb_put(TKey, Term), ExCtx),
		DPred = '$lgt_debugger.goal'(bb_put(Key, Term), TPred, ExCtx)
	;	var(Key) ->
		% runtime key translation
		TPred = '$lgt_call_built_in'(bb_put(Key, Term), ('$lgt_tr_bb_key'(Key, Prefix, TKey, bb_put(Key, Term)), bb_put(TKey, Term)), ExCtx),
		DPred = '$lgt_debugger.goal'(bb_put(Key, Term), TPred, ExCtx)
	;	throw(type_error(atomic, Key))
	).

'$lgt_tr_body'(bb_get(Key, Term), TPred, DPred, Ctx) :-
	'$lgt_pl_built_in'(bb_get(_, _)),
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_comp_ctx_prefix'(Ctx, Prefix),
	(	atomic(Key) ->
		'$lgt_tr_bb_key'(Key, Prefix, TKey),
		TPred = '$lgt_call_built_in'(bb_get(Key, Term), bb_get(TKey, Term), ExCtx),
		DPred = '$lgt_debugger.goal'(bb_get(Key, Term), TPred, ExCtx)
	;	var(Key) ->
		% runtime key translation
		TPred = '$lgt_call_built_in'(bb_get(Key, Term), ('$lgt_tr_bb_key'(Key, Prefix, TKey, bb_get(Key, Term)), bb_get(TKey, Term)), ExCtx),
		DPred = '$lgt_debugger.goal'(bb_get(Key, Term), TPred, ExCtx)
	;	throw(type_error(atomic, Key))
	).

'$lgt_tr_body'(bb_delete(Key, Term), TPred, DPred, Ctx) :-
	'$lgt_pl_built_in'(bb_delete(_, _)),
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_comp_ctx_prefix'(Ctx, Prefix),
	(	atomic(Key) ->
		'$lgt_tr_bb_key'(Key, Prefix, TKey),
		TPred = '$lgt_call_built_in'(bb_delete(Key, Term), bb_delete(TKey, Term), ExCtx),
		DPred = '$lgt_debugger.goal'(bb_delete(Key, Term), TPred, ExCtx)
	;	var(Key) ->
		% runtime key translation
		TPred = '$lgt_call_built_in'(bb_delete(Key, Term), ('$lgt_tr_bb_key'(Key, Prefix, TKey, bb_delete(Key, Term)), bb_delete(TKey, Term)), ExCtx),
		DPred = '$lgt_debugger.goal'(bb_delete(Key, Term), TPred, ExCtx)
	;	throw(type_error(atomic, Key))
	).

'$lgt_tr_body'(bb_update(Key, Term, New), TPred, DPred, Ctx) :-
	'$lgt_pl_built_in'(bb_update(_, _, _)),
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_comp_ctx_prefix'(Ctx, Prefix),
	(	atomic(Key) ->
		'$lgt_tr_bb_key'(Key, Prefix, TKey),
		TPred = '$lgt_call_built_in'(bb_update(Key, Term, New), bb_update(TKey, Term, New), ExCtx),
		DPred = '$lgt_debugger.goal'(bb_update(Key, Term, New), TPred, ExCtx)
	;	var(Key) ->
		% runtime key translation
		TPred = '$lgt_call_built_in'(bb_update(Key, Term, New), ('$lgt_tr_bb_key'(Key, Prefix, TKey, bb_update(Key, Term, New)), bb_update(TKey, Term, New)), ExCtx),
		DPred = '$lgt_debugger.goal'(bb_update(Key, Term, New), TPred, ExCtx)
	;	throw(type_error(atomic, Key))
	).


% Prolog proprietary built-in meta-predicates (must be declared in the config files)

'$lgt_tr_body'(Pred, TPred, DPred, Ctx) :-
	'$lgt_pl_meta_predicate'(Pred, _, _),
	functor(Pred, Functor, Arity),
	(	'$lgt_comp_ctx_mode'(Ctx, runtime) ->
		true
	;	\+ '$lgt_pp_defines_predicate_'(Functor, Arity),
		\+ '$lgt_pp_public_'(Functor, Arity),			% not a redefined
		\+ '$lgt_pp_protected_'(Functor, Arity),		% built-in... unless
		\+ '$lgt_pp_private_'(Functor, Arity),			% the redefinition is
		\+ '$lgt_pp_redefined_built_in_'(Pred, _, _)	% yet to be compiled
	),
	!,
	(	'$lgt_pl_meta_predicate'(Pred, Meta, Type),		% we can have multiple templates for
		Pred =.. [_| Args],								% the same meta-predicate; look for
		Meta =.. [_| MArgs],							% one that matches the predicate call
		'$lgt_tr_meta_args'(Args, MArgs, Ctx, TArgs, DArgs) ->
		TGoal =.. [Functor| TArgs],
		'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
		(	'$lgt_comp_ctx_mode'(Ctx, runtime) ->
			TPred = TGoal,
			(	Type == control_construct ->
				DPred =.. [Functor| DArgs]
			;	DPred = '$lgt_debugger.goal'(Pred, TPred, ExCtx)
			)
		;	TPred = '$lgt_call_built_in'(Pred, TGoal, ExCtx),
			(	Type == control_construct ->
				DGoal =.. [Functor| DArgs],
				DPred = '$lgt_call_built_in'(Pred, DGoal, ExCtx)
			;	DPred = '$lgt_debugger.goal'(Pred, TPred, ExCtx)
			)
		)
	;	% none of the templates is usable, report as an error the first one:
		'$lgt_pl_meta_predicate'(Pred, Meta, _),
		throw(domain_error(meta_predicate_template, Meta))
	).


% Logtalk and Prolog built-in predicates

'$lgt_tr_body'(Pred, TPred, DPred, Ctx) :-
	'$lgt_built_in'(Pred),
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	(	'$lgt_comp_ctx_mode'(Ctx, runtime) ->
		TPred = Pred
	;	functor(Pred, Functor, Arity),
		\+ '$lgt_pp_defines_predicate_'(Functor, Arity),
		\+ '$lgt_pp_public_'(Functor, Arity),			% not a redefined
		\+ '$lgt_pp_protected_'(Functor, Arity),		% built-in... unless
		\+ '$lgt_pp_private_'(Functor, Arity),			% the redefinition is
		\+ '$lgt_pp_redefined_built_in_'(Pred, _, _),   % yet to be compiled
		TPred = '$lgt_call_built_in'(Pred, Pred, ExCtx)
	),
	DPred = '$lgt_debugger.goal'(Pred, TPred, ExCtx),
	!.


% invalid goal

'$lgt_tr_body'(Pred, _, _, _) :-
	\+ callable(Pred),
	throw(type_error(callable, Pred)).


% goal is a call to a dynamic predicate within a category

'$lgt_tr_body'(Pred, TPred, '$lgt_debugger.goal'(Pred, TPred, ExCtx), Ctx) :-
	'$lgt_pp_category_'(_, _, _, _, _, _),		% we're compiling a category
	functor(Pred, Functor, Arity),
	'$lgt_pp_dynamic_'(Functor, Arity),			% which declares the predicate dynamic
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	TPred = '$lgt_call_this'(Pred, ExCtx).


% goal is a call to a user-defined predicate in sender (i.e. a meta-argument)

'$lgt_tr_body'(Pred, TPred, '$lgt_debugger.goal'(Pred, TPred, ExCtx), Ctx) :-
	'$lgt_comp_ctx_meta_vars'(Ctx, MetaVars),
	'$lgt_member_var'(Pred, MetaVars),
	!,
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	'$lgt_comp_ctx_sender'(Ctx, Sender),
	'$lgt_comp_ctx_self'(Ctx, Self),
	'$lgt_exec_ctx'(ExCtx, Sender, _, Self, _, _),
	TPred = '$lgt_metacall_this'(Pred, Sender, Sender, Self).


% goal is a call to a local, user-defined coinductive predicate

'$lgt_tr_body'(Pred, TPred, DPred, Ctx) :-
	'$lgt_pp_coinductive_'(Pred, CoinductivePred),
	!,
	'$lgt_tr_body'(CoinductivePred, TPred, DPred, Ctx).


% goal is a call to a local, user-defined predicate

'$lgt_tr_body'(Pred, TPred, '$lgt_debugger.goal'(Pred, TPred, ExCtx), Ctx) :-
	functor(Pred, Functor, Arity),
	'$lgt_comp_ctx'(Ctx, Head, _, _, _, Prefix, _, _, ExCtx, _, _),
	'$lgt_construct_predicate_indicator'(Prefix, Functor/Arity, TFunctor/TArity),
	(	'$lgt_pp_synchronized_'(Pred, _),
		\+ functor(Head, Functor, Arity) ->			% not a recursive call
		atom_concat(TFunctor, '__sync', STFunctor)
	;	STFunctor = TFunctor
	),
	functor(TPred, STFunctor, TArity),
	'$lgt_unify_head_thead_args'(Arity, Pred, TPred),
	arg(TArity, TPred, ExCtx),
	(	'$lgt_pp_calls_predicate_'(Functor, Arity, _, _) ->
		true
	;	assertz('$lgt_pp_calls_predicate_'(Functor, Arity, STFunctor, TArity))
	).



% '$lgt_gen_aux_predicate_functor'(+atom, -atom)
%
% generates a new functor for an auxiliary predicate
% based on a base atom and an entity global counter

'$lgt_gen_aux_predicate_functor'(Base, Functor) :-
	retract('$lgt_pp_aux_predicate_counter_'(Old)),
	New is Old + 1,
	asserta('$lgt_pp_aux_predicate_counter_'(New)),
	number_codes(New, NewCodes),
	atom_codes(NewAtom, NewCodes),
	atom_concat(Base, NewAtom, Functor).



% '$lgt_tr_bb_key'(@term, +atom, -atom)
%
% compile-time translation of a black-board key

'$lgt_tr_bb_key'(Key, Prefix, TKey) :-
	(	atom(Key) ->
		atom_concat(Prefix, Key, TKey)
	;	integer(Key) ->
		number_codes(Key, KeyCodes),
		atom_codes(AtomKey, KeyCodes),
		atom_concat(Prefix, AtomKey, TKey)
	;	throw(type_error(atomic, Key))
	).



% '$lgt_tr_bb_key'(@term, +atom, -atom, @callable)
%
% translates a black-board key

'$lgt_tr_bb_key'(Key, Prefix, TKey, Goal) :-
	(	'$lgt_tr_bb_key'(Key, Prefix, TKey) ->
		true
	;	var(Key) ->
		throw(error(instantiation_error, Goal))
	;	throw(error(type_error(atomic, Key), Goal))
	).



% '$lgt_tr_threaded_call'(+callable, -callable)
%
% translates the argument of a call to the built-in predicate threaded/1

'$lgt_tr_threaded_call'((TGoal; TGoals), '$lgt_threaded_or'(Queue, MTGoals, Results)) :-
	!,
	'$lgt_tr_threaded_or_call'((TGoal; TGoals), Queue, MTGoals, Results).

'$lgt_tr_threaded_call'((TGoal, TGoals), '$lgt_threaded_and'(Queue, MTGoals, Results)) :-
	!,
	'$lgt_tr_threaded_and_call'((TGoal, TGoals), Queue, MTGoals, Results).

'$lgt_tr_threaded_call'(TGoal, (TGoal -> true; fail)).


'$lgt_tr_threaded_or_call'((TGoal; TGoals), Queue, (MTGoal, MTGoals), [Result| Results]) :-
	!,
	'$lgt_tr_threaded_goal'(TGoal, Queue, MTGoal, Result),
	'$lgt_tr_threaded_or_call'(TGoals, Queue, MTGoals, Results).

'$lgt_tr_threaded_or_call'(TGoal, Queue, MTGoal, [Result]) :-
	'$lgt_tr_threaded_goal'(TGoal, Queue, MTGoal, Result).


'$lgt_tr_threaded_and_call'((TGoal, TGoals), Queue, (MTGoal, MTGoals), [Result| Results]) :-
	!,
	'$lgt_tr_threaded_goal'(TGoal, Queue, MTGoal, Result),
	'$lgt_tr_threaded_and_call'(TGoals, Queue, MTGoals, Results).

'$lgt_tr_threaded_and_call'(TGoal, Queue, MTGoal, [Result]) :-
	'$lgt_tr_threaded_goal'(TGoal, Queue, MTGoal, Result).

'$lgt_tr_threaded_goal'(TGoal, Queue, '$lgt_threaded_goal'(TGoal, TVars, Queue, Id), id(Id, TVars, _)).



% '$lgt_tr_meta_args'(@list, @list, +term, -list, -list)
%
% translates the meta-arguments contained in the list of arguments of a
% call to a meta-predicate (assumes Logtalk meta-predicate notation)

'$lgt_tr_meta_args'([], [], _, [], []).

'$lgt_tr_meta_args'([Arg| Args], [MArg| MArgs], Ctx, [TArg| TArgs], [DArg| DArgs]) :-
	'$lgt_tr_meta_arg'(MArg, Arg, Ctx, TArg0, DArg0),
	'$lgt_fix_predicate_calls'(TArg0, TArg, false),
	'$lgt_fix_predicate_calls'(DArg0, DArg, false),
	'$lgt_tr_meta_args'(Args, MArgs, Ctx, TArgs, DArgs).


'$lgt_tr_meta_arg'((*), Arg, _, Arg, Arg).

'$lgt_tr_meta_arg'((0), Arg, Ctx, TArg, DArg) :-
	'$lgt_tr_body'(Arg, TArg, DArg, Ctx).

'$lgt_tr_meta_arg'([0], [], _, [], []) :- !.
'$lgt_tr_meta_arg'([0], [Arg| Args], Ctx, [TArg| TArgs], [DArg| DArgs]) :-
	'$lgt_tr_meta_arg'((0), Arg, Ctx, TArg, DArg),
	'$lgt_tr_meta_arg'([0], Args, Ctx, TArgs, DArgs).

'$lgt_tr_meta_arg'((/), Arg, _, TArg, TArg) :-
	'$lgt_valid_predicate_indicator'(Arg, _, _),
	'$lgt_compile_predicate_indicators'(Arg, TArg).

'$lgt_tr_meta_arg'([/], [], _, [], []) :- !.
'$lgt_tr_meta_arg'([/], [Arg| Args], Ctx, [TArg| TArgs], [DArg| DArgs]) :-
	'$lgt_tr_meta_arg'((/), Arg, Ctx, TArg, DArg),
	'$lgt_tr_meta_arg'([/], Args, Ctx, TArgs, DArgs).



% '$lgt_tr_module_meta_args'(@list, @list, +term, -list, -list)
%
% translates the meta-arguments contained in the list of arguments of a call
% to a module meta-predicate (assumes Logtalk meta-predicate notation); due
% to the module meta-predicate semantics, the meta-arguments must be explicitly
% qualified as being called from the "user" module

'$lgt_tr_module_meta_args'([], [], _, [], []).

'$lgt_tr_module_meta_args'([Arg| Args], [MArg| MArgs], Ctx, [TArg| TArgs], [DArg| DArgs]) :-
	'$lgt_tr_module_meta_arg'(MArg, Arg, Ctx, TArg0, DArg0),
	'$lgt_fix_predicate_calls'(TArg0, TArg, false),
	'$lgt_fix_predicate_calls'(DArg0, DArg, false),
	'$lgt_tr_module_meta_args'(Args, MArgs, Ctx, TArgs, DArgs).


'$lgt_tr_module_meta_arg'((*), Arg, _, Arg, Arg).

'$lgt_tr_module_meta_arg'((0), Arg, Ctx, TArg, DArg) :-
	(	nonvar(Arg), functor(Arg, ':', 2) ->
		% explicit-qualified meta-argument
		TArg = Arg,
		DArg = Arg
	;	% non-qualified meta-argument
		'$lgt_tr_body'(Arg, TArg0, DArg0, Ctx),
		TArg = ':'(user, TArg0),
		DArg = ':'(user, DArg0)
	).

'$lgt_tr_module_meta_arg'([0], [], _, [], []) :- !.
'$lgt_tr_module_meta_arg'([0], [Arg| Args], Ctx, [TArg| TArgs], [DArg| DArgs]) :-
	'$lgt_tr_module_meta_arg'((0), Arg, Ctx, TArg, DArg),
	'$lgt_tr_module_meta_arg'([0], Args, Ctx, TArgs, DArgs).

'$lgt_tr_module_meta_arg'((/), Arg, _, TArg, DArg) :-
	(	nonvar(Arg), functor(Arg, ':', 2) ->
		% explicit-qualified meta-argument
		TArg = Arg,
		DArg = Arg
	;	% non-qualified meta-argument
		'$lgt_compile_predicate_indicators'(Arg, TArg0),
		TArg = ':'(user, TArg0),
		DArg = ':'(user, TArg0)
	).

'$lgt_tr_module_meta_arg'([/], [], _, [], []) :- !.
'$lgt_tr_module_meta_arg'([/], [Arg| Args], Ctx, [TArg| TArgs], [DArg| DArgs]) :-
	'$lgt_tr_module_meta_arg'((/), Arg, Ctx, TArg, DArg),
	'$lgt_tr_module_meta_arg'([/], Args, Ctx, TArgs, DArgs).



% '$lgt_same_meta_arg_extra_args'(@list(nonvar), @list(var), @var, +integer)
%
% checks that the number of additional arguments being appended to a closure
% in a call/N call matches the corresponding meta-predicate declaration
% (the relative ordering of the meta-vars is the same of the corresponding 
% meta-arguments; assumes Logtalk meta-predicate notation)

'$lgt_same_meta_arg_extra_args'([(*)| MetaArgs], MetaVars, Closure, ExtraArgs) :-
	!,
	'$lgt_same_meta_arg_extra_args'(MetaArgs, MetaVars, Closure, ExtraArgs).

'$lgt_same_meta_arg_extra_args'([(::)| MetaArgs], MetaVars, Closure, ExtraArgs) :-
	!,
	'$lgt_same_meta_arg_extra_args'(MetaArgs, MetaVars, Closure, ExtraArgs).

'$lgt_same_meta_arg_extra_args'([0| MetaArgs], MetaVars, Closure, ExtraArgs) :-
	!,
	'$lgt_same_meta_arg_extra_args'(MetaArgs, MetaVars, Closure, ExtraArgs).

'$lgt_same_meta_arg_extra_args'([MetaArg| _], [MetaVar| _], Closure, ExtraArgs) :-
	MetaVar == Closure,
	!,
	integer(MetaArg),
	MetaArg =:= ExtraArgs.

'$lgt_same_meta_arg_extra_args'([_| MetaArgs], [_| MetaVars], Closure, ExtraArgs) :-
	'$lgt_same_meta_arg_extra_args'(MetaArgs, MetaVars, Closure, ExtraArgs).



% '$lgt_same_number_of_closure_extra_args'(@list, @list, @list, @list)
%
% checks that the number of additional arguments being appended to a closure is kept
% when passing a closure from the clause head to a meta-predicate call in the body

'$lgt_same_number_of_closure_extra_args'([], _, _, _).

'$lgt_same_number_of_closure_extra_args'([PredArg| PredArgs], [PredMetaArg| PredMetaArgs], HeadArgs, HeadMetaArgs) :-
	(	var(PredArg),
		integer(PredMetaArg), PredMetaArg > 0,
		% argument is a closure
		'$lgt_shared_closure_arg'(PredArg, HeadArgs, HeadMetaArgs, HeadMetaArg) ->
		% shared closure argument
		(	PredMetaArg = HeadMetaArg ->
			% same number of closure extra args
			'$lgt_same_number_of_closure_extra_args'(PredArgs, PredMetaArgs, HeadArgs, HeadMetaArgs)
		;	throw(arity_mismatch(closure, PredMetaArg, HeadMetaArg))
		)
	;	'$lgt_same_number_of_closure_extra_args'(PredArgs, PredMetaArgs, HeadArgs, HeadMetaArgs)
	).


'$lgt_shared_closure_arg'(PredArg, [HeadArg| _], [HeadMetaArg| _], HeadMetaArg) :-
	PredArg == HeadArg.

'$lgt_shared_closure_arg'(PredArg, [_| HeadArgs], [_| HeadMetaArgs], HeadMetaArg) :-
	'$lgt_shared_closure_arg'(PredArg, HeadArgs, HeadMetaArgs, HeadMetaArg).



% '$lgt_check_dynamic_directive'(@term)
%
% checks for a dynamic/1 directive for a predicate that is an argument to the
% database built-in methods

'$lgt_check_dynamic_directive'(Term) :-						% runtime argument
	var(Term),
	!.

'$lgt_check_dynamic_directive'((':'(Module, Head) :- _)) :-	% module explicit qualification
	!,
	(	'$lgt_pp_module_'(Module) ->						% same module we're compiling
		'$lgt_check_dynamic_directive'(Head)
	;	true
	).

'$lgt_check_dynamic_directive'(':'(Module, Term)) :-		% module explicit qualification
	!,
	(	'$lgt_pp_module_'(Module) ->						% same module we're compiling
		'$lgt_check_dynamic_directive'(Term)
	;	true
	).

'$lgt_check_dynamic_directive'((Head:-_)) :-				% clause rule
	!,
	'$lgt_check_dynamic_directive'(Head).

'$lgt_check_dynamic_directive'(Term) :-						% predicate indicator
	'$lgt_valid_predicate_indicator'(Term, Functor, Arity),
	!,
	\+ '$lgt_pp_dynamic_'(Functor, Arity),					% dynamic directive not (yet) found
	\+ '$lgt_pp_missing_dynamic_directive_'(Functor, Arity),
	assertz('$lgt_pp_missing_dynamic_directive_'(Functor, Arity)).

'$lgt_check_dynamic_directive'(Head) :-						% clause fact
	nonvar(Head),
	functor(Head, Functor, Arity),
	\+ '$lgt_pp_dynamic_'(Functor, Arity),					% dynamic directive not (yet) found
	\+ '$lgt_pp_missing_dynamic_directive_'(Functor, Arity),
	assertz('$lgt_pp_missing_dynamic_directive_'(Functor, Arity)).



% '$lgt_check_discontiguous_directive'(@predicate_indicator)
%
% checks for a discontiguous/1 directive for a predicate

'$lgt_check_discontiguous_directive'(Functor, Arity) :-
	(	'$lgt_pp_discontiguous_'(Functor, Arity) ->
		true
	;	'$lgt_pp_missing_discontiguous_directive_'(Functor, Arity) ->
		true
	;	assertz('$lgt_pp_missing_discontiguous_directive_'(Functor, Arity))
	).



% '$lgt_optimizable_local_db_call'(@term, -callable)
%
% checks if a call to a database built-in method can be optimized by direct
% translation to a call to the corresponding Prolog built-in predicate

'$lgt_optimizable_local_db_call'(Pred, TPred) :-
	'$lgt_pp_entity'(object, _, Prefix, _, _),		% only for objects
	'$lgt_compiler_flag'(debug, off),				% not debugging
	(	Pred = (Head :- Body) ->					% only facts allowed
		Body == true
	;	Head = Pred
	),
	callable(Head),
	functor(Head, Functor, Arity),
	once('$lgt_pp_dynamic_'(Functor, Arity)),		% a dynamic directive must be present
	once((	'$lgt_pp_public_'(Functor, Arity)		% a scope directive must be present
		;	'$lgt_pp_protected_'(Functor, Arity)
		;	'$lgt_pp_private_'(Functor, Arity)
	)),
	'$lgt_construct_predicate_indicator'(Prefix, Functor/Arity, TFunctor/TArity),
	functor(TPred, TFunctor, TArity),
	'$lgt_unify_head_thead_args'(Arity, Head, TPred).



% '$lgt_runtime_db_clause_chk'(@term)
%
% true if the argument forces runtime validity check

'$lgt_runtime_db_clause_chk'(Pred) :-
	var(Pred),
	!.

'$lgt_runtime_db_clause_chk'((Head :- _)) :-
	var(Head),
	!.

'$lgt_runtime_db_clause_chk'((_ :- Body)) :-
	var(Body).



% '$lgt_compiler_db_clause_chk'(@nonvar)
%
% throws an error if the argument is invalid

'$lgt_compiler_db_clause_chk'((Head :- _)) :-
	\+ callable(Head),
	throw(type_error(callable, Head)).

'$lgt_compiler_db_clause_chk'((_ :- Body)) :-
	nonvar(Body),
	\+ callable(Body),
	throw(type_error(callable, Body)).

'$lgt_compiler_db_clause_chk'(Clause) :-
	\+ callable(Clause),
	throw(type_error(callable, Clause)).

'$lgt_compiler_db_clause_chk'(_).



% '$lgt_runtime_db_pred_ind_chk'(@term)
%
% true if the argument forces runtime validity check

'$lgt_runtime_db_pred_ind_chk'(Pred) :-
	var(Pred),
	!.

'$lgt_runtime_db_pred_ind_chk'(Functor/_) :-
	var(Functor),
	!.

'$lgt_runtime_db_pred_ind_chk'(_/Arity) :-
	var(Arity).



% '$lgt_check_non_portable_functions'(@term)
%
% checks an arithmetic expression for calls to non-standard Prolog functions

'$lgt_check_non_portable_functions'(Exp) :-
	var(Exp),
	!.

'$lgt_check_non_portable_functions'(Exp) :-
	number(Exp),
	!.

'$lgt_check_non_portable_functions'(Exp) :-
	'$lgt_iso_spec_function'(Exp),
	!,
	Exp =.. [_| Exps],
	'$lgt_check_non_portable_function_args'(Exps).

'$lgt_check_non_portable_functions'(Exp) :-
	functor(Exp, Functor, Arity),
	(	'$lgt_pp_non_portable_function_'(Functor, Arity) ->
		true
	;	'$lgt_compiler_flag'(portability, warning) ->
		assertz('$lgt_pp_non_portable_function_'(Functor, Arity))
	;	true
	),
	Exp =.. [_| Exps],
	'$lgt_check_non_portable_function_args'(Exps).


'$lgt_check_non_portable_function_args'([]).

'$lgt_check_non_portable_function_args'([Exp| Exps]) :-
	'$lgt_check_non_portable_functions'(Exp),
	'$lgt_check_non_portable_function_args'(Exps).



% '$lgt_tr_msg'(@term, @object_identifier, -nonvar, @object_identifier)
%
% translates the sending of a message to an object


% invalid object identifier
		
'$lgt_tr_msg'(_, Obj, _, _) :-
	nonvar(Obj),
	\+ callable(Obj),
	throw(type_error(object_identifier, Obj)).


% not runtime message translation; remember object receiving message

'$lgt_tr_msg'(_, Obj, _, This) :-
	nonvar(Obj),
	This \== user,
	\+ functor(Obj, {}, 1),					
	'$lgt_add_referenced_object'(Obj),
	fail.


% convenient access to parametric object proxies

'$lgt_tr_msg'(Pred, Obj, (catch(Proxy, error(Error, _), throw(error(Error, Obj::Pred, This))), TPred), This) :-
	nonvar(Obj),
	Obj = {Proxy},
	!,
	(   var(Proxy) ->
		'$lgt_tr_msg'(Pred, Proxy, TPred, This)
	;   callable(Proxy) ->
		'$lgt_tr_msg'(Pred, Proxy, TPred, This)
	;   throw(type_error(object_identifier, Proxy))
	).


% messages to the pseudo-object "user"

'$lgt_tr_msg'(Pred, Obj, Pred, _) :-
	Obj == user,
	!.


% translation performed at runtime

'$lgt_tr_msg'(Pred, Obj, TPred, This) :-
	var(Pred),
	!,
	(	'$lgt_compiler_flag'(events, allow) ->
		TPred = '$lgt_send_to_obj'(Obj, Pred, This)
	;	TPred = '$lgt_send_to_obj_ne'(Obj, Pred, This)
	).


% invalid message

'$lgt_tr_msg'(Pred, _, _, _) :-
	\+ callable(Pred),
	throw(type_error(callable, Pred)).


% broadcasting control constructs

'$lgt_tr_msg'((Pred1, Pred2), Obj, (TPred1, TPred2), This) :-
	!,
	'$lgt_tr_msg'(Pred1, Obj, TPred1, This),
	'$lgt_tr_msg'(Pred2, Obj, TPred2, This).

'$lgt_tr_msg'((Pred1; Pred2), Obj, (TPred1; TPred2), This) :-
	!,
	'$lgt_tr_msg'(Pred1, Obj, TPred1, This),
	'$lgt_tr_msg'(Pred2, Obj, TPred2, This).

'$lgt_tr_msg'((Pred1 -> Pred2), Obj, (TPred1 -> TPred2), This) :-
	!,
	'$lgt_tr_msg'(Pred1, Obj, TPred1, This),
	'$lgt_tr_msg'(Pred2, Obj, TPred2, This).

'$lgt_tr_msg'(!, Obj, ('$lgt_obj_exists'(Obj, !, This), !), This) :-
	!.


% built-in methods that cannot be redefined

'$lgt_tr_msg'(true, Obj, ('$lgt_obj_exists'(Obj, true, This), true), This) :-
	!.

'$lgt_tr_msg'(fail, Obj, ('$lgt_obj_exists'(Obj, fail, This), fail), This) :-
	!.

'$lgt_tr_msg'(repeat, Obj, ('$lgt_obj_exists'(Obj, repeat, This), repeat), This) :-
	!.


% "reflection" built-in predicates

'$lgt_tr_msg'(current_predicate(Pred), Obj, '$lgt_current_predicate'(Obj, Pred, This, p(p(p))), This) :-
	!.

'$lgt_tr_msg'(predicate_property(Pred, Prop), Obj, '$lgt_predicate_property'(Obj, Pred, Prop, This, p(p(p))), This) :-
	!.


% database handling built-in predicates

'$lgt_tr_msg'(abolish(Pred), Obj, TPred, This) :-
	!,
	(	'$lgt_runtime_db_pred_ind_chk'(Pred) ->
		TPred = '$lgt_abolish'(Obj, Pred, This, p(p(p)))
	;	'$lgt_must_be'(predicate_indicator, Pred),
		TPred = '$lgt_abolish_chk'(Obj, Pred, This, p(p(p)))
	).

'$lgt_tr_msg'(assert(Pred), Obj, TPred, This) :-
	!,
	'$lgt_tr_msg'(assertz(Pred), Obj, TPred, This).

'$lgt_tr_msg'(asserta(Pred), Obj, TPred, This) :-
	!,
	(	'$lgt_runtime_db_clause_chk'(Pred) ->
		TPred = '$lgt_asserta'(Obj, Pred, This, p(p(_)), p(p(p)))
	;	'$lgt_compiler_db_clause_chk'(Pred),
		(	Pred = (Head :- Body) ->
			(	Body == true ->
				TPred = '$lgt_asserta_fact_chk'(Obj, Head, This, p(p(_)), p(p(p)))
			;	TPred = '$lgt_asserta_rule_chk'(Obj, Pred, This, p(p(_)), p(p(p)))
			)
		;	TPred = '$lgt_asserta_fact_chk'(Obj, Pred, This, p(p(_)), p(p(p)))
		)
	).

'$lgt_tr_msg'(assertz(Pred), Obj, TPred, This) :-
	!,
	(	'$lgt_runtime_db_clause_chk'(Pred) ->
		TPred = '$lgt_assertz'(Obj, Pred, This, p(p(_)), p(p(p)))
	;	'$lgt_compiler_db_clause_chk'(Pred),
		(	Pred = (Head :- Body) ->
			(	Body == true ->
				TPred = '$lgt_assertz_fact_chk'(Obj, Head, This, p(p(_)), p(p(p)))
			;	TPred = '$lgt_assertz_rule_chk'(Obj, Pred, This, p(p(_)), p(p(p)))
			)
		;	TPred = '$lgt_assertz_fact_chk'(Obj, Pred, This, p(p(_)), p(p(p)))
		)
	).

'$lgt_tr_msg'(clause(Head, Body), Obj, TPred, This) :-
	!,
	(	'$lgt_runtime_db_clause_chk'((Head :- Body)) ->
		TPred = '$lgt_clause'(Obj, Head, Body, This, p(p(p)))
	;	'$lgt_compiler_db_clause_chk'((Head :- Body)),
		TPred = '$lgt_clause_chk'(Obj, Head, Body, This, p(p(p)))
	).

'$lgt_tr_msg'(retract(Pred), Obj, TPred, This) :-
	!,
	(	'$lgt_runtime_db_clause_chk'(Pred) ->
		TPred = '$lgt_retract'(Obj, Pred, This, p(p(p)))
	;	'$lgt_compiler_db_clause_chk'(Pred),
		(	Pred = (Head :- Body) ->
			(	var(Body) ->
				'$lgt_retract_var_body_chk'(Obj, Pred, This, p(p(p)))
			;	Body == true ->
				TPred = '$lgt_retract_fact_chk'(Obj, Head, This, p(p(p)))
			;	TPred = '$lgt_retract_rule_chk'(Obj, Pred, This, p(p(p)))
			)
		;	TPred = '$lgt_retract_fact_chk'(Obj, Pred, This, p(p(p)))
		)
	).

'$lgt_tr_msg'(retractall(Pred), Obj, TPred, This) :-
	!,
	(	'$lgt_runtime_db_clause_chk'(Pred) ->
		TPred = '$lgt_retractall'(Obj, Pred, This, p(p(p)))
	;	'$lgt_compiler_db_clause_chk'(Pred),
		TPred = '$lgt_retractall_chk'(Obj, Pred, This, p(p(p)))
	).


% term and goal expansion predicates

'$lgt_tr_msg'(expand_term(Term, Clause), Obj, '$lgt_expand_term'(Obj, Term, Clause, This, p(p(p))), This) :-
	!.

'$lgt_tr_msg'(expand_goal(Goal, EGoal), Obj, '$lgt_expand_goal'(Obj, Goal, EGoal, This, p(p(p))), This) :-
	!.


% message is not a built-in control construct or a call to a built-in 
% (meta-)predicate: translation performed at runtime

'$lgt_tr_msg'(Pred, Obj, TPred, This) :-
	(	var(Obj) ->
		(	'$lgt_compiler_flag'(events, allow) ->
			TPred = '$lgt_send_to_obj'(Obj, Pred, This)
		;	TPred = '$lgt_send_to_obj_ne'(Obj, Pred, This)
		)
	;	(	'$lgt_compiler_flag'(events, allow) ->
			TPred = '$lgt_send_to_obj_'(Obj, Pred, This)
		;	TPred = '$lgt_send_to_obj_ne_'(Obj, Pred, This)
		)
	).



% '$lgt_tr_self_msg'(@term, -nonvar, @object_identifier, @object_identifier)
%
% translates the sending of a message to self


% translation performed at runtime

'$lgt_tr_self_msg'(Pred, '$lgt_send_to_self'(Self, Pred, This), This, Self) :-
	var(Pred),
	!.


% invalid message

'$lgt_tr_self_msg'(Pred, _, _, _) :-
	\+ callable(Pred),
	throw(type_error(callable, Pred)).


% broadcasting control constructs

'$lgt_tr_self_msg'((Pred1, Pred2), (TPred1, TPred2), This, Self) :-
	!,
	'$lgt_tr_self_msg'(Pred1, TPred1, This, Self),
	'$lgt_tr_self_msg'(Pred2, TPred2, This, Self).

'$lgt_tr_self_msg'((Pred1; Pred2), (TPred1; TPred2), This, Self) :-
	!,
	'$lgt_tr_self_msg'(Pred1, TPred1, This, Self),
	'$lgt_tr_self_msg'(Pred2, TPred2, This, Self).

'$lgt_tr_self_msg'((Pred1 -> Pred2), (TPred1 -> TPred2), This, Self) :-
	!,
	'$lgt_tr_self_msg'(Pred1, TPred1, This, Self),
	'$lgt_tr_self_msg'(Pred2, TPred2, This, Self).

'$lgt_tr_self_msg'(!, !, _, _) :-
	!.


% built-in methods that cannot be redefined

'$lgt_tr_self_msg'(true, true, _, _) :-
	!.

'$lgt_tr_self_msg'(fail, fail, _, _) :-
	!.

'$lgt_tr_self_msg'(repeat, repeat, _, _) :-
	!.


% "reflection" built-in predicates

'$lgt_tr_self_msg'(current_predicate(Pred), '$lgt_current_predicate'(Self, Pred, This, p(_)), This, Self) :-
	!.

'$lgt_tr_self_msg'(predicate_property(Pred, Prop), '$lgt_predicate_property'(Self, Pred, Prop, This, p(_)), This, Self) :-
	!.


% database handling built-in predicates

'$lgt_tr_self_msg'(abolish(Pred), TPred, This, Self) :-
	!,
	(	'$lgt_runtime_db_pred_ind_chk'(Pred) ->
		TPred = '$lgt_abolish'(Self, Pred, This, p(_))
	;	'$lgt_must_be'(predicate_indicator, Pred),
		TPred = '$lgt_abolish_chk'(Self, Pred, This, p(_))
	).

'$lgt_tr_self_msg'(assert(Pred), TPred, This, Self) :-
	!,
	'$lgt_tr_self_msg'(assertz(Pred), TPred, This, Self).

'$lgt_tr_self_msg'(asserta(Pred), TPred, This, Self) :-
	!,
	(	'$lgt_runtime_db_clause_chk'(Pred) ->
		TPred = '$lgt_asserta'(Self, Pred, This, p(_), p(p))
	;	'$lgt_compiler_db_clause_chk'(Pred),
		(	Pred = (Head :- Body) ->
			(	Body == true ->
				TPred = '$lgt_asserta_fact_chk'(Self, Head, This, p(_), p(p))
			;	TPred = '$lgt_asserta_rule_chk'(Self, Pred, This, p(_), p(p))
			)
		;	TPred = '$lgt_asserta_fact_chk'(Self, Pred, This, p(_), p(p))
		)
	).

'$lgt_tr_self_msg'(assertz(Pred), TPred, This, Self) :-
	!,
	(	'$lgt_runtime_db_clause_chk'(Pred) ->
		TPred = '$lgt_assertz'(Self, Pred, This, p(_), p(p))
	;	'$lgt_compiler_db_clause_chk'(Pred),
		(	Pred = (Head :- Body) ->
			(	Body == true ->
				TPred = '$lgt_assertz_fact_chk'(Self, Head, This, p(_), p(p))
			;	TPred = '$lgt_assertz_rule_chk'(Self, Pred, This, p(_), p(p))
			)
		;	TPred = '$lgt_assertz_fact_chk'(Self, Pred, This, p(_), p(p))
		)
	).

'$lgt_tr_self_msg'(clause(Head, Body), TPred, This, Self) :-
	!,
	(	'$lgt_runtime_db_clause_chk'((Head :- Body)) ->
		TPred = '$lgt_clause'(Self, Head, Body, This, p(_))
	;	'$lgt_compiler_db_clause_chk'((Head :- Body)),
		TPred = '$lgt_clause_chk'(Self, Head, Body, This, p(_))
	).

'$lgt_tr_self_msg'(retract(Pred), TPred, This, Self) :-
	!,
	(	'$lgt_runtime_db_clause_chk'(Pred) ->
		TPred = '$lgt_retract'(Self, Pred, This, p(_))
	;	'$lgt_compiler_db_clause_chk'(Pred),
		(	Pred = (Head :- Body) ->
			(	var(Body) ->
				'$lgt_retract_var_body_chk'(Self, Pred, This, p(_))
			;	Body == true ->
				TPred = '$lgt_retract_fact_chk'(Self, Head, This, p(_))
			;	TPred = '$lgt_retract_rule_chk'(Self, Pred, This, p(_))
			)
		;	TPred = '$lgt_retract_fact_chk'(Self, Pred, This, p(_))
		)
	).

'$lgt_tr_self_msg'(retractall(Pred), TPred, This, Self) :-
	!,
	(	'$lgt_runtime_db_clause_chk'(Pred) ->
		TPred = '$lgt_retractall'(Self, Pred, This, p(_))
	;	'$lgt_compiler_db_clause_chk'(Pred),
		TPred = '$lgt_retractall_chk'(Self, Pred, This, p(_))
	).


% term and goal expansion predicates

'$lgt_tr_self_msg'(expand_term(Term, Clause), '$lgt_expand_term'(Self, Term, Clause, This, p(_)), This, Self) :-
	!.

'$lgt_tr_self_msg'(expand_goal(Goal, EGoal), '$lgt_expand_goal'(Self, Goal, EGoal, This, p(_)), This, Self) :-
	!.


% message is not a built-in control construct or a call to a built-in 
% (meta-)predicate: translation performed at runtime

'$lgt_tr_self_msg'(Pred, '$lgt_send_to_self_'(Self, Pred, This), This, Self) :-
	!.



% '$lgt_tr_super_call'(@term, -term, +term)
%
% translates calling of redefined predicates ("super" calls)

'$lgt_tr_super_call'(Pred, _, _) :-				% invalid goal (not callable)
	nonvar(Pred),
	\+ callable(Pred),
	throw(type_error(callable, Pred)).

'$lgt_tr_super_call'(_, _, _) :-				% invalid goal (standalone object)
	'$lgt_pp_object_'(Obj, _, _, _, _, _, _, _, _, _, _),
	\+ '$lgt_pp_extended_object_'(_, _, _, _, _, _, _, _, _, _),
	\+ '$lgt_pp_instantiated_class_'(_, _, _, _, _, _, _, _, _, _),
	\+ '$lgt_pp_specialized_class_'(_, _, _, _, _, _, _, _, _, _),
	throw(existence_error(ancestor_object, Obj)).

'$lgt_tr_super_call'(_, _, _) :-				% invalid goal (not an extended category)
	'$lgt_pp_category_'(Ctg, _, _, _, _, _),
	\+ '$lgt_pp_extended_category_'(_, _, _, _, _),
	throw(existence_error(ancestor_category, Ctg)).

'$lgt_tr_super_call'(Pred, TPred, Ctx) :-		% translation performed at runtime
	nonvar(Pred),
	'$lgt_comp_ctx_head'(Ctx, Head),
	nonvar(Head),
	Head \= ':'(_, _),	% ignore multifile
	Head \= _::_,		% predicates
	'$lgt_term_template'(Pred, Head),			% "super" call to the predicate being redefined
	!,
	(   '$lgt_pp_object_'(_, _, _, _, Super, _, _, _, _, _, _) ->
		'$lgt_comp_ctx'(Ctx, _, _, This, Self, _, _, _, ExCtx, _, _),
		'$lgt_exec_ctx'(ExCtx, _, This, Self, _, _),
		TPred = '$lgt_obj_super_call_same_'(Super, Pred, ExCtx)
	;	'$lgt_pp_category_'(Ctg, _, _, _, _, _),
		'$lgt_comp_ctx'(Ctx, _, _, _, _, _, _, _, ExCtx, _, _),
		TPred = '$lgt_ctg_super_call_same_'(Ctg, Pred, ExCtx)
	).

'$lgt_tr_super_call'(Pred, TPred, Ctx) :-		% "super" call to a predicate other than the one being redefined
	'$lgt_comp_ctx_exec_ctx'(Ctx, ExCtx),
	(   '$lgt_pp_object_'(_, _, _, _, Super, _, _, _, _, _, _) ->
		(	var(Pred) ->
			TPred = '$lgt_obj_super_call_other'(Super, Pred, ExCtx)
		;	TPred = '$lgt_obj_super_call_other_'(Super, Pred, ExCtx)
		)
	;	'$lgt_pp_category_'(Ctg, _, _, _, _, _),
		(	var(Pred) ->
			TPred = '$lgt_ctg_super_call_other'(Ctg, Pred, ExCtx)
		;	TPred = '$lgt_ctg_super_call_other_'(Ctg, Pred, ExCtx)
		)
	).



% '$lgt_tr_ctx_call'(@term, @term, -callable, @object_identifier)
%
% translates context switching calls

'$lgt_tr_ctx_call'(Obj, _, _, _) :-
	nonvar(Obj),
	\+ callable(Obj),
	throw(type_error(object_identifier, Obj)).

'$lgt_tr_ctx_call'(_, Goal, _, _) :-
	nonvar(Goal),
	\+ callable(Goal),
	throw(type_error(callable, Goal)).

'$lgt_tr_ctx_call'(Obj, Goal, '$lgt_call_within_context'(Obj, Goal, This), This) :-
	(	var(Obj)
	;	var(Goal)
	),
	!.

'$lgt_tr_ctx_call'(Obj, Goal, '$lgt_call_within_context_nv'(Obj, Goal, This), This).



% '$lgt_pred_meta_vars'(+callable, -list)
%
% constructs a list of all variables that occur
% in a position corresponding to a meta-argument

'$lgt_pred_meta_vars'(Pred, MetaVars) :-
	'$lgt_term_template'(Pred, Meta),
	(	'$lgt_pp_meta_predicate_'(Meta) ->
		Pred =.. [_| Args],
		Meta =.. [_| MArgs],
		'$lgt_extract_meta_vars'(Args, MArgs, MetaVars)
	;	MetaVars = []
	).



% '$lgt_pred_meta_vars'(+callable, +callable, -list)
%
% constructs a list of all variables that occur
% in a position corresponding to a meta-argument

'$lgt_pred_meta_vars'(Pred, Meta, MetaVars) :-
	(	Meta == no ->
		MetaVars = []
	;	Pred =.. [_| Args],
		Meta =.. [_| MArgs],
		'$lgt_extract_meta_vars'(Args, MArgs, MetaVars)
	).



% '$lgt_extract_meta_vars'(+list, +list, -list)

'$lgt_extract_meta_vars'([], [], []).

'$lgt_extract_meta_vars'([Var| Args], [MArg| MArgs], [Var| MetaVars]) :-
	var(Var),
	MArg \== (*),
	!,
	'$lgt_extract_meta_vars'(Args, MArgs, MetaVars).

'$lgt_extract_meta_vars'([_| Args], [_| MArgs], MetaVars) :-
	'$lgt_extract_meta_vars'(Args, MArgs, MetaVars).



% '$lgt_iso_read_term'(@stream, ?term, +read_options_list, @list)
%
% wraps read_term/3 call with the necessary operator settings

'$lgt_iso_read_term'(Stream, Term, Options, Ops) :-
	catch(
		('$lgt_save_operators'(Ops, Saved),
		 '$lgt_add_operators'(Ops),
		 read_term(Stream, Term, Options),
		 '$lgt_remove_operators'(Ops),
		 '$lgt_add_operators'(Saved)),
		Error,
		'$lgt_iso_read_error_handler'(Ops, Saved, Error)).



% '$lgt_iso_read_term'(?term, +read_options_list, @list)
%
% wraps read_term/2 call with the necessary operator settings

'$lgt_iso_read_term'(Term, Options, Ops) :-
	catch(
		('$lgt_save_operators'(Ops, Saved),
		 '$lgt_add_operators'(Ops),
		 read_term(Term, Options),
		 '$lgt_remove_operators'(Ops),
		 '$lgt_add_operators'(Saved)),
		Error,
		'$lgt_iso_read_error_handler'(Ops, Saved, Error)).



% '$lgt_iso_read'(@stream, ?term, @list)
%
% wraps read/2 call with the necessary operator settings

'$lgt_iso_read'(Stream, Term, Ops) :-
	catch(
		('$lgt_save_operators'(Ops, Saved),
		 '$lgt_add_operators'(Ops),
		 read(Stream, Term),
		 '$lgt_remove_operators'(Ops),
		 '$lgt_add_operators'(Saved)),
		Error,
		'$lgt_iso_read_error_handler'(Ops, Saved, Error)).



% '$lgt_iso_read'(?term, @list)
%
% wraps read/1 call with the necessary operator settings

'$lgt_iso_read'(Term, Ops) :-
	catch(
		('$lgt_save_operators'(Ops, Saved),
		 '$lgt_add_operators'(Ops),
		 read(Term),
		 '$lgt_remove_operators'(Ops),
		 '$lgt_add_operators'(Saved)),
		Error,
		'$lgt_iso_read_error_handler'(Ops, Saved, Error)).



% '$lgt_iso_write_term'(@stream_or_alias, @term, @write_options_list, @list)
%
% wraps write_term/3 call with the necessary operator settings

'$lgt_iso_write_term'(Stream, Term, Options, Ops) :-
	catch(
		('$lgt_save_operators'(Ops, Saved),
		 '$lgt_add_operators'(Ops),
		 write_term(Stream, Term, Options),
		 '$lgt_remove_operators'(Ops),
		 '$lgt_add_operators'(Saved)),
		Error,
		'$lgt_iso_read_error_handler'(Ops, Saved, Error)).



% '$lgt_iso_write_term'(@term, @write_options_list, @list)
%
% wraps write_term/2 call with the necessary operator settings

'$lgt_iso_write_term'(Term, Options, Ops) :-
	catch(
		('$lgt_save_operators'(Ops, Saved),
		 '$lgt_add_operators'(Ops),
		 write_term(Term, Options),
		 '$lgt_remove_operators'(Ops),
		 '$lgt_add_operators'(Saved)),
		Error,
		'$lgt_iso_read_error_handler'(Ops, Saved, Error)).



% '$lgt_iso_write'(@stream_or_alias, @term, @list)
%
% wraps write/2 call with the necessary operator settings

'$lgt_iso_write'(Stream, Term, Ops) :-
	catch(
		('$lgt_save_operators'(Ops, Saved),
		 '$lgt_add_operators'(Ops),
		 write(Stream, Term),
		 '$lgt_remove_operators'(Ops),
		 '$lgt_add_operators'(Saved)),
		Error,
		'$lgt_iso_read_error_handler'(Ops, Saved, Error)).



% '$lgt_iso_write'(@term, @list)
%
% wraps write/1 call with the necessary operator settings

'$lgt_iso_write'(Term, Ops):-
	catch(
		('$lgt_save_operators'(Ops, Saved),
		 '$lgt_add_operators'(Ops),
		 write(Term),
		 '$lgt_remove_operators'(Ops),
		 '$lgt_add_operators'(Saved)),
		Error,
		'$lgt_iso_read_error_handler'(Ops, Saved, Error)).



% '$lgt_iso_writeq'(@stream_or_alias, @term, @list)
%
% wraps writeq/2 call with the necessary operator settings

'$lgt_iso_writeq'(Stream, Term, Ops) :-
	catch(
		('$lgt_save_operators'(Ops, Saved),
		 '$lgt_add_operators'(Ops),
		 writeq(Stream, Term),
		 '$lgt_remove_operators'(Ops),
		 '$lgt_add_operators'(Saved)),
		Error,
		'$lgt_iso_read_error_handler'(Ops, Saved, Error)).



% '$lgt_iso_writeq'(@term, @list)
%
% wraps writeq/1 call with the necessary operator settings

'$lgt_iso_writeq'(Term, Ops) :-
	catch(
		('$lgt_save_operators'(Ops, Saved),
		 '$lgt_add_operators'(Ops),
		 writeq(Term),
		 '$lgt_remove_operators'(Ops),
		 '$lgt_add_operators'(Saved)),
		Error,
		'$lgt_iso_read_error_handler'(Ops, Saved, Error)).



% '$lgt_save_operators'(@list, -list)
%
% saves currently defined operators that might be
% redefined when a list of operators is added

'$lgt_save_operators'([], []).

'$lgt_save_operators'([op(_, Spec, Op)| Ops], Saved) :-
	(	current_op(Pr, SCSpec, Op),
		'$lgt_same_op_class'(Spec, SCSpec) ->
		Saved = [op(Pr, SCSpec, Op)| Saved2]
	;	Saved = Saved2
	),
	'$lgt_save_operators'(Ops, Saved2).



% '$lgt_add_operators'(@list)
%
% adds operators to the global operator table

'$lgt_add_operators'([]).

'$lgt_add_operators'([op(Pr, Spec, Op)| Ops]) :-
	op(Pr, Spec, Op),
	'$lgt_add_operators'(Ops).



% '$lgt_remove_operators'(@list)
%
% removes operators from the global operator table

'$lgt_remove_operators'([]).

'$lgt_remove_operators'([op(_, Spec, Op)| Ops]) :-
	op(0, Spec, Op),
	'$lgt_remove_operators'(Ops).



% '$lgt_iso_read_error_handler'(@list, @list, @nonvar)
%
% restores operator table to the its state before the call
% to one of the '$lgt_iso_read...' that raised an error

'$lgt_iso_read_error_handler'(Ops, Saved, Error) :-
	'$lgt_remove_operators'(Ops),
	'$lgt_add_operators'(Saved),
	throw(Error).



% '$lgt_simplify_body'(+callable, -callable)
%
% removes redundant calls to true/0 from a translated clause body;
% we must be careful with control constructs that are opaque to
% cuts such as call/1 and once/1

'$lgt_simplify_body'(G, G) :-
	var(G),
	!.

'$lgt_simplify_body'(catch(G, E, R), catch(SG, E, SR)) :-
	!,
	'$lgt_simplify_body'(G, SG),
	'$lgt_simplify_body'(R, SR).

'$lgt_simplify_body'(call(G), true) :-
	G == !,
	!.
'$lgt_simplify_body'(call(G), G) :-
	nonvar(G),
	functor(G, '$lgt_metacall', _),
	!.
'$lgt_simplify_body'(call(G), call(SG)) :-
	!,
	'$lgt_simplify_body'(G, SG).

'$lgt_simplify_body'(once(G), true) :-
	G == !,
	!.
'$lgt_simplify_body'(once(G), once(SG)) :-
	!,
	'$lgt_simplify_body'(G, SG).

'$lgt_simplify_body'(ignore(G), ignore(SG)) :-
	!,
	'$lgt_simplify_body'(G, SG).

'$lgt_simplify_body'(bagof(T, G, L), bagof(T, SG, L)) :-
	!,
	'$lgt_simplify_body'(G, SG).

'$lgt_simplify_body'(setof(T, G, L), setof(T, SG, L)) :-
	!,
	'$lgt_simplify_body'(G, SG).

'$lgt_simplify_body'(findall(T, G, L), findall(T, SG, L)) :-
	!,
	'$lgt_simplify_body'(G, SG).

'$lgt_simplify_body'(forall(G, T), forall(SG, ST)) :-
	!,
	'$lgt_simplify_body'(G, SG),
	'$lgt_simplify_body'(T, ST).

'$lgt_simplify_body'((A; B), (SA; SB)) :-
	!,
	'$lgt_simplify_body'(A, SA),
	'$lgt_simplify_body'(B, SB).

'$lgt_simplify_body'((A -> B), (SA -> SB)) :-
	!,
	'$lgt_simplify_body'(A, SA),
	'$lgt_simplify_body'(B, SB).

'$lgt_simplify_body'((true, B), SB) :-
	!,
	'$lgt_simplify_body'(B, SB).

'$lgt_simplify_body'((B, true), SB) :-
	!,
	'$lgt_simplify_body'(B, SB).

'$lgt_simplify_body'((A, B), (SA, SB)) :-
	!,
	'$lgt_simplify_body'(A, SA),
	'$lgt_simplify_body'(B, SB).

'$lgt_simplify_body'(\+ G, \+ SG) :-
	!,
	'$lgt_simplify_body'(G, SG).

'$lgt_simplify_body'(G, G).



% '$lgt_tr_object_identifier'(+object_identifier, +atom)
%
% from the object identifier construct the set of 
% functor prefixes used in the compiled code clauses

'$lgt_tr_object_identifier'(Obj, Mode) :-
	'$lgt_entity_template'(Obj, GObj),
	'$lgt_add_referenced_object'(GObj),
	'$lgt_construct_object_functors'(GObj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm),
	'$lgt_tr_entity_flags'(object, Mode, Flags),
	assertz('$lgt_pp_object_'(GObj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm, Flags)),
	asserta('$lgt_pp_predicate_mutex_counter_'(0)).



% '$lgt_tr_category_identifier'(+category_identifier, +atom)
%
% from the category identifier construct the set of 
% functor prefixes used in the compiled code clauses

'$lgt_tr_category_identifier'(Ctg, Mode) :-
	'$lgt_entity_template'(Ctg, GCtg),
	'$lgt_add_referenced_category'(GCtg),
	'$lgt_construct_category_functors'(GCtg, Prefix, Dcl, Def, Rnm),
	'$lgt_tr_entity_flags'(category, Mode, Flags),
	assertz('$lgt_pp_category_'(GCtg, Prefix, Dcl, Def, Rnm, Flags)),
	asserta('$lgt_pp_predicate_mutex_counter_'(0)).



% '$lgt_tr_protocol_identifier'(+protocol_identifier, +atom)
%
% from the protocol identifier construct the set of  
% functor prefixes used in the compiled code clauses

'$lgt_tr_protocol_identifier'(Ptc, Mode) :-
	'$lgt_add_referenced_protocol'(Ptc),
	'$lgt_construct_protocol_functors'(Ptc, Prefix, Dcl, Rnm),
	'$lgt_tr_entity_flags'(protocol, Mode, Flags),
	assertz('$lgt_pp_protocol_'(Ptc, Prefix, Dcl, Rnm, Flags)),
	% necessary in order to be able to save synchronized predicate properties:
	asserta('$lgt_pp_predicate_mutex_counter_'(0)). 



% '$lgt_tr_implements_protocol'(+list, +object_identifier)
% '$lgt_tr_implements_protocol'(+list, +category_identifier)
%
% translates an "implementents" relation between a category or an object and a list of protocols

'$lgt_tr_implements_protocol'([], _).

'$lgt_tr_implements_protocol'([Ref| _], _) :-
	var(Ref),
	throw(instantiation_error).

'$lgt_tr_implements_protocol'([Scope::Ptc| _], _) :-
	(var(Scope); var(Ptc)),
	throw(instantiation_error).

'$lgt_tr_implements_protocol'([Ref| Refs], ObjOrCtg) :-
	(	'$lgt_valid_ref_scope'(Ref, Scope) ->
		(	'$lgt_valid_protocol_ref'(Ref, Ptc) ->
			(	ObjOrCtg \= Ptc ->
				(	('$lgt_is_object'(Ptc); '$lgt_is_category'(Ptc)) ->
					throw(type_error(protocol, Ptc))
				;	'$lgt_add_referenced_protocol'(Ptc),
					assertz('$lgt_pp_relation_clause_'('$lgt_implements_protocol_'(ObjOrCtg, Ptc, Scope))),
					'$lgt_construct_protocol_functors'(Ptc, Prefix, Dcl, _),
					assertz('$lgt_pp_implemented_protocol_'(Ptc, Prefix, Dcl, Scope)),
					'$lgt_tr_implements_protocol'(Refs, ObjOrCtg)
				)
			;	throw(permission_error(implement, self, ObjOrCtg))
			)
		;	throw(type_error(protocol_identifier, Ref))
		)
	;	throw(type_error(scope, Ref))
	).



% '$lgt_tr_imports_category'(+list, +object_identifier)
%
% translates an "imports" relation between an object and a list of categories 

'$lgt_tr_imports_category'([], _).

'$lgt_tr_imports_category'([Ref| _], _) :-
	var(Ref),
	throw(instantiation_error).

'$lgt_tr_imports_category'([Scope::Ctg| _], _) :-
	(var(Scope); var(Ctg)),
	throw(instantiation_error).

'$lgt_tr_imports_category'([Ref| Refs], Obj) :-
	(	'$lgt_valid_ref_scope'(Ref, Scope) ->
		(	'$lgt_valid_category_ref'(Ref, Ctg) ->
			(	functor(Obj, Functor, Arity), \+ functor(Ctg, Functor, Arity) ->
				(	('$lgt_is_object'(Ctg); '$lgt_is_protocol'(Ctg)) ->
					throw(type_error(category, Ctg))
				;	'$lgt_add_referenced_category'(Ctg),
					assertz('$lgt_pp_relation_clause_'('$lgt_imports_category_'(Obj, Ctg, Scope))),
					'$lgt_construct_category_functors'(Ctg, Prefix, Dcl, Def, _),
					assertz('$lgt_pp_imported_category_'(Ctg, Prefix, Dcl, Def, Scope)),
					'$lgt_tr_imports_category'(Refs, Obj)
				)
			;	throw(permission_error(import, self, Obj))
			)
		;	throw(type_error(category_identifier, Ref))
		)
	;	throw(type_error(scope, Ref))
	).



% '$lgt_tr_instantiates_class'(+list, +object_identifier)
%
% translates an "instantiates" relation between an instance and a list of classes

'$lgt_tr_instantiates_class'([], _).

'$lgt_tr_instantiates_class'([Ref| _], _) :-
	var(Ref),
	throw(instantiation_error).

'$lgt_tr_instantiates_class'([Scope::Class| _], _) :-
	(var(Scope); var(Class)),
	throw(instantiation_error).

'$lgt_tr_instantiates_class'([Ref| Refs], Obj) :-
	(	'$lgt_valid_ref_scope'(Ref, Scope) ->
		(	'$lgt_valid_object_ref'(Ref, Class) ->
			(	('$lgt_is_protocol'(Class); '$lgt_is_category'(Class)) ->
				throw(type_error(object, Class))
			;	(	\+ '$lgt_is_prototype'(Class) ->
					'$lgt_add_referenced_object'(Class),
					assertz('$lgt_pp_relation_clause_'('$lgt_instantiates_class_'(Obj, Class, Scope))),
					'$lgt_construct_object_functors'(Class, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, _),
					assertz('$lgt_pp_instantiated_class_'(Class, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Scope)),
					'$lgt_tr_instantiates_class'(Refs, Obj)
				;	throw(domain_error(class, Class))
				)
			)
		;	throw(type_error(object_identifier, Ref))
		)
	;	throw(type_error(scope, Ref))
	).



% '$lgt_tr_specializes_class'(+list, +object_identifier)
%
% translates a "specializes" relation between a class and a list of superclasses

'$lgt_tr_specializes_class'([], _).

'$lgt_tr_specializes_class'([Ref| _], _) :-
	var(Ref),
	throw(instantiation_error).

'$lgt_tr_specializes_class'([Scope::Superclass| _], _) :-
	(var(Scope); var(Superclass)),
	throw(instantiation_error).

'$lgt_tr_specializes_class'([Ref| Refs], Class) :-
	(	'$lgt_valid_ref_scope'(Ref, Scope) ->
		(	'$lgt_valid_object_ref'(Ref, Superclass) ->
			(	functor(Class, Functor, Arity), \+ functor(Superclass, Functor, Arity) ->
				(	('$lgt_is_protocol'(Superclass); '$lgt_is_category'(Superclass)) ->
					throw(type_error(object, Superclass))
				;	(	\+ '$lgt_is_prototype'(Class) ->
						'$lgt_add_referenced_object'(Superclass),
						assertz('$lgt_pp_relation_clause_'('$lgt_specializes_class_'(Class, Superclass, Scope))),
						'$lgt_construct_object_functors'(Superclass, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, _),
						assertz('$lgt_pp_specialized_class_'(Superclass, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Scope)),
						'$lgt_tr_specializes_class'(Refs, Class)
					;	throw(domain_error(class, Class))
					)
				)
			;	throw(permission_error(specialize, self, Class))
			)
		;	throw(type_error(object_identifier, Ref))
		)
	;	throw(type_error(scope, Ref))
	).



% '$lgt_tr_extends_object'(+list, +object_identifier)
%
% translates an "extends" relation between a prototype and a list of parents

'$lgt_tr_extends_object'([], _).

'$lgt_tr_extends_object'([Ref| _], _) :-
	var(Ref),
	throw(instantiation_error).

'$lgt_tr_extends_object'([Scope::Parent| _], _) :-
	(var(Scope); var(Parent)),
	throw(instantiation_error).

'$lgt_tr_extends_object'([Ref| Refs], Obj) :-
	(	'$lgt_valid_ref_scope'(Ref, Scope) ->
		(	'$lgt_valid_object_ref'(Ref, Parent) ->
			(	functor(Obj, Functor, Arity), \+ functor(Parent, Functor, Arity) ->
				(	('$lgt_is_protocol'(Parent); '$lgt_is_category'(Parent)) ->
					throw(type_error(object, Parent))
				;	(	\+ '$lgt_is_class'(Parent) ->
						'$lgt_add_referenced_object'(Parent),
						assertz('$lgt_pp_relation_clause_'('$lgt_extends_object_'(Obj, Parent, Scope))),
						'$lgt_construct_object_functors'(Parent, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, _),
						assertz('$lgt_pp_extended_object_'(Parent, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Scope)),
						'$lgt_tr_extends_object'(Refs, Obj)
					;	throw(domain_error(prototype, Parent))
					)
				)
			;	throw(permission_error(extend, self, Obj))
			)
		;	throw(type_error(object_identifier, Ref))
		)
	;	throw(type_error(scope, Ref))
	).



% '$lgt_tr_extends_protocol'(+list, +protocol_identifier)
%
% translates an "extends" relation between a protocol and a list of protocols

'$lgt_tr_extends_protocol'([], _).

'$lgt_tr_extends_protocol'([Ref| _], _) :-
	var(Ref),
	throw(instantiation_error).

'$lgt_tr_extends_protocol'([Scope::Ptc| _], _) :-
	(var(Scope); var(Ptc)),
	throw(instantiation_error).

'$lgt_tr_extends_protocol'([Ref| Refs], Ptc1) :-
	(	'$lgt_valid_ref_scope'(Ref, Scope) ->
		(	'$lgt_valid_protocol_ref'(Ref, Ptc2) ->
			(	Ptc1 \= Ptc2 ->
				(	('$lgt_is_object'(Ptc2); '$lgt_is_category'(Ptc2)) ->
					throw(type_error(protocol, Ptc2))
				;	'$lgt_add_referenced_protocol'(Ptc2),
					assertz('$lgt_pp_relation_clause_'('$lgt_extends_protocol_'(Ptc1, Ptc2, Scope))),
					'$lgt_construct_protocol_functors'(Ptc2, Prefix, Dcl, _),
					assertz('$lgt_pp_extended_protocol_'(Ptc2, Prefix, Dcl, Scope)),
					'$lgt_tr_extends_protocol'(Refs, Ptc1)
				)
			;	throw(permission_error(extend, self, Ptc1))
			)
		;	throw(type_error(protocol_identifier, Ref))
		)
	;	throw(type_error(scope, Ref))
	).



% '$lgt_tr_extends_category'(+list, +category_identifier)
%
% translates an "extends" relation between a category and a list of categories

'$lgt_tr_extends_category'([], _).

'$lgt_tr_extends_category'([Ref| _], _) :-
	var(Ref),
	throw(instantiation_error).

'$lgt_tr_extends_category'([Scope::Ctg| _], _) :-
	(var(Scope); var(Ctg)),
	throw(instantiation_error).

'$lgt_tr_extends_category'([Ref| Refs], Ctg1) :-
	(	'$lgt_valid_ref_scope'(Ref, Scope) ->
		(	'$lgt_valid_category_ref'(Ref, Ctg2) ->
			(	functor(Ctg1, Functor, Arity), \+ functor(Ctg2, Functor, Arity) ->
				(	('$lgt_is_object'(Ctg2); '$lgt_is_protocol'(Ctg2)) ->
					throw(type_error(category, Ctg2))
				;	'$lgt_add_referenced_category'(Ctg2),
					assertz('$lgt_pp_relation_clause_'('$lgt_extends_category_'(Ctg1, Ctg2, Scope))),
					'$lgt_construct_category_functors'(Ctg2, Prefix, Dcl, Def, _),
					assertz('$lgt_pp_extended_category_'(Ctg2, Prefix, Dcl, Def, Scope)),
					'$lgt_tr_extends_category'(Refs, Ctg1)
				)
			;	throw(permission_error(extend, self, Ctg1))
			)
		;	throw(type_error(category_identifier, Ref))
		)
	;	throw(type_error(scope, Ref))
	).



% '$lgt_tr_complements_category'(+list, +category_identifier)
%
% translates a "complements" relation between a category and a list of objects

'$lgt_tr_complements_category'(Objs, Ctg) :-
	'$lgt_pp_category_'(Ctg, _, Dcl, Def, Rnm, _),
	'$lgt_tr_complements_category'(Objs, Ctg, Dcl, Def, Rnm).


'$lgt_tr_complements_category'([], _, _, _, _).

'$lgt_tr_complements_category'([Obj| _], _, _, _, _) :-
	var(Obj),
	throw(instantiation_error).

'$lgt_tr_complements_category'([Obj| _], _, _, _, _) :-
	\+ callable(Obj),
	throw(type_error(object_identifier, Obj)).

'$lgt_tr_complements_category'([Obj| _], _, _, _, _) :-
	('$lgt_is_protocol'(Obj); '$lgt_is_category'(Obj)),
	throw(type_error(object, Obj)).	

'$lgt_tr_complements_category'([Obj| _], Ctg, _, _, _) :-
	'$lgt_entity_template'(Obj, Ctg),
	throw(permission_error(complement, self, Obj)).

'$lgt_tr_complements_category'([Obj| Objs], Ctg, Dcl, Def, Rnm) :-
	'$lgt_add_referenced_object'(Obj),
	assertz('$lgt_pp_complemented_object_'(Obj)),
	assertz('$lgt_pp_relation_clause_'('$lgt_complemented_object_'(Obj, Ctg, Dcl, Def, Rnm))),
	'$lgt_tr_complements_category'(Objs, Ctg, Dcl, Def, Rnm).



% '$lgt_is_prototype'(+entity_identifier)
%
% true if the argument is a defined prototype or a prototype being compiled

'$lgt_is_prototype'(Obj) :-
	(	'$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, _) ->
		% existing object; first, check that is not being compiled as a different kind of entity:
		\+ '$lgt_pp_file_relation_clause_'('$lgt_current_protocol_'(Obj, _, _, _, _)),
		\+ '$lgt_pp_file_relation_clause_'('$lgt_current_category_'(Obj, _, _, _, _, _)),
		% second, check that it's a prototype:
		\+ '$lgt_instantiates_class_'(Obj, _, _),
		\+ '$lgt_instantiates_class_'(_, Obj, _),
		\+ '$lgt_specializes_class_'(Obj, _, _),
		\+ '$lgt_specializes_class_'(_, Obj, _)
	;	'$lgt_pp_file_relation_clause_'('$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, _)) ->
		% object defined previously in the same file; check that it's a prototype:
		\+ '$lgt_pp_file_relation_clause_'('$lgt_instantiates_class_'(Obj, _, _)),
		\+ '$lgt_pp_file_relation_clause_'('$lgt_instantiates_class_'(_, Obj, _)),
		\+ '$lgt_pp_file_relation_clause_'('$lgt_specializes_class_'(Obj, _, _)),
		\+ '$lgt_pp_file_relation_clause_'('$lgt_specializes_class_'(_, Obj, _))
	;	fail
	).



% '$lgt_is_class'(+entity_identifier)
%
% true if the argument is a defined class or a class being compiled

'$lgt_is_class'(Obj) :-
	(	'$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, _) ->
		% existing object; first, check that is not being compiled as a different kind of entity:
		\+ '$lgt_pp_file_relation_clause_'('$lgt_current_protocol_'(Obj, _, _, _, _)),
		\+ '$lgt_pp_file_relation_clause_'('$lgt_current_category_'(Obj, _, _, _, _, _)),
		% second, check that it's an instance or a class:
		(	'$lgt_instantiates_class_'(Obj, _, _)
		;	'$lgt_instantiates_class_'(_, Obj, _)
		;	'$lgt_specializes_class_'(Obj, _, _)
		;	'$lgt_specializes_class_'(_, Obj, _)
		)
	;	'$lgt_pp_file_relation_clause_'('$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, _)) ->
		% object defined previously in the same file; check that it's an instance or a class:
		(	'$lgt_pp_file_relation_clause_'('$lgt_instantiates_class_'(Obj, _, _))
		;	'$lgt_pp_file_relation_clause_'('$lgt_instantiates_class_'(_, Obj, _))
		;	'$lgt_pp_file_relation_clause_'('$lgt_specializes_class_'(Obj, _, _))
		;	'$lgt_pp_file_relation_clause_'('$lgt_specializes_class_'(_, Obj, _))
		)
	;	fail
	).



% '$lgt_is_object'(+entity_identifier)
%
% true if the argument is a defined object or an object being compiled

'$lgt_is_object'(Obj) :-
	(	'$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, _) ->
		% existing object; check that is not being compiled as a different kind of entity:
		\+ '$lgt_pp_file_relation_clause_'('$lgt_current_protocol_'(Obj, _, _, _, _)),
		\+ '$lgt_pp_file_relation_clause_'('$lgt_current_category_'(Obj, _, _, _, _, _))
	;	'$lgt_pp_file_relation_clause_'('$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, _)) ->
		% object defined previously in the same file
		true
	;	fail
	).



% '$lgt_is_protocol'(+entity_identifier)
%
% true if the argument is a defined protocol or a protocol being compiled

'$lgt_is_protocol'(Ptc) :-
	(	'$lgt_current_protocol_'(Ptc, _, _, _, _) ->
		% existing protocol; check that is not being compiled as a different kind of entity:
		\+ '$lgt_pp_file_relation_clause_'('$lgt_current_object_'(Ptc, _, _, _, _, _, _, _, _, _, _)),
		\+ '$lgt_pp_file_relation_clause_'('$lgt_current_category_'(Ptc, _, _, _, _, _))
	;	'$lgt_pp_file_relation_clause_'('$lgt_current_protocol_'(Ptc, _, _, _, _)) ->
		% protocol defined previously in the same file
		true
	;	fail
	).



% '$lgt_is_category'(+entity_identifier)
%
% true if the argument is a defined category or a category being compiled

'$lgt_is_category'(Ctg) :-
	(	'$lgt_current_category_'(Ctg, _, _, _, _, _) ->
		% existing category; check that is not being compiled as a different kind of entity:
		\+ '$lgt_pp_file_relation_clause_'('$lgt_current_object_'(Ctg, _, _, _, _, _, _, _, _, _, _)),
		\+ '$lgt_pp_file_relation_clause_'('$lgt_current_protocol_'(Ctg, _, _, _, _))
	;	'$lgt_pp_file_relation_clause_'('$lgt_current_category_'(Ctg, _, _, _, _, _)) ->
		% category defined previously in the same file
		true
	;	fail
	).



% '$lgt_report_problems'(+atom, +entity_identifier)
%
% reports any potential problem found while compiling an entity 

'$lgt_report_problems'(Type, Entity) :-
	(	'$lgt_compiler_flag'(report, off) ->
		true
	;	Type == protocol ->
		'$lgt_report_unknown_entities'(Type, Entity)
	;	'$lgt_report_undefined_predicate_calls'(Type, Entity),
		'$lgt_report_missing_dynamic_directives'(Type, Entity),
		'$lgt_report_missing_discontiguous_directives'(Type, Entity),
		'$lgt_report_misspelt_calls'(Type, Entity),
		'$lgt_report_non_portable_calls'(Type, Entity),
		'$lgt_report_non_portable_functions'(Type, Entity),
		'$lgt_report_unknown_entities'(Type, Entity)
	).



% '$lgt_report_warning_entity_context'(+atom, +entity_identifier)
%
% reports warning entity context when no line information is available 

'$lgt_report_warning_entity_context'(Type, Entity) :-
	(	'$lgt_compiler_flag'(report, warnings) ->
		'$lgt_pp_file_path_'(File, _),
		write('%                   in '), write(Type), write(' '), writeq(Entity),
		write(', defined in file '), write(File), nl,
		write('%')
	;	true
	).



% '$lgt_report_warning_file_context'
%
% reports warning file context when no entity information is available 

'$lgt_report_warning_file_context' :-
	stream_property(Input, alias('$lgt_input')),
	!,	% avoid a spurious choice-point with some Prolog compilers
	(	'$lgt_compiler_flag'(report, warnings) ->
		'$lgt_pp_file_path_'(File, _),
		write('%                   in file '), write(File),
		'$lgt_report_warning_line_number'(Input),
		write('%')
	;	'$lgt_compiler_flag'(report, on) ->
		'$lgt_report_warning_line_number'(Input)
	;	true
	).



% '$lgt_report_warning_full_context'(+atom, +entity_identifier)
%
% reports warning full context

'$lgt_report_warning_full_context'(Type, Entity) :-
	'$lgt_pp_file_path_'(File, _),
	stream_property(Input, alias('$lgt_input')),
	!,	% avoid a spurious choice-point with some Prolog compilers
	(	'$lgt_compiler_flag'(report, warnings) ->
		write('%                   in '), write(Type), write(' '), writeq(Entity),
		write(', defined in file '), write(File),
		'$lgt_report_warning_line_number'(Input),
		write('%')
	;	'$lgt_compiler_flag'(report, on) ->
		'$lgt_report_warning_line_number'(Input)
	;	true
	).



% '$lgt_report_warning_line_number'(@stream)
%
% reports warning line number (if possible) 

'$lgt_report_warning_line_number'(Input) :-
	(	'$lgt_compiler_flag'(report, warnings) ->
		(	'$lgt_pp_term_position_'(Lines) ->
			write(', in lines '), write(Lines), nl
		;	(	catch('$lgt_stream_current_line_number'(Input, Next), _, fail) ->
				write(', above line '), write(Next), nl
			;	nl
			)
		)
	;	'$lgt_compiler_flag'(report, on) ->
		(	'$lgt_pp_term_position_'(Lines) ->
			write('%                   in lines '), write(Lines), nl
		;	(	catch('$lgt_stream_current_line_number'(Input, Next), _, fail) ->
				write('%                   above line '), write(Next), nl
			;	nl
			)
		)
	;	true
	).



% '$lgt_report_warning_in_new_line'
%
% reports warning full context 

'$lgt_report_warning_in_new_line' :-
	(	'$lgt_compiler_flag'(report, warnings) ->
		% in "warnings" mode, only warnings are printed, one per line
		nl
	;	'$lgt_pp_entity_warnings_flag_' ->
		% not the first entity warning so we're already in a new line
		true
	;	% in "on" mode, change line before printing the first warning
		nl
	).



% '$lgt_report_unknown_entities'(+atom, +entity_identifier)
%
% reports any unknown referenced entities found while compiling an entity

'$lgt_report_unknown_entities'(Type, Entity) :-
	(	'$lgt_compiler_flag'(unknown, warning) ->
		'$lgt_report_unknown_objects'(Type, Entity),
		'$lgt_report_unknown_protocols'(Type, Entity),
		'$lgt_report_unknown_categories'(Type, Entity)
	;	true
	).



% '$lgt_report_unknown_objects'(+atom, +entity_identifier)
%
% reports any unknown referenced objects found while compiling an entity

'$lgt_report_unknown_objects'(Type, Entity) :-
	(	setof(Obj, '$lgt_unknown_object'(Obj), Objs) ->
		'$lgt_report_warning_in_new_line',
		'$lgt_inc_compile_warnings_counter',
		(	Objs = [_] ->
			write('%         WARNING!  Reference to unknown object: ')
		;	write('%         WARNING!  References to unknown objects: '), nl,
			write('%                       ')
		),
		'$lgt_writeq_list'(Objs), nl,
		'$lgt_report_warning_entity_context'(Type, Entity)
	;	true
	).


'$lgt_unknown_object'(Obj) :-
	'$lgt_pp_referenced_object_'(Obj),
	\+ '$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, _),	% not a currently loaded object
	\+ '$lgt_pp_object_'(Obj, _, _, _, _, _, _, _, _, _, _),		% not the object being compiled (self reference)
	\+ '$lgt_pp_entity_init_'(object, Obj, _),						% not an object defined in the source file being compiled
	\+ '$lgt_pp_file_relation_clause_'('$lgt_current_object_'(Obj, _, _, _, _, _, _, _, _, _, _)),
	\+ (atom(Obj), catch(current_module(Obj), _, fail)).			% not a currently loaded module; use catch/3 to avoid 
																	% errors with Prolog compilers with no module support


% '$lgt_report_unknown_protocols'(+atom, +entity_identifier)
%
% reports any unknown referenced protocols found while compiling an entity

'$lgt_report_unknown_protocols'(Type, Entity) :-
	(	setof(Ptc, '$lgt_unknown_protocol'(Ptc), Ptcs) ->
		'$lgt_report_warning_in_new_line',
		'$lgt_inc_compile_warnings_counter',
		(	Ptcs = [_] ->
			write('%         WARNING!  Reference to unknown protocol: ')
		;	write('%         WARNING!  References to unknown protocols: '), nl,
			write('%                       ')
		),
		'$lgt_writeq_list'(Ptcs), nl,
		'$lgt_report_warning_entity_context'(Type, Entity)
	;	true
	).


'$lgt_unknown_protocol'(Ptc) :-
	'$lgt_pp_referenced_protocol_'(Ptc),
	\+ '$lgt_current_protocol_'(Ptc, _, _, _, _),	% not a currently loaded protocol
	\+ '$lgt_pp_protocol_'(Ptc, _, _, _, _),		% not the protocol being compiled (self reference)
	\+ '$lgt_pp_entity_init_'(protocol, Ptc, _),	% not a protocol defined in the source file being compiled
	\+ '$lgt_pp_file_relation_clause_'('$lgt_current_protocol_'(Ptc, _, _, _, _)).



% '$lgt_report_unknown_categories'(+atom, +entity_identifier)
%
% reports any unknown referenced categories found while compiling an entity

'$lgt_report_unknown_categories'(Type, Entity) :-
	(	setof(Ctg, '$lgt_unknown_category'(Ctg), Ctgs) ->
		'$lgt_report_warning_in_new_line',
		'$lgt_inc_compile_warnings_counter',
		(	Ctgs = [_] ->
			write('%         WARNING!  Reference to unknown category: ')
		;	write('%         WARNING!  References to unknown categories: '), nl,
			write('%                       ')
		),
		'$lgt_writeq_list'(Ctgs), nl,
		'$lgt_report_warning_entity_context'(Type, Entity)
	;	true
	).


'$lgt_unknown_category'(Ctg) :-
	'$lgt_pp_referenced_category_'(Ctg),
	\+ '$lgt_current_category_'(Ctg, _, _, _, _, _),	% not a currently loaded category
	\+ '$lgt_pp_category_'(Ctg, _, _, _, _, _),			% not the category being compiled (self reference)
	\+ '$lgt_pp_entity_init_'(category, Ctg, _),		% not a category defined in the source file being compiled
	\+ '$lgt_pp_file_relation_clause_'('$lgt_current_category_'(Ctg, _, _, _, _, _)).



% '$lgt_pp_term_location'(-compound)
%
% returns the location of the last source file term read;
% returns the atom "none" if the location information is not available

'$lgt_pp_term_location'(Location) :-
	(	'$lgt_pp_term_position_'(Line-_),
		'$lgt_pp_file_path_'(File, Path) ->
		Location = Path+File+Line
	;	'$lgt_pp_file_path_'(File, Path) ->
		Location = Path+File+1
	;	Location = none
	).



% '$lgt_writeq_list'(+list)
%
% auxiliary predicate for writing a non-empty list of elements (quoted)

'$lgt_writeq_list'([Term]) :-
	writeq(Term), !.

'$lgt_writeq_list'([Term1, Term2| Terms]) :-
	writeq(Term1), write(', '),
	'$lgt_writeq_list'([Term2| Terms]).



% '$lgt_write_list'(+list)
%
% auxiliary predicate for writing a non-empty list of elements (non-quoted)

'$lgt_write_list'([Term]) :-
	write(Term), !.

'$lgt_write_list'([Term1, Term2| Terms]) :-
	write(Term1), write(', '),
	'$lgt_write_list'([Term2| Terms]).



% '$lgt_add_def_clause'(+callable, -callable, +atom, +integer, +nonvar)
%
% adds a "def clause" (used to translate a predicate call) and returns
% the translated clause head

'$lgt_add_def_clause'(Head, Functor, Arity, HeadDef, Ctx) :-
	functor(HeadTemplate, Functor, Arity),
	'$lgt_comp_ctx'(Ctx, _, _, _, _, Prefix, _, _, ExCtx, _, _),
	'$lgt_construct_predicate_indicator'(Prefix, Functor/Arity, TFunctor/TArity),	
	functor(HeadDefTemplate, TFunctor, TArity),
	'$lgt_unify_head_thead_args'(Arity, HeadTemplate, HeadDefTemplate),
	arg(TArity, HeadDefTemplate, ExCtxTemplate),
	once((  '$lgt_pp_object_'(_, _, _, Def, _, _, _, _, _, _, _)
		;   '$lgt_pp_category_'(_, _, _, Def, _, _)
	)),
	functor(Clause, Def, 3),
	arg(1, Clause, HeadTemplate),
	arg(2, Clause, ExCtxTemplate),
	arg(3, Clause, HeadDefTemplate),
	(	'$lgt_pp_def_'(Clause) ->
		true
	;	assertz('$lgt_pp_def_'(Clause))
	),
	(	'$lgt_built_in'(Head) ->
		(	'$lgt_pp_redefined_built_in_'(HeadTemplate, _, _) ->
			true
		;	assertz('$lgt_pp_redefined_built_in_'(HeadTemplate, ExCtxTemplate, HeadDefTemplate))
		)
	;	true
	),
	Head = HeadTemplate,
	ExCtx = ExCtxTemplate,
	HeadDef = HeadDefTemplate,
	'$lgt_remember_predicate'(Functor, Arity, Ctx).



% '$lgt_add_ddef_clause'(+callable, +atom, +integer, -callable, +nonvar)
%
% adds a "ddef clause" (used to translate a predicate call) and returns
% the translated clause head

'$lgt_add_ddef_clause'(Head, Functor, Arity, HeadDef, Ctx) :-
	functor(HeadTemplate, Functor, Arity),
	'$lgt_comp_ctx'(Ctx, _, _, _, _, Prefix, _, _, ExCtx, _, _),
	'$lgt_construct_predicate_indicator'(Prefix, Functor/Arity, TFunctor/TArity),	
	functor(HeadDefTemplate, TFunctor, TArity),
	'$lgt_unify_head_thead_args'(Arity, HeadTemplate, HeadDefTemplate),
	arg(TArity, HeadDefTemplate, ExCtxTemplate),
	once('$lgt_pp_object_'(_, _, _, _, _, _, _, _, DDef, _, _)),
	functor(Clause, DDef, 3),
	arg(1, Clause, HeadTemplate),
	arg(2, Clause, ExCtxTemplate),
	arg(3, Clause, HeadDefTemplate),
	(	'$lgt_pp_ddef_'(Clause) ->
		true
	;	assertz('$lgt_pp_ddef_'(Clause))
	),
	(	'$lgt_built_in'(Head) ->
		(	'$lgt_pp_redefined_built_in_'(HeadTemplate, _, _) ->
			true
		;	assertz('$lgt_pp_redefined_built_in_'(HeadTemplate, ExCtxTemplate, HeadDefTemplate))
		)
	;	true
	),
	Head = HeadTemplate,
	ExCtx = ExCtxTemplate,
	HeadDef = HeadDefTemplate,
	'$lgt_remember_predicate'(Functor, Arity, Ctx).


% is necessary to remember which predicates are defined in order to deal with 
% redefinition of built-in predicates and detect missing predicate directives
%
% the check for discontiguous predicates is not performed when compiling clauses
% for auxiliary predicates (using the logtalk::compile_aux_clauses/1 hook predicate)

'$lgt_remember_predicate'(Functor, Arity, Ctx) :-
	(	'$lgt_pp_defines_predicate_'(Functor, Arity) ->
		% not the first clause for the predicate
		(	'$lgt_pp_previous_predicate_'(PreviousFunctor, PreviousArity),
			PreviousFunctor/PreviousArity \== Functor/Arity,
			\+ '$lgt_comp_ctx_mode'(Ctx, compile(aux)) ->
			% clauses for the predicate are discontiguous
			'$lgt_check_discontiguous_directive'(Functor, Arity)
		;	% more clauses for the same predicate
			true
		)
	;	% first clause for this predicate; remember it
		asserta('$lgt_pp_defines_predicate_'(Functor, Arity))
	),
	(	'$lgt_comp_ctx_mode'(Ctx, compile(aux)) ->
		true
	;	retractall('$lgt_pp_previous_predicate_'(_, _)),
		asserta('$lgt_pp_previous_predicate_'(Functor, Arity))
	).



% '$lgt_update_ddef_table'(+atom, @callable, @callable)
%
% retracts a dynamic "ddef clause" (used to translate a predicate call)
% if there are no more clauses for the predicate otherwise does nothing
%
% this is needed in order to allow definitions in ancestors to be found

'$lgt_update_ddef_table'(DDef, Head, THead) :-
	'$lgt_term_template'(THead, GTHead),
	(	clause(GTHead, _) ->
		true
	;	functor(DDefClause, DDef, 3),
		arg(1, DDefClause, Head),
		retractall(DDefClause),
		'$lgt_clean_lookup_caches'(Head)
	).



% '$lgt_update_ddef_table_opt'(+callable)
%
% retracts a dynamic "ddef clause" (used to translate a predicate call)
% if there are no more clauses for the predicate otherwise does nothing
%
% this is needed in order to allow definitions in ancestors to be found

'$lgt_update_ddef_table_opt'(UClause) :-
	(	UClause == true ->
		true
	;	arg(3, UClause, THead),
		clause(THead, _) ->
		true
	;	retractall(UClause),
		arg(1, UClause, Head),
		'$lgt_clean_lookup_caches'(Head)
	).



% '$lgt_generate_code'(+atom)
%
% generates code for the entity being compiled

'$lgt_generate_code'(protocol) :-
	'$lgt_fix_predicate_calls',		% necessary because of possible initialization goal
	'$lgt_gen_protocol_clauses',
	'$lgt_gen_protocol_directives',
	'$lgt_gen_entity_init_goal'.

'$lgt_generate_code'(object) :-
	'$lgt_gen_local_def_clauses',
	'$lgt_fix_synchronized_predicates',
	'$lgt_fix_predicate_calls',
	'$lgt_gen_object_clauses',
	'$lgt_gen_object_directives',
	'$lgt_gen_entity_init_goal'.

'$lgt_generate_code'(category) :-
	'$lgt_gen_local_def_clauses',
	'$lgt_fix_synchronized_predicates',
	'$lgt_fix_predicate_calls',
	'$lgt_gen_category_clauses',
	'$lgt_gen_category_directives',
	'$lgt_gen_entity_init_goal'.



'$lgt_gen_object_directives' :-
	'$lgt_gen_object_dynamic_directives',
	'$lgt_gen_object_discontiguous_directives'.



'$lgt_gen_category_directives' :-
	'$lgt_gen_category_dynamic_directives',
	'$lgt_gen_category_discontiguous_directives'.



'$lgt_gen_protocol_directives' :-
	(	'$lgt_pp_protocol_'(_, Prefix, Dcl, Rnm, Flags), Flags /\ 2 =:= 2 ->
		assertz('$lgt_pp_directive_'(dynamic(Prefix/2))),
		assertz('$lgt_pp_directive_'(dynamic(Dcl/4))),
		assertz('$lgt_pp_directive_'(dynamic(Dcl/5))),
		assertz('$lgt_pp_directive_'(dynamic(Rnm/3)))
	;	true
	).



'$lgt_gen_object_dynamic_directives' :-
	(	'$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, _, Flags),	Flags /\ 2 =:= 2 ->
		'$lgt_gen_dynamic_object_dynamic_directives'
	;	'$lgt_gen_static_object_dynamic_directives'
	).



'$lgt_gen_dynamic_object_dynamic_directives' :-
	'$lgt_pp_object_'(_, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm, _),
	assertz('$lgt_pp_directive_'(dynamic(Prefix/8))),
	assertz('$lgt_pp_directive_'(dynamic(Dcl/4))),
	assertz('$lgt_pp_directive_'(dynamic(Dcl/6))),
	assertz('$lgt_pp_directive_'(dynamic(Def/3))),
	assertz('$lgt_pp_directive_'(dynamic(Def/4))),
	assertz('$lgt_pp_directive_'(dynamic(Super/4))),
	assertz('$lgt_pp_directive_'(dynamic(IDcl/6))),
	assertz('$lgt_pp_directive_'(dynamic(IDef/4))),
	(	'$lgt_compiler_flag'(dynamic_declarations, allow) ->
		assertz('$lgt_pp_directive_'(dynamic(DDcl/2)))
	;	true
	),
	assertz('$lgt_pp_directive_'(dynamic(DDef/3))),
	assertz('$lgt_pp_directive_'(dynamic(Rnm/3))),
	'$lgt_gen_dynamic_entity_dynamic_predicate_directives'.


'$lgt_gen_dynamic_entity_dynamic_predicate_directives' :-
	'$lgt_pp_final_def_'(Clause),
	Clause \= (_ :- _),		% only local table; reject linking clauses
	arg(3, Clause, Call),
	functor(Call, Functor, Arity),
	assertz('$lgt_pp_directive_'(dynamic(Functor/Arity))),
	fail.

'$lgt_gen_dynamic_entity_dynamic_predicate_directives'.



'$lgt_gen_static_object_dynamic_directives' :-
	'$lgt_pp_object_'(_, Prefix, _, _, _, _, _, DDcl, DDef, _, _),
	(	'$lgt_compiler_flag'(dynamic_declarations, allow) ->
		assertz('$lgt_pp_directive_'(dynamic(DDcl/2)))
	;	true
	),
	assertz('$lgt_pp_directive_'(dynamic(DDef/3))),
	'$lgt_pp_dynamic_'(Functor, Arity),
		'$lgt_construct_predicate_indicator'(Prefix, Functor/Arity, TFunctor/TArity),
		assertz('$lgt_pp_directive_'(dynamic(TFunctor/TArity))),
	fail.

'$lgt_gen_static_object_dynamic_directives'.



'$lgt_gen_object_discontiguous_directives' :-
	'$lgt_pp_object_'(_, Prefix, _, _, _, _, _, _, _, _, _),
	'$lgt_pp_discontiguous_'(Functor, Arity),
		'$lgt_construct_predicate_indicator'(Prefix, Functor/Arity, TFunctor/TArity),
		assertz('$lgt_pp_directive_'(discontiguous(TFunctor/TArity))),
	fail.

'$lgt_gen_object_discontiguous_directives'.



'$lgt_gen_category_dynamic_directives' :-
	(	'$lgt_pp_category_'(_, Prefix, Dcl, Def, Rnm, Flags), Flags /\ 2 =:= 2 ->
		assertz('$lgt_pp_directive_'(dynamic(Prefix/3))),
		assertz('$lgt_pp_directive_'(dynamic(Dcl/4))),
		assertz('$lgt_pp_directive_'(dynamic(Dcl/5))),
		assertz('$lgt_pp_directive_'(dynamic(Def/3))),
		assertz('$lgt_pp_directive_'(dynamic(Rnm/3))),
		'$lgt_gen_dynamic_entity_dynamic_predicate_directives'
	;	true
	).



'$lgt_gen_category_discontiguous_directives' :-
	'$lgt_pp_category_'(_, Prefix, _, _, _, _),
	'$lgt_pp_discontiguous_'(Functor, Arity),
		'$lgt_construct_predicate_indicator'(Prefix, Functor/Arity, TFunctor/TArity),
		assertz('$lgt_pp_directive_'(discontiguous(TFunctor/TArity))),
	fail.

'$lgt_gen_category_discontiguous_directives'.



'$lgt_gen_object_clauses' :-
	(	\+ '$lgt_pp_instantiated_class_'(_, _, _, _, _, _, _, _, _, _),
		\+ '$lgt_pp_specialized_class_'(_, _, _, _, _, _, _, _, _, _) ->
		'$lgt_gen_prototype_clauses'
	;	'$lgt_gen_ic_clauses'
	).



% '$lgt_gen_local_dcl_clauses'(-atom)
%
% a (local) predicate declaration is only generated if there is a scope 
% declaration for the predicate; the single argument returns the atom
% "true" if there are local declaration clauses and the atom "false" otherwise

'$lgt_gen_local_dcl_clauses'(_) :-
	'$lgt_pp_entity'(_, _, _, Dcl, Mode),
	(	'$lgt_pp_public_'(Functor, Arity), Scope = p(p(p))
	;	'$lgt_pp_protected_'(Functor, Arity), Scope = p(p)
	;	'$lgt_pp_private_'(Functor, Arity), Scope = p
	),
	functor(Pred, Functor, Arity),
	functor(Template, Functor, Arity),
	(	'$lgt_pp_meta_predicate_'(Template) ->
		Meta = Template
	;	Meta = no
	),
	(	'$lgt_pp_coinductive_'(Pred, _) ->
		Coinductive = 32				% 0b00100000
	;	Coinductive = 0
	),
	(	'$lgt_pp_multifile_'(Functor, Arity) ->
		Multifile = 16					% 0b00010000
	;	Multifile = 0
	),
	(	'$lgt_pp_non_terminal_'(Functor, _, Arity) ->
		NonTerminal = 8					% 0b00001000
	;	NonTerminal = 0
	),
	(	'$lgt_pp_synchronized_'(Pred, _) ->
		Synchronized = 4				% 0b00000100
	;	Synchronized = 0
	),
	(	(Mode == (dynamic); '$lgt_pp_dynamic_'(Functor, Arity)) ->
		Dynamic = 2						% 0b00000010
	;	Dynamic = 0
	),
	Flags is Coinductive + Multifile + NonTerminal + Synchronized + Dynamic,
	Fact =.. [Dcl, Pred, Scope, Meta, Flags],
	assertz('$lgt_pp_dcl_'(Fact)),
	fail.

'$lgt_gen_local_dcl_clauses'(Local) :-
	(	'$lgt_pp_dcl_'(_) ->
		Local = true
	;	Local = false
	).



% '$lgt_gen_local_def_clauses'
%
% generates local def clauses for undefined but declared (using scope or dynamic
% directives) predicates

'$lgt_gen_local_def_clauses' :-
	% categories cannot contain clauses for dynamic predicates:
	'$lgt_pp_entity'(object, _, Prefix, _, _),
	'$lgt_comp_ctx_prefix'(Ctx, Prefix),
	'$lgt_pp_dynamic_'(Functor, Arity),
	\+ '$lgt_pp_defines_predicate_'(Functor, Arity),
	functor(Head, Functor, Arity),
	(	\+ '$lgt_pp_public_'(Functor, Arity),
		\+ '$lgt_pp_protected_'(Functor, Arity),
		\+ '$lgt_pp_private_'(Functor, Arity) ->
		'$lgt_add_ddef_clause'(Head, Functor, Arity, _, Ctx)
	;	'$lgt_add_def_clause'(Head, Functor, Arity, _, Ctx)
	),
	fail.

'$lgt_gen_local_def_clauses' :-
	% annotations *may* result in the definition of predicates:
	once((	'$lgt_pp_value_annotation_'(_, _, _, _)
		 ;	'$lgt_pp_goal_annotation_'(_, _, _, _)
	)),
	'$lgt_pp_entity'(_, _, Prefix, _, _),
	'$lgt_comp_ctx_prefix'(Ctx, Prefix),
	'$lgt_pp_calls_predicate_'(Functor, Arity, _, _),
	\+ '$lgt_pp_defines_predicate_'(Functor, Arity),
	once((	'$lgt_pp_public_'(Functor, Arity)
		 ;	'$lgt_pp_protected_'(Functor, Arity)
		 ;	'$lgt_pp_private_'(Functor, Arity)
	)),
	functor(Head, Functor, Arity),
	'$lgt_add_def_clause'(Head, Functor, Arity, _, Ctx),
	fail.

'$lgt_gen_local_def_clauses'.



'$lgt_gen_protocol_clauses' :-
	'$lgt_gen_local_dcl_clauses'(Local),
	'$lgt_gen_protocol_linking_clauses'(Local),
	'$lgt_gen_protocol_extend_clauses',
	'$lgt_gen_protocol_catchall_clauses'.



'$lgt_gen_protocol_linking_clauses'(true) :-
	'$lgt_pp_protocol_'(Ptc, _, PDcl, _, _),
	Head =.. [PDcl, Pred, Scope, Meta, Flags, Ptc],
	Body =.. [PDcl, Pred, Scope, Meta, Flags],
	assertz('$lgt_pp_dcl_'((Head:-Body))).

'$lgt_gen_protocol_linking_clauses'(false).



'$lgt_gen_protocol_extend_clauses' :-
	'$lgt_pp_protocol_'(_, _, PDcl1, Rnm, _),
	'$lgt_pp_extended_protocol_'(Ptc2, _, PDcl2, EntityScope),
	(	EntityScope == (public) ->
		Lookup =.. [PDcl2, Pred, Scope, Meta, Flags, Ctn]
	;	EntityScope == protected ->
		Call =.. [PDcl2, Pred, Scope2, Meta, Flags, Ctn],
		Lookup = (Call, '$lgt_filter_scope'(Scope2, Scope))
	;	Scope = p,
		Lookup =.. [PDcl2, Pred, _, Meta, Flags, Ctn]
	),
	(	'$lgt_pp_predicate_alias_'(Ptc2, _, _) ->
		Head =.. [PDcl1, Alias, Scope, Meta, Flags, Ctn],
		Rename =.. [Rnm, Ptc2, Pred, Alias],
		assertz('$lgt_pp_dcl_'((Head :- Rename, Lookup)))
	;	Head =.. [PDcl1, Pred, Scope, Meta, Flags, Ctn],
		assertz('$lgt_pp_dcl_'((Head:-Lookup)))
	),
	fail.

'$lgt_gen_protocol_extend_clauses'.



% when a static protocol is empty, i.e. when it does not contain any predicate
% declarations, and does not extend other protocols, we need a catchall clause
% in order to prevent predicate existence errors when sending a message to an
% object implementing (directly or indirectly) the protocol

'$lgt_gen_protocol_catchall_clauses' :-
	(	'$lgt_pp_dcl_'(_) ->
		true
	;	% empty, standalone protocol 
		'$lgt_pp_protocol_'(_, _, Dcl, _, Flags),
		(	Flags /\ 2 =:= 2 ->
			% dynamic protocol
			true
		;	% generate a catchall clause for static protocols
			Head =.. [Dcl, _, _, _, _, _],
			assertz('$lgt_pp_dcl_'((Head:-fail)))
		)
	).



'$lgt_gen_category_clauses' :-
	'$lgt_gen_category_dcl_clauses',
	'$lgt_gen_category_def_clauses'.



'$lgt_gen_category_dcl_clauses' :-
	'$lgt_gen_local_dcl_clauses'(Local),
	'$lgt_gen_category_linking_dcl_clauses'(Local),
	'$lgt_gen_category_implements_dcl_clauses',
	'$lgt_gen_category_extends_dcl_clauses',
	'$lgt_gen_category_catchall_dcl_clauses'.



'$lgt_gen_category_linking_dcl_clauses'(true) :-
	'$lgt_pp_category_'(Ctg, _, CDcl, _, _, _),
	Head =.. [CDcl, Pred, Scope, Meta, Flags, Ctg],
	Body =.. [CDcl, Pred, Scope, Meta, Flags],
	assertz('$lgt_pp_dcl_'((Head:-Body))).

'$lgt_gen_category_linking_dcl_clauses'(false).



'$lgt_gen_category_implements_dcl_clauses' :-
	'$lgt_pp_category_'(_, _, CDcl, _, Rnm, _),
	'$lgt_pp_implemented_protocol_'(Ptc, _, PDcl, EntityScope),
	(	EntityScope == (public) ->
		Lookup =.. [PDcl, Pred, Scope, Meta, Flags, Ctn]
	;	EntityScope == protected ->
		Call =.. [PDcl, Pred, Scope2, Meta, Flags, Ctn],
		Lookup = (Call, '$lgt_filter_scope'(Scope2, Scope))
	;	Scope = p,
		Lookup =.. [PDcl, Pred, _, Meta, Flags, Ctn]
	),
	(	'$lgt_pp_predicate_alias_'(Ptc, _, _) ->
		Head =.. [CDcl, Alias, Scope, Meta, Flags, Ctn],
		Rename =.. [Rnm, Ptc, Pred, Alias],
		assertz('$lgt_pp_dcl_'((Head :- Rename, Lookup)))
	;	Head =.. [CDcl, Pred, Scope, Meta, Flags, Ctn],
		assertz('$lgt_pp_dcl_'((Head:-Lookup)))
	),
	fail.

'$lgt_gen_category_implements_dcl_clauses'.



'$lgt_gen_category_extends_dcl_clauses' :-
	'$lgt_pp_category_'(_, _, CDcl, _, Rnm, _),
	'$lgt_pp_extended_category_'(Ctg, _, ECDcl, _, EntityScope),
	(	EntityScope == (public) ->
		Lookup =.. [ECDcl, Pred, Scope, Meta, Flags, Ctn]
	;	EntityScope == protected ->
		Call =.. [ECDcl, Pred, Scope2, Meta, Flags, Ctn],
		Lookup = (Call, '$lgt_filter_scope'(Scope2, Scope))
	;	Scope = p,
		Lookup =.. [ECDcl, Pred, _, Meta, Flags, Ctn]
	),
	(	'$lgt_pp_predicate_alias_'(Ctg, _, _) ->
		Head =.. [CDcl, Alias, Scope, Meta, Flags, Ctn],
		Rename =.. [Rnm, Ctg, Pred, Alias],
		assertz('$lgt_pp_dcl_'((Head :- Rename, Lookup)))
	;	Head =.. [CDcl, Pred, Scope, Meta, Flags, Ctn],
		assertz('$lgt_pp_dcl_'((Head:-Lookup)))
	),
	fail.

'$lgt_gen_category_extends_dcl_clauses'.



% when a static category contains no predicate declarations, does not implement any
% protocol, and does not extend other categories, we need a catchall clause in order
% to prevent predicate existence errors when sending a message to an object importing
% (directly or indirectly) the category

'$lgt_gen_category_catchall_dcl_clauses' :-
	(	'$lgt_pp_dcl_'(_) ->
		true
	;	% standalone category with no local or inherited predicate declarations
		'$lgt_pp_category_'(_, _, Dcl, _, _, Flags),
		(	Flags /\ 2 =:= 2 ->
			% dynamic category
			true
		;	% generate a catchall clause for static categories
			Head =.. [Dcl, _, _, _, _, _],
			assertz('$lgt_pp_dcl_'((Head:-fail)))
		)
	).



'$lgt_gen_category_def_clauses' :-
	'$lgt_gen_category_linking_def_clauses',
	'$lgt_gen_category_extends_def_clauses'.



'$lgt_gen_category_linking_def_clauses' :-
	'$lgt_pp_category_'(Ctg, _, _, Def, _, _),
	Head =.. [Def, Pred, ExCtx, Call, Ctg],
	(	'$lgt_pp_final_def_'(_) ->
		Body =.. [Def, Pred, ExCtx, Call]
	;	Body = fail
	),
	assertz('$lgt_pp_final_def_'((Head:-Body))).



'$lgt_gen_category_extends_def_clauses' :-
	'$lgt_pp_category_'(Ctg, _, _, Def, Rnm, _),
	'$lgt_pp_relation_clause_'('$lgt_extends_category_'(Ctg, Ctg2, _)),		% necessary for parameter passing
	'$lgt_pp_extended_category_'(Ctg2, _, _, Def2, _),
	Lookup =.. [Def2, Pred, ExCtx, Call, Ctn],
	(	'$lgt_pp_predicate_alias_'(Ctg2, _, _) ->
		Head =.. [Def, Alias, ExCtx, Call, Ctn],
		Rename =.. [Rnm, Ctg2, Pred, Alias],
		assertz('$lgt_pp_final_def_'((Head :- Rename, Lookup)))
	;	Head =.. [Def, Pred, ExCtx, Call, Ctn],
		assertz('$lgt_pp_final_def_'((Head:-Lookup)))
	),
	fail.

'$lgt_gen_category_extends_def_clauses'.



% the database built-in methods need to check if a local declaration or a local definition 
% exists for a predicate; in order to avoid predicate existence errors, we need to generate 
% catchall clauses for static when there are no local predicate declarations or no local
% predicate definitions

'$lgt_gen_object_catchall_dcl_clauses'(true).

'$lgt_gen_object_catchall_dcl_clauses'(false) :-
	'$lgt_pp_object_'(_, _, Dcl, _, _, _, _, _, _, _, Flags),
	(	Flags /\ 2 =:= 2 ->
		% dynamic object
		true
	;	% generate a catchall clause for static objects
		Head =.. [Dcl, _, _, _, _],			
		assertz('$lgt_pp_dcl_'((Head:-fail)))
	).



'$lgt_gen_object_catchall_def_clauses'(true).

'$lgt_gen_object_catchall_def_clauses'(false) :-
	'$lgt_pp_object_'(_, _, _, Def, _, _, _, _, _, _, Flags),
	(	Flags /\ 2 =:= 2 ->
		% dynamic object
		true
	;	% generate a catchall clause for static objects
		Head =.. [Def, _, _, _],
		assertz('$lgt_pp_final_def_'((Head:-fail)))
	).



'$lgt_gen_prototype_clauses' :-
	'$lgt_gen_prototype_dcl_clauses',
	'$lgt_gen_prototype_def_clauses',
	'$lgt_gen_prototype_super_clauses'.



'$lgt_gen_prototype_dcl_clauses' :-
	'$lgt_gen_local_dcl_clauses'(Local),
	'$lgt_gen_prototype_linking_dcl_clauses'(Local),
	'$lgt_gen_prototype_implements_dcl_clauses',
	'$lgt_gen_prototype_imports_dcl_clauses',
	'$lgt_gen_prototype_extends_dcl_clauses',
	'$lgt_gen_object_catchall_dcl_clauses'(Local).



'$lgt_gen_prototype_linking_dcl_clauses'(true) :-
	'$lgt_pp_object_'(Obj, _, Dcl, _, _, _, _, DDcl, _, _, _),
	HeadDcl =.. [Dcl, Pred, Scope, Meta, Flags, Obj, Obj],
	BodyDcl =.. [Dcl, Pred, Scope, Meta, Flags],
	assertz('$lgt_pp_dcl_'((HeadDcl:-BodyDcl))),
	(	'$lgt_compiler_flag'(dynamic_declarations, allow) ->
		HeadDDcl =.. [Dcl, Pred, Scope, no, 2, Obj, Obj],
		BodyDDcl =.. [DDcl, Pred, Scope],
		assertz('$lgt_pp_dcl_'((HeadDDcl:-BodyDDcl)))
	;	true
	).

'$lgt_gen_prototype_linking_dcl_clauses'(false) :-
	'$lgt_pp_object_'(Obj, _, Dcl, _, _, _, _, DDcl, _, _, _),
	(	'$lgt_compiler_flag'(dynamic_declarations, allow) ->
		HeadDDcl =.. [Dcl, Pred, Scope, no, 2, Obj, Obj],
		BodyDDcl =.. [DDcl, Pred, Scope],
		assertz('$lgt_pp_dcl_'((HeadDDcl:-BodyDDcl)))
	;	\+ '$lgt_pp_implemented_protocol_'(_, _, _, _),
		\+ '$lgt_pp_imported_category_'(_, _, _, _, _),
		\+ '$lgt_pp_extended_object_'(_, _, _, _, _, _, _, _, _, _) ->
		HeadDDcl =.. [Dcl, _, _, _, _, _, _],
		assertz('$lgt_pp_dcl_'((HeadDDcl:-fail)))
	;	true
	).



'$lgt_gen_prototype_implements_dcl_clauses' :-
	'$lgt_pp_object_'(Obj, _, ODcl, _, _, _, _, _, _, Rnm, _),
	'$lgt_pp_implemented_protocol_'(Ptc, _, PDcl, EntityScope),
	(	EntityScope == (public) ->
		Lookup =.. [PDcl, Pred, Scope, Meta, Flags, Ctn]
	;	EntityScope == protected ->
		Call =.. [PDcl, Pred, Scope2, Meta, Flags, Ctn],
		Lookup = (Call, '$lgt_filter_scope'(Scope2, Scope))
	;	Scope = p,
		Lookup =.. [PDcl, Pred, _, Meta, Flags, Ctn]
	),
	(	'$lgt_pp_predicate_alias_'(Ptc, _, _) ->
		Head =.. [ODcl, Alias, Scope, Meta, Flags, Obj, Ctn],
		Rename =.. [Rnm, Ptc, Pred, Alias],
		assertz('$lgt_pp_dcl_'((Head :- Rename, Lookup)))
	;	Head =.. [ODcl, Pred, Scope, Meta, Flags, Obj, Ctn],
		assertz('$lgt_pp_dcl_'((Head:-Lookup)))
	),
	fail.

'$lgt_gen_prototype_implements_dcl_clauses'.



'$lgt_gen_prototype_imports_dcl_clauses' :-
	'$lgt_pp_object_'(Obj, _, ODcl, _, _, _, _, _, _, Rnm, _),
	'$lgt_pp_imported_category_'(Ctg, _, CDcl, _, EntityScope),
	(	EntityScope == (public) ->
		Lookup =.. [CDcl, Pred, Scope, Meta, Flags, Ctn]
	;	EntityScope == protected ->
		Call =.. [CDcl, Pred, Scope2, Meta, Flags, Ctn],
		Lookup = (Call, '$lgt_filter_scope'(Scope2, Scope))
	;	Scope = p,
		Lookup =.. [CDcl, Pred, _, Meta, Flags, Ctn]
	),
	(	'$lgt_pp_predicate_alias_'(Ctg, _, _) ->
		Head =.. [ODcl, Alias, Scope, Meta, Flags, Obj, Ctn],
		Rename =.. [Rnm, Ctg, Pred, Alias],
		assertz('$lgt_pp_dcl_'((Head :- Rename, Lookup)))
	;	Head =.. [ODcl, Pred, Scope, Meta, Flags, Obj, Ctn],
		assertz('$lgt_pp_dcl_'((Head:-Lookup)))
	),
	fail.

'$lgt_gen_prototype_imports_dcl_clauses' :-
	(	'$lgt_compiler_flag'(complements, allow) ->
		'$lgt_pp_object_'(Obj, _, ODcl, _, _, _, _, _, _, _, _),
		Head =.. [ODcl, Pred, Scope, Meta, Flags, SCtn, TCtn],
		Lookup = '$lgt_complemented_object'(Obj, ODcl, Pred, Scope, Meta, Flags, SCtn, TCtn),
		assertz('$lgt_pp_dcl_'((Head:-Lookup)))
	;	true
	).



'$lgt_gen_prototype_extends_dcl_clauses' :-
	'$lgt_pp_object_'(Obj, _, ODcl, _, _, _, _, _, _, Rnm, _),
	'$lgt_pp_extended_object_'(Parent, _, PDcl, _, _, _, _, _, _, EntityScope),
	(	EntityScope == (public) ->
		Lookup =.. [PDcl, Pred, Scope, Meta, Flags, SCtn, TCtn]
	;	EntityScope == protected ->
		Call =.. [PDcl, Pred, Scope2, Meta, Flags, SCtn, TCtn],
		Lookup = (Call, '$lgt_filter_scope'(Scope2, Scope))
	;	Scope = p,
		Call =.. [PDcl, Pred, Scope2, Meta, Flags, SCtn2, TCtn],
		Lookup = (Call, '$lgt_set_scope_container'(Scope2, SCtn2, Obj, SCtn))
	),
	(	'$lgt_pp_predicate_alias_'(Parent, _, _) ->
		Head =.. [ODcl, Alias, Scope, Meta, Flags, SCtn, TCtn],
		Rename =.. [Rnm, Parent, Pred, Alias],
		assertz('$lgt_pp_dcl_'((Head :- Rename, Lookup)))
	;	Head =.. [ODcl, Pred, Scope, Meta, Flags, SCtn, TCtn],
		assertz('$lgt_pp_dcl_'((Head:-Lookup)))
	),
	fail.

'$lgt_gen_prototype_extends_dcl_clauses'.



'$lgt_gen_prototype_def_clauses' :-
	(	'$lgt_pp_final_def_'(_) ->
		Local = true
	;	Local = false
	),
	'$lgt_gen_prototype_linking_def_clauses'(Local),
	'$lgt_gen_prototype_imports_def_clauses',
	'$lgt_gen_prototype_extends_def_clauses',
	'$lgt_gen_object_catchall_def_clauses'(Local).



'$lgt_gen_prototype_linking_def_clauses'(true) :-
	'$lgt_pp_object_'(Obj, _, _, Def, _, _, _, _, DDef, _, _),
	Head =.. [Def, Pred, ExCtx, Call, Obj],
	BodyDef =.. [Def, Pred, ExCtx, Call],
	assertz('$lgt_pp_final_def_'((Head:-BodyDef))),
	BodyDDef =.. [DDef, Pred, ExCtx, Call],
	assertz('$lgt_pp_final_def_'((Head:-BodyDDef))).

'$lgt_gen_prototype_linking_def_clauses'(false) :-
	'$lgt_pp_object_'(Obj, _, _, Def, _, _, _, _, DDef, _, _),
	Head =.. [Def, Pred, ExCtx, Call, Obj],
	BodyDDef =.. [DDef, Pred, ExCtx, Call],
	assertz('$lgt_pp_final_def_'((Head:-BodyDDef))).



'$lgt_gen_prototype_imports_def_clauses' :-
	'$lgt_pp_object_'(Obj, _, _, ODef, _, _, _, _, _, Rnm, _),
	'$lgt_pp_relation_clause_'('$lgt_imports_category_'(Obj, Ctg, _)),			% necessary for parameter passing
	'$lgt_pp_imported_category_'(Ctg, _, _, CDef, _),
	Lookup =.. [CDef, Pred, ExCtx, Call, Ctn],
	(	'$lgt_pp_predicate_alias_'(Ctg, _, _) ->
		Head =.. [ODef, Alias, ExCtx, Call, Ctn],
		Rename =.. [Rnm, Ctg, Pred, Alias],
		assertz('$lgt_pp_final_def_'((Head :- Rename, Lookup)))
	;	Head =.. [ODef, Pred, ExCtx, Call, Ctn],
		assertz('$lgt_pp_final_def_'((Head:-Lookup)))
	),
	fail.

'$lgt_gen_prototype_imports_def_clauses' :-
	(	'$lgt_compiler_flag'(complements, allow) ->
		'$lgt_pp_object_'(_, _, _, ODef, _, _, _, _, _, _, _),
		Head =.. [ODef, Pred, ExCtx, Call, Ctn],
		Lookup = '$lgt_complemented_object'(ODef, Pred, ExCtx, Call, Ctn),
		assertz('$lgt_pp_final_def_'((Head:-Lookup)))
	;	true
	).



'$lgt_gen_prototype_extends_def_clauses' :-
	'$lgt_pp_object_'(Obj, _, _, ODef, _, _, _, _, _, Rnm, _),
	'$lgt_pp_relation_clause_'('$lgt_extends_object_'(Obj, Parent, _)),			% necessary for parameter passing
	'$lgt_pp_extended_object_'(Parent, _, _, PDef, _, _, _, _, _, _),
	'$lgt_exec_ctx_this_rest'(PExCtx, Parent, Ctx),
	Lookup =.. [PDef, Pred, PExCtx, Call, Ctn],
	'$lgt_exec_ctx_this_rest'(OExCtx, Obj, Ctx),
	(	'$lgt_pp_predicate_alias_'(Parent, _, _) ->
		Head =.. [ODef, Alias, OExCtx, Call, Ctn],
		Rename =.. [Rnm, Parent, Pred, Alias],
		assertz('$lgt_pp_final_def_'((Head :- Rename, Lookup)))
	;	Head =.. [ODef, Pred, OExCtx, Call, Ctn],
		assertz('$lgt_pp_final_def_'((Head:-Lookup)))
	),
	fail.

'$lgt_gen_prototype_extends_def_clauses'.



% we can have a root object where super have nowhere to go ...

'$lgt_gen_prototype_super_clauses' :-
	'$lgt_pp_object_'(_, _, _, _, OSuper, _, _, _, _, _, _),
	\+ '$lgt_pp_extended_object_'(_, _, _, _, _, _, _, _, _, _),
	Head =.. [OSuper, _, _, _, _],
	assertz('$lgt_pp_super_'((Head:-fail))),
	!.

% ... or we may extends some objects

'$lgt_gen_prototype_super_clauses' :-
	'$lgt_pp_object_'(Obj, _, _, _, OSuper, _, _, _, _, Rnm, _),
	'$lgt_pp_relation_clause_'('$lgt_extends_object_'(Obj, Parent, _)),			% necessary for parameter passing
	'$lgt_pp_extended_object_'(Parent, _, _, PDef, _, _, _, _, _, _),
	'$lgt_exec_ctx_this_rest'(PExCtx, Parent, Ctx),
	Lookup =.. [PDef, Pred, PExCtx, Call, Ctn],
	'$lgt_exec_ctx_this_rest'(OExCtx, Obj, Ctx),
	(	'$lgt_pp_predicate_alias_'(Parent, _, _) ->
		Head =.. [OSuper, Alias, OExCtx, Call, Ctn],
		Rename =.. [Rnm, Parent, Pred, Alias],
		assertz('$lgt_pp_super_'((Head :- Rename, Lookup)))
	;	Head =.. [OSuper, Pred, OExCtx, Call, Ctn],
		assertz('$lgt_pp_super_'((Head:-Lookup)))
	),
	fail.

'$lgt_gen_prototype_super_clauses'.



'$lgt_gen_ic_clauses' :-
	'$lgt_gen_ic_dcl_clauses',
	'$lgt_gen_ic_def_clauses',
	'$lgt_gen_ic_super_clauses'.



'$lgt_gen_ic_dcl_clauses' :-
	'$lgt_gen_local_dcl_clauses'(Local),
	'$lgt_gen_ic_idcl_clauses'(Local),
	'$lgt_gen_ic_hierarchy_dcl_clauses',
	'$lgt_gen_object_catchall_dcl_clauses'(Local).



'$lgt_gen_ic_hierarchy_dcl_clauses' :-
	'$lgt_pp_object_'(_, _, ODcl, _, _, _, _, _, _, _, _),
	\+ '$lgt_pp_instantiated_class_'(_, _, _, _, _, _, _, _, _, _),
	!,
	Head =.. [ODcl, _, _, _, _, _, _],
	assertz('$lgt_pp_dcl_'((Head:-fail))).

'$lgt_gen_ic_hierarchy_dcl_clauses' :-
	'$lgt_pp_object_'(Obj, _, ODcl, _, _, _, _, _, _, Rnm, _),
	'$lgt_pp_instantiated_class_'(Class, _, _, _, _, CIDcl, _, _, _, EntityScope),
	(	EntityScope == (public) ->
		Lookup =.. [CIDcl, Pred, Scope, Meta, Flags, SCtn, TCtn]
	;	EntityScope == protected ->
		Call =.. [CIDcl, Pred, Scope2, Meta, Flags, SCtn, TCtn],
		Lookup = (Call, '$lgt_filter_scope'(Scope2, Scope))
	;	Scope = p,
		Call =.. [CIDcl, Pred, Scope2, Meta, Flags, SCtn2, TCtn],
		Lookup = (Call, '$lgt_set_scope_container'(Scope2, SCtn2, Obj, SCtn))
	),
	(	'$lgt_pp_predicate_alias_'(Class, _, _) ->
		Head =.. [ODcl, Alias, Scope, Meta, Flags, SCtn, TCtn],
		Rename =.. [Rnm, Class, Pred, Alias],
		assertz('$lgt_pp_dcl_'((Head :- Rename, Lookup)))
	;	Head =.. [ODcl, Pred, Scope, Meta, Flags, SCtn, TCtn],
		assertz('$lgt_pp_dcl_'((Head:-Lookup)))
	),
	fail.

'$lgt_gen_ic_hierarchy_dcl_clauses'.



% generates instance/class inherited declaration clauses

'$lgt_gen_ic_idcl_clauses'(Local) :-
	'$lgt_gen_ic_linking_idcl_clauses'(Local),
	'$lgt_gen_ic_protocol_idcl_clauses',
	'$lgt_gen_ic_category_idcl_clauses',
	'$lgt_gen_ic_hierarchy_idcl_clauses'.



'$lgt_gen_ic_linking_idcl_clauses'(true) :-
	'$lgt_pp_object_'(Obj, _, Dcl, _, _, IDcl, _, DDcl, _, _, _),
	HeadDcl =.. [IDcl, Pred, Scope, Meta, Flags, Obj, Obj],
	BodyDcl =.. [Dcl, Pred, Scope, Meta, Flags],
	assertz('$lgt_pp_dcl_'((HeadDcl:-BodyDcl))),
	(	'$lgt_compiler_flag'(dynamic_declarations, allow) ->
		HeadDDcl =.. [IDcl, Pred, Scope, no, 2, Obj, Obj],
		BodyDDcl =.. [DDcl, Pred, Scope],
		assertz('$lgt_pp_dcl_'((HeadDDcl:-BodyDDcl)))
	;	true
	).

'$lgt_gen_ic_linking_idcl_clauses'(false) :-
	'$lgt_pp_object_'(Obj, _, _, _, _, IDcl, _, DDcl, _, _, _),
	(	'$lgt_compiler_flag'(dynamic_declarations, allow) ->
		HeadDDcl =.. [IDcl, Pred, Scope, no, 2, Obj, Obj],
		BodyDDcl =.. [DDcl, Pred, Scope],
		assertz('$lgt_pp_dcl_'((HeadDDcl:-BodyDDcl)))
	;	\+ '$lgt_pp_implemented_protocol_'(_, _, _, _),
		\+ '$lgt_pp_imported_category_'(_, _, _, _, _),
		\+ '$lgt_pp_specialized_class_'(_, _, _, _, _, _, _, _, _, _) ->
		HeadDDcl =.. [IDcl, _, _, _, _, _, _],
		assertz('$lgt_pp_dcl_'((HeadDDcl:-fail)))
	;	true
	).



'$lgt_gen_ic_protocol_idcl_clauses' :-
	'$lgt_pp_object_'(Obj, _, _, _, _, OIDcl, _, _, _, Rnm, _),
	'$lgt_pp_implemented_protocol_'(Ptc, _, PDcl, EntityScope),
	(	EntityScope == (public) ->
		Lookup =.. [PDcl, Pred, Scope, Meta, Flags, Ctn]
	;	EntityScope == protected ->
		Call =.. [PDcl, Pred, Scope2, Meta, Flags, Ctn],
		Lookup = (Call, '$lgt_filter_scope'(Scope2, Scope))
	;	Scope = p,
		Lookup =.. [PDcl, Pred, _, Meta, Flags, Ctn]
	),
	(	'$lgt_pp_predicate_alias_'(Ptc, _, _) ->
		Head =.. [OIDcl, Alias, Scope, Meta, Flags, Obj, Ctn],
		Rename =.. [Rnm, Ptc, Pred, Alias],
		assertz('$lgt_pp_dcl_'((Head :- Rename, Lookup)))
	;	Head =.. [OIDcl, Pred, Scope, Meta, Flags, Obj, Ctn],
		assertz('$lgt_pp_dcl_'((Head:-Lookup)))
	),
	fail.

'$lgt_gen_ic_protocol_idcl_clauses'.



'$lgt_gen_ic_category_idcl_clauses' :-
	'$lgt_pp_object_'(Obj, _, _, _, _, OIDcl, _, _, _, Rnm, _),
	'$lgt_pp_imported_category_'(Ctg, _, CDcl, _, EntityScope),
	(	EntityScope == (public) ->
		Lookup =.. [CDcl, Pred, Scope, Meta, Flags, Ctn]
	;	EntityScope == protected ->
		Call =.. [CDcl, Pred, Scope2, Meta, Flags, Ctn],
		Lookup = (Call, '$lgt_filter_scope'(Scope2, Scope))
	;	Scope = p,
		Lookup =.. [CDcl, Pred, _, Meta, Flags, Ctn]
	),
	(	'$lgt_pp_predicate_alias_'(Ctg, _, _) ->
		Head =.. [OIDcl, Alias, Scope, Meta, Flags, Obj, Ctn],
		Rename =.. [Rnm, Ctg, Pred, Alias],
		assertz('$lgt_pp_dcl_'((Head :- Rename, Lookup)))
	;	Head =.. [OIDcl, Pred, Scope, Meta, Flags, Obj, Ctn],
		assertz('$lgt_pp_dcl_'((Head:-Lookup)))
	),
	fail.

'$lgt_gen_ic_category_idcl_clauses' :-
	(	'$lgt_compiler_flag'(complements, allow) ->
		'$lgt_pp_object_'(Obj, _, _, _, _, OIDcl, _, _, _, _, _),
		Head =.. [OIDcl, Pred, Scope, Meta, Flags, SCtn, TCtn],
		Lookup = '$lgt_complemented_object'(Obj, OIDcl, Pred, Scope, Meta, Flags, SCtn, TCtn),
		assertz('$lgt_pp_dcl_'((Head:-Lookup)))
	;	true
	).



'$lgt_gen_ic_hierarchy_idcl_clauses' :-
	'$lgt_pp_object_'(Obj, _, _, _, _, CIDcl, _, _, _, Rnm, _),
	'$lgt_pp_specialized_class_'(Super, _, _, _, _, SIDcl, _, _, _, EntityScope),
	(	EntityScope == (public) ->
		Lookup =.. [SIDcl, Pred, Scope, Meta, Flags, SCtn, TCtn]
	;	EntityScope == protected ->
		Call =.. [SIDcl, Pred, Scope2, Meta, Flags, SCtn, TCtn],
		Lookup = (Call, '$lgt_filter_scope'(Scope2, Scope))
	;	Scope = p,
		Call =.. [SIDcl, Pred, Scope2, Meta, Flags, SCtn2, TCtn],
		Lookup = (Call, '$lgt_set_scope_container'(Scope2, SCtn2, Obj, SCtn))
	),
	(	'$lgt_pp_predicate_alias_'(Super, _, _) ->
		Head =.. [CIDcl, Alias, Scope, Meta, Flags, SCtn, TCtn],
		Rename =.. [Rnm, Super, Pred, Alias],
		assertz('$lgt_pp_dcl_'((Head :- Rename, Lookup)))
	;	Head =.. [CIDcl, Pred, Scope, Meta, Flags, SCtn, TCtn],
		assertz('$lgt_pp_dcl_'((Head:-Lookup)))
	),
	fail.

'$lgt_gen_ic_hierarchy_idcl_clauses'.



'$lgt_gen_ic_def_clauses' :-
	(	'$lgt_pp_final_def_'(_) ->
		Local = true
	;	Local = false
	),
	'$lgt_gen_ic_linking_def_clauses'(Local),
	'$lgt_gen_ic_imports_def_clauses',
	'$lgt_gen_ic_hierarchy_def_clauses',
	'$lgt_gen_ic_idef_clauses'(Local),
	'$lgt_gen_object_catchall_def_clauses'(Local).



'$lgt_gen_ic_linking_def_clauses'(true) :-
	'$lgt_pp_object_'(Obj, _, _, Def, _, _, _, _, DDef, _, _),
	Head =.. [Def, Pred, ExCtx, Call, Obj],
	BodyDef =.. [Def, Pred, ExCtx, Call],
	assertz('$lgt_pp_final_def_'((Head:-BodyDef))),
	BodyDDef =.. [DDef, Pred, ExCtx, Call],
	assertz('$lgt_pp_final_def_'((Head:-BodyDDef))).

'$lgt_gen_ic_linking_def_clauses'(false) :-
	'$lgt_pp_object_'(Obj, _, _, Def, _, _, _, _, DDef, _, _),
	Head =.. [Def, Pred, ExCtx, Call, Obj],
	BodyDDef =.. [DDef, Pred, ExCtx, Call],
	assertz('$lgt_pp_final_def_'((Head:-BodyDDef))).



'$lgt_gen_ic_imports_def_clauses' :-
	'$lgt_pp_object_'(Obj, _, _, ODef, _, _, _, _, _, Rnm, _),
	'$lgt_pp_relation_clause_'('$lgt_imports_category_'(Obj, Ctg, _)),			% necessary for parameter passing
	'$lgt_pp_imported_category_'(Ctg, _, _, CDef, _),
	Lookup =.. [CDef, Pred, ExCtx, Call, Ctn],
	(	'$lgt_pp_predicate_alias_'(Ctg, _, _) ->
		Head =.. [ODef, Alias, ExCtx, Call, Ctn],
		Rename =.. [Rnm, Ctg, Pred, Alias],
		assertz('$lgt_pp_final_def_'((Head :- Rename, Lookup)))
	;	Head =.. [ODef, Pred, ExCtx, Call, Ctn],
		assertz('$lgt_pp_final_def_'((Head:-Lookup)))
	),
	fail.

'$lgt_gen_ic_imports_def_clauses' :-
	(	'$lgt_compiler_flag'(complements, allow) ->
		'$lgt_pp_object_'(_, _, _, ODef, _, _, _, _, _, _, _),
		Head =.. [ODef, Pred, ExCtx, Call, Ctn],
		Lookup = '$lgt_complemented_object'(ODef, Pred, ExCtx, Call, Ctn),
		assertz('$lgt_pp_final_def_'((Head:-Lookup)))
	;	true
	).



'$lgt_gen_ic_hierarchy_def_clauses' :-
	'$lgt_pp_object_'(Obj, _, _, ODef, _, _, _, _, _, Rnm, _),
	'$lgt_pp_relation_clause_'('$lgt_instantiates_class_'(Obj, Class, _)),		% necessary for parameter passing
	'$lgt_pp_instantiated_class_'(Class, _, _, _, _, _, CIDef, _, _, _),
	'$lgt_exec_ctx_this_rest'(CExCtx, Class, Ctx),
	Lookup =.. [CIDef, Pred, CExCtx, Call, Ctn],
	'$lgt_exec_ctx_this_rest'(OExCtx, Obj, Ctx),
	(	'$lgt_pp_predicate_alias_'(Class, _, _) ->
		Head =.. [ODef, Alias, OExCtx, Call, Ctn],
		Rename =.. [Rnm, Class, Pred, Alias],
		assertz('$lgt_pp_final_def_'((Head :- Rename, Lookup)))
	;	Head =.. [ODef, Pred, OExCtx, Call, Ctn],
		assertz('$lgt_pp_final_def_'((Head:-Lookup)))
	),
	fail.

'$lgt_gen_ic_hierarchy_def_clauses'.



'$lgt_gen_ic_idef_clauses'(Local) :-
	'$lgt_gen_ic_linking_idef_clauses'(Local),
	'$lgt_gen_ic_category_idef_clauses',
	'$lgt_gen_ic_hierarchy_idef_clauses'.



'$lgt_gen_ic_linking_idef_clauses'(true) :-
	'$lgt_pp_object_'(Obj, _, _, Def, _, _, IDef, _, DDef, _, _),
	Head =.. [IDef, Pred, ExCtx, Call, Obj],
	BodyDef =.. [Def, Pred, ExCtx, Call],
	assertz('$lgt_pp_final_def_'((Head:-BodyDef))),
	BodyDDef =.. [DDef, Pred, ExCtx, Call],
	assertz('$lgt_pp_final_def_'((Head:-BodyDDef))).

'$lgt_gen_ic_linking_idef_clauses'(false) :-
	'$lgt_pp_object_'(Obj, _, _, _, _, _, IDef, _, DDef, _, _),
	Head =.. [IDef, Pred, ExCtx, Call, Obj],
	BodyDDef =.. [DDef, Pred, ExCtx, Call],
	assertz('$lgt_pp_final_def_'((Head:-BodyDDef))).



'$lgt_gen_ic_category_idef_clauses' :-
	'$lgt_pp_object_'(Obj, _, _, _, _, _, OIDef, _, _, Rnm, _),
	'$lgt_pp_relation_clause_'('$lgt_imports_category_'(Obj, Ctg, _)),			% necessary for parameter passing
	'$lgt_pp_imported_category_'(Ctg, _, _, CDef, _),
	'$lgt_exec_ctx_this'(ExCtx, Obj),
	Lookup =.. [CDef, Pred, ExCtx, Call, Ctn],
	(	'$lgt_pp_predicate_alias_'(Ctg, _, _) ->
		Head =.. [OIDef, Alias, ExCtx, Call, Ctn],
		Rename =.. [Rnm, Ctg, Pred, Alias],
		assertz('$lgt_pp_final_def_'((Head :- Rename, Lookup)))
	;	Head =.. [OIDef, Pred, ExCtx, Call, Ctn],
		assertz('$lgt_pp_final_def_'((Head:-Lookup)))
	),
	fail.

'$lgt_gen_ic_category_idef_clauses' :-
	(	'$lgt_compiler_flag'(complements, allow) ->
		'$lgt_pp_object_'(Obj, _, _, _, _, _, OIDef, _, _, _, _),
		'$lgt_exec_ctx_this'(ExCtx, Obj),
		Head =.. [OIDef, Pred, ExCtx, Call, Ctn],
		Lookup = '$lgt_complemented_object'(OIDef, Pred, ExCtx, Call, Ctn),
		assertz('$lgt_pp_final_def_'((Head:-Lookup)))
	;	true
	).



'$lgt_gen_ic_hierarchy_idef_clauses' :-
	'$lgt_pp_object_'(Class, _, _, _, _, _, CIDef, _, _, Rnm, _),
	'$lgt_pp_relation_clause_'('$lgt_specializes_class_'(Class, Super, _)),		% necessary for parameter passing
	'$lgt_pp_specialized_class_'(Super, _, _, _, _, _, SIDef, _, _, _),
	'$lgt_exec_ctx_this_rest'(SExCtx, Super, Ctx),
	Lookup =.. [SIDef, Pred, SExCtx, Call, Ctn],
	'$lgt_exec_ctx_this_rest'(CExCtx, Class, Ctx),
	(	'$lgt_pp_predicate_alias_'(Super, _, _) ->
		Head =.. [CIDef, Alias, CExCtx, Call, Ctn],
		Rename =.. [Rnm, Super, Pred, Alias],
		assertz('$lgt_pp_final_def_'((Head :- Rename, Lookup)))
	;	Head =.. [CIDef, Pred, CExCtx, Call, Ctn],
		assertz('$lgt_pp_final_def_'((Head:-Lookup)))
	),
	fail.

'$lgt_gen_ic_hierarchy_idef_clauses'.



% we can have a root object where "super" have nowhere to go ...

'$lgt_gen_ic_super_clauses' :-
	'$lgt_pp_object_'(_, _, _, _, OSuper, _, _, _, _, _, _),
	\+ '$lgt_pp_instantiated_class_'(_, _, _, _, _, _, _, _, _, _),
	\+ '$lgt_pp_specialized_class_'(_, _, _, _, _, _, _, _, _, _),
	Head =.. [OSuper, _, _, _, _],
	assertz('$lgt_pp_super_'((Head:-fail))),
	!.

% ... or predicates can be redefined in instances...

'$lgt_gen_ic_super_clauses' :-
	'$lgt_pp_object_'(Obj, _, _, _, OSuper, _, _, _, _, Rnm, _),
	'$lgt_pp_relation_clause_'('$lgt_instantiates_class_'(Obj, Class, _)),		% necessary for parameter passing
	'$lgt_pp_instantiated_class_'(Class, _, _, _, _, _, CIDef, _, _, _),
	'$lgt_exec_ctx_this_rest'(CExCtx, Class, Ctx),
	Lookup =.. [CIDef, Pred, CExCtx, Call, Ctn],
	'$lgt_exec_ctx_this_rest'(OExCtx, Obj, Ctx),
	'$lgt_exec_ctx'(OExCtx, _, Obj, Obj, _, _),
	(	'$lgt_pp_predicate_alias_'(Class, _, _) ->
		Head =.. [OSuper, Alias, OExCtx, Call, Ctn],
		Rename =.. [Rnm, Class, Pred, Alias],
		assertz('$lgt_pp_super_'((Head :- Rename, Lookup)))
	;	Head =.. [OSuper, Pred, OExCtx, Call, Ctn],
		assertz('$lgt_pp_super_'((Head:-Lookup)))
	),
	fail.

% ... or/and in subclasses...

'$lgt_gen_ic_super_clauses' :-
	'$lgt_pp_object_'(Class, _, _, _, CSuper, _, _, _, _, Rnm, _),
	'$lgt_pp_relation_clause_'('$lgt_specializes_class_'(Class, Super, _)),		% necessary for parameter passing
	'$lgt_pp_specialized_class_'(Super, _, _, _, _, _, SIDef, _, _, _),
	'$lgt_exec_ctx_this_rest'(SExCtx, Super, Ctx),
	Lookup =.. [SIDef, Pred, SExCtx, Call, Ctn],
	'$lgt_exec_ctx_this_rest'(CExCtx, Class, Ctx),
	(	'$lgt_pp_predicate_alias_'(Super, _, _) ->
		Head =.. [CSuper, Alias, CExCtx, Call, Ctn],
		Rename =.. [Rnm, Super, Pred, Alias],
		assertz('$lgt_pp_super_'((Head :- Rename, Lookup)))
	;	Head =.. [CSuper, Pred, CExCtx, Call, Ctn],
		assertz('$lgt_pp_super_'((Head:-Lookup)))
	),
	fail.

'$lgt_gen_ic_super_clauses'.



% '$lgt_fix_synchronized_predicates'
%
% adds mutex wrappers for calling synchronized predicates;
% for Prolog compilers that do not support multi-threading,
% synchronized predicates are compiled as normal predicates

'$lgt_fix_synchronized_predicates' :-
	\+ '$lgt_compiler_flag'(threads, supported),
	!,
	(	retract('$lgt_pp_def_'(Def)),
		assertz('$lgt_pp_final_def_'(Def)),
		fail
	;	retract('$lgt_pp_ddef_'(DDef)),
		assertz('$lgt_pp_final_ddef_'(DDef)),
		fail
	;	true
	).

'$lgt_fix_synchronized_predicates' :-
	(	'$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, _, _) ->
		'$lgt_fix_synchronized_pred_defs',
		'$lgt_fix_synchronized_pred_ddefs'
	;	'$lgt_pp_category_'(_, _, _, _, _, _) ->
		'$lgt_fix_synchronized_pred_defs'
	;	true
	).


'$lgt_fix_synchronized_pred_defs' :-
	retract('$lgt_pp_def_'(Old)),
	Old =.. [Def, Head, ExCtx, THead],
	(	'$lgt_pp_synchronized_'(Head, Mutex) ->
		THead =.. [TFunctor| Args],
		atom_concat(TFunctor, '__sync', MFunctor),
		MHead =.. [MFunctor| Args],
		New =.. [Def, Head, ExCtx, MHead],
		assertz('$lgt_pp_final_def_'(New)),
		(	'$lgt_term_template'(Head, Mode),
			'$lgt_pp_mode_'(Mode, _),
			forall('$lgt_pp_mode_'(Mode, Solutions), (Solutions \== zero_or_one, Solutions \== one, Solutions \== zero)) ->
			assertz('$lgt_pp_entity_aux_clause_'((MHead:-setup_call_cleanup(mutex_lock(Mutex), THead, mutex_unlock(Mutex)))))
		;	assertz('$lgt_pp_entity_aux_clause_'((MHead:-with_mutex(Mutex, THead))))
		)
	;	assertz('$lgt_pp_final_def_'(Old))
	),
	fail.

'$lgt_fix_synchronized_pred_defs'.


'$lgt_fix_synchronized_pred_ddefs' :-
	retract('$lgt_pp_ddef_'(Old)),
	Old =.. [DDef, Head, ExCtx, THead],
	(	'$lgt_pp_synchronized_'(Head, Mutex) ->
		THead =.. [TFunctor| Args],
		atom_concat(TFunctor, '__sync', MFunctor),
		MHead =.. [MFunctor| Args],
		New =.. [DDef, Head, ExCtx, MHead],
		assertz('$lgt_pp_final_ddef_'(New)),
		(	'$lgt_term_template'(Head, Mode),
			'$lgt_pp_mode_'(Mode, _),
			forall('$lgt_pp_mode_'(Mode, Solutions), (Solutions \== zero_or_one, Solutions \== one, Solutions \== zero)) ->
			assertz('$lgt_pp_entity_aux_clause_'((MHead:-setup_call_cleanup(mutex_lock(Mutex), THead, mutex_unlock(Mutex)))))
		;	assertz('$lgt_pp_entity_aux_clause_'((MHead:-with_mutex(Mutex, THead))))
		)
	;	assertz('$lgt_pp_final_ddef_'(Old))
	),
	fail.

'$lgt_fix_synchronized_pred_ddefs'.



% '$lgt_fix_predicate_calls'
%
% fixes predicate calls in entity clauses and initialization goals

'$lgt_fix_predicate_calls' :-
	retract('$lgt_pp_entity_clause_'(Clause, Location)),
	(	Clause = {Term} ->
		assertz('$lgt_pp_final_entity_clause_'(Term, Location))
	;	Clause = (Head:-Body) ->
		'$lgt_fix_predicate_calls'(Body, FBody, false),
		assertz('$lgt_pp_final_entity_clause_'((Head:-FBody), Location))
	;	'$lgt_pp_value_annotation_'(Clause, Functor, Value, Body) ->
		'$lgt_fix_predicate_calls'(Body, FBody, true),
		'$lgt_pp_value_annotation_'(FClause, Functor, Value, FBody),
		assertz('$lgt_pp_final_entity_clause_'(FClause, Location))
	;	'$lgt_pp_goal_annotation_'(Clause, Functor, Body1, Body2) ->
		'$lgt_fix_predicate_calls'(Body1, FBody1, true),
		'$lgt_fix_predicate_calls'(Body2, FBody2, true),
		'$lgt_pp_goal_annotation_'(FClause, Functor, FBody1, FBody2),
		assertz('$lgt_pp_final_entity_clause_'(FClause, Location))
	;	assertz('$lgt_pp_final_entity_clause_'(Clause, Location))
	),
	fail.

'$lgt_fix_predicate_calls' :-
	retract('$lgt_pp_entity_aux_clause_'(Clause)),
	(	Clause = {Term} ->
		assertz('$lgt_pp_final_entity_aux_clause_'(Term))
	;	Clause = (Head:-Body) ->
		'$lgt_fix_predicate_calls'(Body, FBody, false),
		assertz('$lgt_pp_final_entity_aux_clause_'((Head:-FBody)))
	;	'$lgt_pp_value_annotation_'(Clause, Functor, Value, Body) ->
		'$lgt_fix_predicate_calls'(Body, FBody, true),
		'$lgt_pp_value_annotation_'(FClause, Functor, Value, FBody),
		assertz('$lgt_pp_final_entity_aux_clause_'(FClause))
	;	'$lgt_pp_goal_annotation_'(Clause, Functor, Body1, Body2) ->
		'$lgt_fix_predicate_calls'(Body1, FBody1, true),
		'$lgt_fix_predicate_calls'(Body2, FBody2, true),
		'$lgt_pp_goal_annotation_'(FClause, Functor, FBody1, FBody2),
		assertz('$lgt_pp_final_entity_aux_clause_'(FClause))
	;	assertz('$lgt_pp_final_entity_aux_clause_'(Clause))
	),
	fail.

'$lgt_fix_predicate_calls' :-
	retract('$lgt_pp_entity_init_'(Call)),
	'$lgt_fix_predicate_calls'(Call, Fixed, false),
	assertz('$lgt_pp_final_entity_init_'(Fixed)),
	fail.

'$lgt_fix_predicate_calls'.



% '$lgt_fix_predicate_calls'(+body, -body, +atom)
%
% fixes predicate calls in a clause body;
% the third argument is the atom "true" if we are fixing an annotated body

'$lgt_fix_predicate_calls'(Pred, Pred, _) :-
	var(Pred),
	!.

'$lgt_fix_predicate_calls'({Pred}, {Pred}, _) :-
	!.

'$lgt_fix_predicate_calls'((Pred1, Pred2), (TPred1, TPred2), Annotation) :-
	!,
	'$lgt_fix_predicate_calls'(Pred1, TPred1, Annotation),
	'$lgt_fix_predicate_calls'(Pred2, TPred2, Annotation).

'$lgt_fix_predicate_calls'((Pred1; Pred2), (TPred1; TPred2), Annotation) :-
	!,
	'$lgt_fix_predicate_calls'(Pred1, TPred1, Annotation),
	'$lgt_fix_predicate_calls'(Pred2, TPred2, Annotation).

'$lgt_fix_predicate_calls'((Pred1 -> Pred2), (TPred1 -> TPred2), Annotation) :-
	!,
	'$lgt_fix_predicate_calls'(Pred1, TPred1, Annotation),
	'$lgt_fix_predicate_calls'(Pred2, TPred2, Annotation).

'$lgt_fix_predicate_calls'(\+ Pred, \+ TPred, Annotation) :-
	!,
	'$lgt_fix_predicate_calls'(Pred, TPred, Annotation).

'$lgt_fix_predicate_calls'(Var^Pred, Var^TPred, Annotation) :-
	!,
	'$lgt_fix_predicate_calls'(Pred, TPred, Annotation).

'$lgt_fix_predicate_calls'(call(Pred), call(TPred), Annotation) :-
	!,
	'$lgt_fix_predicate_calls'(Pred, TPred, Annotation).

'$lgt_fix_predicate_calls'(once(Pred), once(TPred), Annotation) :-
	!,
	'$lgt_fix_predicate_calls'(Pred, TPred, Annotation).

'$lgt_fix_predicate_calls'(catch(Goal, Catcher, Recovery), catch(TGoal, Catcher, TRecovery), Annotation) :-
	!,
	'$lgt_fix_predicate_calls'(Goal, TGoal, Annotation),
	'$lgt_fix_predicate_calls'(Recovery, TRecovery, Annotation).

'$lgt_fix_predicate_calls'(bagof(Term, Pred, List), bagof(Term, TPred, List), Annotation) :-
	!,
	'$lgt_fix_predicate_calls'(Pred, TPred, Annotation).

'$lgt_fix_predicate_calls'(findall(Term, Pred, List), findall(Term, TPred, List), Annotation) :-
	!,
	'$lgt_fix_predicate_calls'(Pred, TPred, Annotation).

'$lgt_fix_predicate_calls'(forall(Gen, Test), forall(TGen, TTest), Annotation) :-
	!,
	'$lgt_fix_predicate_calls'(Gen, TGen, Annotation),
	'$lgt_fix_predicate_calls'(Test, TTest, Annotation).

'$lgt_fix_predicate_calls'(setof(Term, Pred, List), setof(Term, TPred, List), Annotation) :-
	!,
	'$lgt_fix_predicate_calls'(Pred, TPred, Annotation).

'$lgt_fix_predicate_calls'('$lgt_threaded_or'(Queue, MTGoals, Results), '$lgt_threaded_or'(Queue, TMTGoals, Results), Annotation) :-
	!,
	'$lgt_fix_predicate_calls'(MTGoals, TMTGoals, Annotation).

'$lgt_fix_predicate_calls'('$lgt_threaded_and'(Queue, MTGoals, Results), '$lgt_threaded_and'(Queue, TMTGoals, Results), Annotation) :-
	!,
	'$lgt_fix_predicate_calls'(MTGoals, TMTGoals, Annotation).

'$lgt_fix_predicate_calls'('$lgt_threaded_goal'(Pred, TVars, Queue, Id), '$lgt_threaded_goal'(TPred, TVars, Queue, Id), Annotation) :-
	!,
	'$lgt_fix_predicate_calls'(Pred, TPred, Annotation).

'$lgt_fix_predicate_calls'('$lgt_threaded_ignore'(Pred), '$lgt_threaded_ignore'(TPred), Annotation) :-
	!,
	'$lgt_fix_predicate_calls'(Pred, TPred, Annotation).

'$lgt_fix_predicate_calls'('$lgt_debugger.goal'(OPred, Pred, ExCtx), '$lgt_debugger.goal'(OPred, TPred, ExCtx), Annotation) :-
	!,
	'$lgt_fix_predicate_calls'(Pred, TPred, Annotation).

'$lgt_fix_predicate_calls'('$lgt_send_to_obj_'(Obj, Pred, This), TPred, _) :-
	'$lgt_obj_static_binding_cache'(Obj, Pred, This, Call),
	!,
	TPred = '$lgt_guarded_method_call'(Obj, Pred, This, Call).

'$lgt_fix_predicate_calls'('$lgt_send_to_obj_ne_'(Obj, Pred, This), TPred, _) :-
	'$lgt_obj_static_binding_cache'(Obj, Pred, This, Call),
	!,
	TPred = Call.

'$lgt_fix_predicate_calls'('$lgt_call_built_in'(Pred, MetaExPred, ExCtx), TPred, _) :-
	!,												% calls to Logtalk and Prolog built-in (meta-)predicates
	(	'$lgt_pp_redefined_built_in_'(Pred, ExCtx, TPred) ->
		true
	;	'$lgt_fix_predicate_calls'(MetaExPred, TPred, false)
	).

'$lgt_fix_predicate_calls'('$lgt_debugger.goal'(Pred, DPred, ExCtx), '$lgt_debugger.goal'(Pred, TPred, ExCtx), Annotation) :-
	!,												% calls in debug mode
	'$lgt_fix_predicate_calls'(DPred, TPred, Annotation).

'$lgt_fix_predicate_calls'(Pred, fail, false) :-	% calls to static, declared but undefined predicates;
	functor(Pred, Functor, Arity),					% must fail instead of throwing an exception
	'$lgt_undefined_predicate_call'(_, Functor/Arity),
	!.

'$lgt_fix_predicate_calls'(Pred, TPred, _) :-
	'$lgt_pp_value_annotation_'(Pred, Functor, Value, Goal),
	!,
	'$lgt_fix_predicate_calls'(Goal, TGoal, true),
	'$lgt_pp_value_annotation_'(TPred, Functor, Value, TGoal).

'$lgt_fix_predicate_calls'(Pred, TPred, _) :-
	'$lgt_pp_goal_annotation_'(Pred, Functor, Goal1, Goal2),
	!,
	'$lgt_fix_predicate_calls'(Goal1, TGoal1, true),
	'$lgt_fix_predicate_calls'(Goal2, TGoal2, true),
	'$lgt_pp_goal_annotation_'(TPred, Functor, TGoal1, TGoal2).

'$lgt_fix_predicate_calls'(Pred, Pred, _) :-
	'$lgt_lgt_built_in'(Pred),
	!.

'$lgt_fix_predicate_calls'(Pred, Pred, _) :-
	'$lgt_built_in_method'(Pred, _, _, _),
	!.

'$lgt_fix_predicate_calls'(Pred, TPred, _) :-		% calls to non-standard Prolog built-in meta-predicates
	'$lgt_pl_built_in'(Pred),
	'$lgt_pl_meta_predicate'(Pred, Meta, _),
	!,
	Pred =.. [Functor| Args],
	Meta =.. [Functor| MArgs],
	'$lgt_fix_pred_calls_in_margs'(Args, MArgs, TArgs),
	TPred =.. [Functor| TArgs].

'$lgt_fix_predicate_calls'(':'(Module, Pred), ':'(Module, Pred), _) :-
	var(Pred),
	!.

'$lgt_fix_predicate_calls'(':'(Module1, ':'(Module2, Pred)), ':'(Module1, ':'(Module2, TPred)), _) :-
	!,												% calls to Prolog module meta-predicates
	'$lgt_fix_predicate_calls'(':'(Module2, Pred), ':'(Module2, TPred), false).

'$lgt_fix_predicate_calls'(':'(Module, Pred), ':'(Module, TPred), _) :-
	'$lgt_term_template'(Pred, OverridingMeta),
	catch('$lgt_predicate_property'(':'(Module, Pred), meta_predicate(OriginalMeta)), _, fail),
	(	'$lgt_pp_meta_predicate_'(':'(Module, OverridingMeta)) ->
		% we're overriding the original meta-predicate template:
		Meta = OverridingMeta
	;	Meta = OriginalMeta
	),
	!,												% calls to Prolog module meta-predicates
	Pred =.. [Functor| Args],
	Meta =.. [Functor| MArgs],
	'$lgt_tr_module_meta_predicate_directives_args'(MArgs, CMArgs),
	'$lgt_fix_pred_calls_in_margs'(Args, CMArgs, TArgs),
	TPred =.. [Functor| TArgs].

'$lgt_fix_predicate_calls'(':'(Module, Pred), ':'(Module, TPred), _) :-
	!,
	'$lgt_fix_predicate_calls'(Pred, TPred, false).

'$lgt_fix_predicate_calls'(Pred, Pred, _).



% '$lgt_fix_pred_calls_in_margs'(@list, @list, -list)
%
% fixes predicate calls in non-standard meta-predicate arguments

'$lgt_fix_pred_calls_in_margs'([], [], []).

'$lgt_fix_pred_calls_in_margs'([Arg| Args], [MArg| MArgs], [TArg| TArgs]) :-
	'$lgt_fix_pred_calls_ins_in_marg'(MArg, Arg, TArg),
	'$lgt_fix_pred_calls_in_margs'(Args, MArgs, TArgs).


'$lgt_fix_pred_calls_ins_in_marg'(0, Arg, TArg) :-
	!,
	'$lgt_fix_predicate_calls'(Arg, TArg, false).

'$lgt_fix_pred_calls_ins_in_marg'([0], [Arg| Args], [TArg| TArgs]) :-
	!,
	'$lgt_fix_predicate_calls'(Arg, TArg, false),
	'$lgt_fix_pred_calls_ins_in_marg'([0], Args, TArgs).

'$lgt_fix_pred_calls_ins_in_marg'([0], [], []) :-
	!.

'$lgt_fix_pred_calls_ins_in_marg'(_, Arg, Arg).



% reports calls to declared, static but undefined predicates in the body of object and category predicates

'$lgt_report_undefined_predicate_calls'(_, _) :-
	'$lgt_compiler_flag'(misspelt, error),
	'$lgt_undefined_predicate_call'(Pred),
	throw(existence_error(procedure, Pred)).

'$lgt_report_undefined_predicate_calls'(Type, Entity) :-
	(	'$lgt_compiler_flag'(misspelt, warning),
		setof(Pred, '$lgt_undefined_predicate_call'(Pred), Preds) ->
		'$lgt_report_warning_in_new_line',
		'$lgt_inc_compile_warnings_counter',

		(	Preds = [_] ->
			(	('$lgt_pp_value_annotation_'(_, _, _, _); '$lgt_pp_goal_annotation_'(_, _, _, _)) ->
				write('%         WARNING!  This declared static predicate is called but may not be defined: ')
			;	write('%         WARNING!  This declared static predicate is called but never defined: ')
			)
		;	(	('$lgt_pp_value_annotation_'(_, _, _, _); '$lgt_pp_goal_annotation_'(_, _, _, _)) ->
				write('%         WARNING!  These declared static predicates are called but may not be defined: ')
			;	write('%         WARNING!  These declared static predicates are called but never defined: ')
			),
			nl, write('%                       ')
		),
		'$lgt_writeq_list'(Preds), nl,
		(	'$lgt_compiler_flag'(report, warnings) ->
			'$lgt_report_warning_entity_context'(Type, Entity)
		;	true
		)
	;	true
	).


'$lgt_undefined_predicate_call'(Pred) :-
	'$lgt_undefined_predicate_call'(Pred, _).


'$lgt_undefined_predicate_call'(Functor/Arity, TFunctor/TArity) :-
	'$lgt_pp_calls_predicate_'(Functor, Arity, TFunctor, TArity),
	\+ '$lgt_pp_defines_predicate_'(Functor, Arity),		% predicate not defined in object/category and
	\+ '$lgt_pp_dynamic_'(Functor, Arity),					% predicate not declared dynamic in in object/category
	Arity2 is Arity - 2,									% (only return predicates that are not the expansion 
	\+ '$lgt_pp_calls_non_terminal_'(Functor, Arity2),		% of grammar rules; see second clause)
	once((	'$lgt_pp_public_'(Functor, Arity)				% but there is a scope directive for the predicate
		;	'$lgt_pp_protected_'(Functor, Arity)
		;	'$lgt_pp_private_'(Functor, Arity)
	)).

'$lgt_undefined_predicate_call'(Functor//Arity, TFunctor/TArity) :-
	'$lgt_pp_calls_non_terminal_'(Functor, Arity),
	\+ '$lgt_pp_defines_non_terminal_'(Functor, Arity),		% non-terminal not defined in object/category and
	ExtArity is Arity + 2,
	\+ '$lgt_pp_defines_predicate_'(Functor, ExtArity),		% no corresponding predicate is defined
	\+ '$lgt_pp_dynamic_'(Functor, ExtArity),				% no dynamic directive for the corresponding predicate 
	once((	'$lgt_pp_public_'(Functor, ExtArity)			% but there is a scope directive for the non-terminal 
		;	'$lgt_pp_protected_'(Functor, ExtArity)			% or the corresponding predicate 
		;	'$lgt_pp_private_'(Functor, ExtArity)
	)),
	'$lgt_pp_calls_predicate_'(Functor, ExtArity, TFunctor, TArity).



% reports possibly missing dynamic directives

'$lgt_report_missing_dynamic_directives'(Type, Entity) :-
	(   '$lgt_compiler_flag'(missing_directives, warning),
		setof(Pred, '$lgt_missing_dynamic_directive'(Pred), Preds) ->
		'$lgt_report_warning_in_new_line',
		'$lgt_inc_compile_warnings_counter',
		(	Preds = [_] ->
			(	('$lgt_pp_value_annotation_'(_, _, _, _); '$lgt_pp_goal_annotation_'(_, _, _, _); \+ '$lgt_pp_module_'(_)) ->
				write('%         WARNING!  Possibly missing dynamic/1 directive for the predicate: ')
			;	write('%         WARNING!  Missing dynamic/1 directive for the predicate: ')
			)
		;	(	('$lgt_pp_value_annotation_'(_, _, _, _); '$lgt_pp_goal_annotation_'(_, _, _, _); \+ '$lgt_pp_module_'(_)) ->
				write('%         WARNING!  Possibly missing dynamic/1 directives for the predicates: ')
			;	write('%         WARNING!  Missing dynamic/1 directives for the predicates: ')
			),
			nl, write('%                       ')
		),
		'$lgt_writeq_list'(Preds), nl,
		(	'$lgt_compiler_flag'(report, warnings) ->
			'$lgt_report_warning_entity_context'(Type, Entity)
		;	true
		)
	;	true
	).


'$lgt_missing_dynamic_directive'(Functor/Arity) :-
	'$lgt_pp_missing_dynamic_directive_'(Functor, Arity),
	\+ '$lgt_pp_dynamic_'(Functor, Arity).		% check for out-of-place dynamic/1 directive



% reports possibly missing discontiguous directives

'$lgt_report_missing_discontiguous_directives'(Type, Entity) :-
	(   '$lgt_compiler_flag'(missing_directives, warning),
		setof(Pred, '$lgt_missing_discontiguous_directive'(Pred), Preds) ->
		'$lgt_report_warning_in_new_line',
		'$lgt_inc_compile_warnings_counter',
		(	Preds = [_] ->
			(	('$lgt_pp_value_annotation_'(_, _, _, _); '$lgt_pp_goal_annotation_'(_, _, _, _); \+ '$lgt_pp_module_'(_)) ->
				write('%         WARNING!  Possibly missing discontiguous/1 directive for the predicate: ')
			;	write('%         WARNING!  Missing discontiguous/1 directive for the predicate: ')
			)
		;	(	('$lgt_pp_value_annotation_'(_, _, _, _); '$lgt_pp_goal_annotation_'(_, _, _, _); \+ '$lgt_pp_module_'(_)) ->
				write('%         WARNING!  Possibly missing discontiguous/1 directives for the predicates: ')
			;	write('%         WARNING!  Missing discontiguous/1 directives for the predicates: ')
			),
			nl, write('%                       ')
		),
		'$lgt_writeq_list'(Preds), nl,
		(	'$lgt_compiler_flag'(report, warnings) ->
			'$lgt_report_warning_entity_context'(Type, Entity)
		;	true
		)
	;	true
	).


'$lgt_missing_discontiguous_directive'(Functor/Arity) :-
	'$lgt_pp_missing_discontiguous_directive_'(Functor, Arity),
	\+ '$lgt_pp_discontiguous_'(Functor, Arity).		% check for out-of-place dynamic/1 directive



% reports possible misspelt predicate calls in the body of object and category predicates

'$lgt_report_misspelt_calls'(_, _) :-
	'$lgt_compiler_flag'(misspelt, error),
	'$lgt_misspelt_call'(Pred),
	throw(existence_error(predicate, Pred)).

'$lgt_report_misspelt_calls'(Type, Entity) :-
	(   '$lgt_compiler_flag'(misspelt, warning),
		setof(Pred, '$lgt_misspelt_call'(Pred), Preds) ->
		'$lgt_report_warning_in_new_line',
		'$lgt_inc_compile_warnings_counter',
		(	Preds = [_] ->
			(	('$lgt_pp_value_annotation_'(_, _, _, _); '$lgt_pp_goal_annotation_'(_, _, _, _)) ->
				write('%         WARNING!  This predicate is called but may not be defined: ')
			;	write('%         WARNING!  This predicate is called but never defined: ')
			)
		;	(	('$lgt_pp_value_annotation_'(_, _, _, _); '$lgt_pp_goal_annotation_'(_, _, _, _)) ->
				write('%         WARNING!  These predicates are called but may not be defined: ')
			;	write('%         WARNING!  These predicates are called but never defined: ')
			),
			nl, write('%                       ')
		),
		'$lgt_writeq_list'(Preds), nl,
		(	'$lgt_compiler_flag'(report, warnings) ->
			'$lgt_report_warning_entity_context'(Type, Entity)
		;	true
		)
	;	true
	).


'$lgt_misspelt_call'(Functor/Arity) :-
	'$lgt_pp_calls_predicate_'(Functor, Arity, _, _),
	\+ '$lgt_pp_defines_predicate_'(Functor, Arity),
	\+ '$lgt_pp_dynamic_'(Functor, Arity),
	\+ '$lgt_pp_public_'(Functor, Arity),
	\+ '$lgt_pp_protected_'(Functor, Arity),
	\+ '$lgt_pp_private_'(Functor, Arity),
	Arity2 is Arity - 2,
	\+ '$lgt_pp_calls_non_terminal_'(Functor, Arity2).

'$lgt_misspelt_call'(Functor//Arity) :-
	'$lgt_pp_calls_non_terminal_'(Functor, Arity),
	\+ '$lgt_pp_defines_non_terminal_'(Functor, Arity),
	ExtArity is Arity + 2,
	\+ '$lgt_pp_defines_predicate_'(Functor, ExtArity),
	\+ '$lgt_pp_dynamic_'(Functor, ExtArity),
	\+ '$lgt_pp_public_'(Functor, ExtArity),
	\+ '$lgt_pp_protected_'(Functor, ExtArity),
	\+ '$lgt_pp_private_'(Functor, ExtArity).



% reports non-portable predicate calls in the body of object and category predicates

'$lgt_report_non_portable_calls'(Type, Entity) :-
	(	setof(Pred, '$lgt_non_portable_call'(Pred), Preds) ->
		'$lgt_report_warning_in_new_line',
		'$lgt_inc_compile_warnings_counter',
		(	Preds = [_] ->
			write('%         WARNING!  Call to non-standard Prolog built-in predicate: ')
		;	write('%         WARNING!  Calls to non-standard Prolog built-in predicates: '), nl,
			write('%                       ')
		),
		'$lgt_writeq_list'(Preds), nl,
		(	'$lgt_compiler_flag'(report, warnings) ->
			'$lgt_report_warning_entity_context'(Type, Entity)
		;	true
		)
	;	true
	).


'$lgt_non_portable_call'(Functor/Arity) :-
	'$lgt_pp_non_portable_call_'(Functor, Arity),
	\+ '$lgt_pp_defines_predicate_'(Functor, Arity),
	functor(Pred, Functor, Arity),
	\+ '$lgt_pp_redefined_built_in_'(Pred, _, _).



% reports non-portable arithmetic function calls in the body of object and category predicates

'$lgt_report_non_portable_functions'(Type, Entity) :-
	(	setof(Functor/Arity, '$lgt_pp_non_portable_function_'(Functor, Arity), Functions) ->
		'$lgt_report_warning_in_new_line',
		'$lgt_inc_compile_warnings_counter',
		(	Functions = [_] ->
			write('%         WARNING!  Call to non-standard Prolog built-in arithmetic function: ')
		;	write('%         WARNING!  Calls to non-standard Prolog built-in arithmetic functions: '), nl,
			write('%                       ')
		),
		'$lgt_writeq_list'(Functions), nl,
		(	'$lgt_compiler_flag'(report, warnings) ->
			'$lgt_report_warning_entity_context'(Type, Entity)
		;	true
		)
	;	true
	).



% '$lgt_write_encoding_directive'(@stream)
%
% writes the encoding/1 directive (if it exists); must be the first term in the file

'$lgt_write_encoding_directive'(Stream) :-
	(	'$lgt_compiler_flag'(encoding_directive, full),
		'$lgt_pp_file_encoding_'(_, Encoding) ->
		write_canonical(Stream, (:- encoding(Encoding))),
		write(Stream, '.'),
		nl(Stream)
	;	true
	).



% '$lgt_write_logtalk_directives'(@stream)
%
% writes Logtalk directives

'$lgt_write_logtalk_directives'(Stream) :-
	'$lgt_pp_directive_'(Directive),
	write_canonical(Stream, (:- Directive)),
	write(Stream, '.'),
	nl(Stream),
	fail.

'$lgt_write_logtalk_directives'(_).



% '$lgt_write_prolog_terms'(@stream)
%
% writes any Prolog clauses that appear before an entity opening directive

'$lgt_write_prolog_terms'(Stream) :-
	'$lgt_pp_prolog_term_'(Term, Location),
	'$lgt_write_term_and_source_location'(Stream, Term, user, Location),
	fail.

'$lgt_write_prolog_terms'(_).



% '$lgt_write_logtalk_clauses'(@stream)
%
% writes Logtalk entity clauses

'$lgt_write_logtalk_clauses'(Stream) :-
	'$lgt_write_dcl_clauses'(Stream),
	'$lgt_write_def_clauses'(Stream),
	'$lgt_write_ddef_clauses'(Stream),
	'$lgt_write_super_clauses'(Stream),
	'$lgt_write_alias_clauses'(Stream),
	'$lgt_write_entity_clauses'(Stream),
	'$lgt_write_entity_aux_clauses'(Stream).



'$lgt_write_dcl_clauses'(Stream) :-
	'$lgt_pp_file_path_'(File, Path),
	'$lgt_pp_dcl_'(Clause),
	'$lgt_write_term_and_source_location'(Stream, Clause, aux, Path+File+1),
	fail.

'$lgt_write_dcl_clauses'(_).



'$lgt_write_def_clauses'(Stream) :-
	'$lgt_pp_file_path_'(File, Path),
	'$lgt_pp_final_def_'(Clause),
	'$lgt_write_term_and_source_location'(Stream, Clause, aux, Path+File+1),
	fail.

'$lgt_write_def_clauses'(_).



'$lgt_write_ddef_clauses'(Stream) :-
	'$lgt_pp_file_path_'(File, Path),
	'$lgt_pp_final_ddef_'(Clause),
	'$lgt_write_term_and_source_location'(Stream, Clause, aux, Path+File+1),
	fail.

'$lgt_write_ddef_clauses'(_).



'$lgt_write_super_clauses'(Stream) :-
	'$lgt_pp_file_path_'(File, Path),
	'$lgt_pp_super_'(Clause),
	'$lgt_write_term_and_source_location'(Stream, Clause, aux, Path+File+1),
	fail.

'$lgt_write_super_clauses'(_).



'$lgt_write_alias_clauses'(Stream) :-
	(	'$lgt_pp_object_'(_, _, _, _, _, _, _, _, _, Rnm, _)
	;	'$lgt_pp_category_'(_, _, _, _, Rnm, _)
	;	'$lgt_pp_protocol_'(_, _, _, Rnm, _)
	), !,
	'$lgt_write_alias_clauses'(Stream, Rnm).


'$lgt_write_alias_clauses'(Stream, Rnm) :-
	'$lgt_pp_file_path_'(File, Path),
	'$lgt_pp_predicate_alias_'(Entity, Pred, Alias),
	Clause =.. [Rnm, Entity, Pred, Alias],
	'$lgt_write_term_and_source_location'(Stream, Clause, aux, Path+File+1),
	fail.

'$lgt_write_alias_clauses'(Stream, Rnm) :-
	'$lgt_pp_file_path_'(File, Path),
	Catchall =.. [Rnm, _, Pred, Pred],
	'$lgt_write_term_and_source_location'(Stream, Catchall, aux, Path+File+1).



'$lgt_write_entity_clauses'(Stream) :-
	'$lgt_pp_final_entity_clause_'(Clause, Location),
	'$lgt_write_term_and_source_location'(Stream, Clause, user, Location),
	fail.

'$lgt_write_entity_clauses'(_).



'$lgt_write_entity_aux_clauses'(Stream) :-
	'$lgt_pp_file_path_'(File, Path),
	'$lgt_pp_final_entity_aux_clause_'(Clause),
	'$lgt_write_term_and_source_location'(Stream, Clause, aux, Path+File+1),
	fail.

'$lgt_write_entity_aux_clauses'(_).



% '$lgt_write_runtime_clauses'(@stream)
%
% writes the entity runtime multifile and dynamic directives and the entity 
% runtime clauses for all defined entities

'$lgt_write_runtime_clauses'(Stream) :-
	'$lgt_write_runtime_clauses'(Stream, '$lgt_current_protocol_'/5),
	'$lgt_write_runtime_clauses'(Stream, '$lgt_current_category_'/6),
	'$lgt_write_runtime_clauses'(Stream, '$lgt_current_object_'/11),
	'$lgt_write_runtime_clauses'(Stream, '$lgt_entity_property_'/2),
	'$lgt_write_runtime_clauses'(Stream, '$lgt_implements_protocol_'/3),
	'$lgt_write_runtime_clauses'(Stream, '$lgt_imports_category_'/3),
	'$lgt_write_runtime_clauses'(Stream, '$lgt_instantiates_class_'/3),
	'$lgt_write_runtime_clauses'(Stream, '$lgt_specializes_class_'/3),
	'$lgt_write_runtime_clauses'(Stream, '$lgt_extends_category_'/3),
	'$lgt_write_runtime_clauses'(Stream, '$lgt_extends_object_'/3),
	'$lgt_write_runtime_clauses'(Stream, '$lgt_extends_protocol_'/3),
	'$lgt_write_runtime_clauses'(Stream, '$lgt_complemented_object_'/5),
	'$lgt_write_runtime_clauses'(Stream, '$lgt_debugging_entity_'/1),
	'$lgt_write_runtime_clauses'(Stream, '$lgt_static_binding_entity_'/1).


'$lgt_write_runtime_clauses'(Stream, Functor/Arity) :-
	functor(Clause, Functor, Arity),
	(	\+ \+ '$lgt_pp_file_relation_clause_'(Clause) ->
		write_canonical(Stream, (:- multifile(Functor/Arity))), write(Stream, '.'), nl(Stream),
		write_canonical(Stream, (:- dynamic(Functor/Arity))), write(Stream, '.'), nl(Stream),
			'$lgt_pp_file_path_'(File, Path),
		(	'$lgt_pp_file_relation_clause_'(Clause),
			'$lgt_write_term_and_source_location'(Stream, Clause, aux, Path+File+1),
			fail
		;	true
		)
	;	true
	).



% '$lgt_write_init_call'(@stream)
%
% writes the initialization goal for the compiled source file, a conjunction
% of the initialization goals of the defined entities; for Prolog compilers
% that don't support the multifile/1 predicate directive, the initialization
% goal also asserts the relation clauses for all defined entities

'$lgt_write_init_call'(Stream) :-
	'$lgt_init_goal'(Goal),
	(	Goal == true ->
		true
	;	write_canonical(Stream, (:- initialization(Goal))),
		write(Stream, '.'), nl(Stream)
	).



% '$lgt_init_goal'(-callable)
%
% source file initialization goal constructed from each entity initialization
% goals and from the source file initialization/1 directive if present

'$lgt_init_goal'(Goal) :-
	findall(EGoal, '$lgt_pp_entity_init_'(_, _, EGoal), EGoals),
	findall(FGoal, '$lgt_pp_file_init_'(FGoal), FGoals),
	'$lgt_append'(EGoals, FGoals, Goals),
	'$lgt_list_to_conjunction'(Goals, Goals2),
	'$lgt_simplify_body'(Goals2, Goal).


% converts a list of goals into a conjunction of goals

'$lgt_list_to_conjunction'([], true) :- !.

'$lgt_list_to_conjunction'([Goal], Goal) :- !.

'$lgt_list_to_conjunction'([Goal1, Goal2| Goals], (Goal1, Rest)) :-
	'$lgt_list_to_conjunction'([Goal2| Goals], Rest).



% converts a conjunction into a list of terms

'$lgt_conjunction_to_list'(Conjunction, Terms) :-
	'$lgt_conjunction_to_list'(Conjunction, Terms, _).


'$lgt_conjunction_to_list'(Conjunction, Terms, N) :-
	'$lgt_conjunction_to_list'(Conjunction, Terms, 1, N).


'$lgt_conjunction_to_list'(Term, [Term], N, N) :-
	var(Term),
	!.

'$lgt_conjunction_to_list'((Term, Conjunction), [Term| Terms], N0, N) :-
	!,
	N1 is N0 + 1,
	'$lgt_conjunction_to_list'(Conjunction, Terms, N1, N).

'$lgt_conjunction_to_list'(Term, [Term], N, N).



% generates and asserts the initialization goal for the entity being compiled

'$lgt_gen_entity_init_goal' :-
	'$lgt_pp_entity'(Type, Entity, Prefix, _, _),
	(	setof(Mutex, Head^'$lgt_pp_synchronized_'(Head, Mutex), Mutexes) ->
		Goal1 = '$lgt_create_mutexes'(Mutexes)
	;	Goal1 = true
	),
	(	'$lgt_pp_threaded_' ->
		Goal2 = '$lgt_init_object_message_queue'(Prefix)
	;	Goal2 = true
	),
	findall(EntityInitGoal, '$lgt_pp_final_entity_init_'(EntityInitGoal), EntityInitGoals),
	'$lgt_list_to_conjunction'(EntityInitGoals, Goal3),
	'$lgt_simplify_body'((Goal1, Goal2, Goal3), Goal),
	(	Goal == true ->
		true
	;	assertz('$lgt_pp_entity_init_'(Type, Entity, Goal))
	).



% '$lgt_assert_tr_entity'
%
% adds a dynamically created entity to memory

'$lgt_assert_tr_entity' :-
	'$lgt_assert_directives',
	'$lgt_assert_dcl_clauses',
	'$lgt_assert_def_clauses',
	'$lgt_assert_ddef_clauses',
	'$lgt_assert_super_clauses',
	'$lgt_assert_entity_clauses',
	'$lgt_assert_entity_aux_clauses',
	'$lgt_assert_runtime_clauses',
	'$lgt_assert_init_goal'.



'$lgt_assert_directives' :-
	'$lgt_pp_directive_'(dynamic(Functor/Arity)),
		functor(Pred, Functor, Arity),
		asserta(Pred),
		retract(Pred),
	fail.

'$lgt_assert_directives' :-
	'$lgt_pp_directive_'(op(Pr, Spec, Ops)),
		op(Pr, Spec, Ops),
	fail.
	
'$lgt_assert_directives'.



'$lgt_assert_dcl_clauses' :-
	'$lgt_pp_dcl_'(Clause),
		'$lgt_assertz_entity_clause'(Clause, aux),
	fail.

'$lgt_assert_dcl_clauses'.



'$lgt_assert_def_clauses' :-
	'$lgt_pp_final_def_'(Clause),
		'$lgt_assertz_entity_clause'(Clause, aux),
	fail.

'$lgt_assert_def_clauses'.



'$lgt_assert_ddef_clauses' :-
	'$lgt_pp_final_ddef_'(Clause),
		'$lgt_assertz_entity_clause'(Clause, aux),
	fail.

'$lgt_assert_ddef_clauses'.



'$lgt_assert_super_clauses' :-
	'$lgt_pp_super_'(Clause),
		'$lgt_assertz_entity_clause'(Clause, aux),
	fail.

'$lgt_assert_super_clauses'.



'$lgt_assert_entity_clauses' :-
	'$lgt_pp_final_entity_clause_'(Clause, _),
		'$lgt_assertz_entity_clause'(Clause, user),
	fail.

'$lgt_assert_entity_clauses'.



'$lgt_assert_entity_aux_clauses' :-
	'$lgt_pp_final_entity_aux_clause_'(Clause),
		'$lgt_assertz_entity_clause'(Clause, aux),
	fail.

'$lgt_assert_entity_aux_clauses'.



'$lgt_assert_runtime_clauses' :-
	'$lgt_pp_relation_clause'(Clause),
		'$lgt_assertz_entity_clause'(Clause, aux),
	fail.

'$lgt_assert_runtime_clauses'.



% '$lgt_assert_init_goal'
%
% calls any defined initialization goal for a dynamically created entity

'$lgt_assert_init_goal' :-
	(	setof(Mutex, Head^'$lgt_pp_synchronized_'(Head, Mutex), Mutexes) ->
		'$lgt_create_mutexes'(Mutexes)
	;	true
	),
	(	'$lgt_pp_object_'(_, Prefix, _, _, _, _, _, _, _, _, _),
		'$lgt_pp_threaded_' ->
		'$lgt_init_object_message_queue'(Prefix)
	;	true 
	),
	findall(Goal, '$lgt_pp_final_entity_init_'(Goal), GoalList),
	'$lgt_list_to_conjunction'(GoalList, Goals),
	once(Goals).



% '$lgt_construct_object_functors'(+object_identifier, -atom, -atom, -atom, -atom, -atom, -atom, -atom, -atom, -atom)
%
% constructs functors used in the compiled code of an object

'$lgt_construct_object_functors'(Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm) :-
	(	'$lgt_current_object_'(Obj, Prefix, Dcl, Def, Super, IDcl, IDef, DDcl, DDef, Rnm, _) ->
		true
	;	'$lgt_construct_entity_prefix'(Obj, Prefix0, Prefix),
		atom_concat(Prefix0, '_dcl', Dcl),
		atom_concat(Prefix0, '_def', Def),
		atom_concat(Prefix0, '_super', Super),
		atom_concat(Prefix0, '_idcl', IDcl),
		atom_concat(Prefix0, '_idef', IDef),
		atom_concat(Prefix0, '_ddcl', DDcl),
		atom_concat(Prefix0, '_ddef', DDef),
		atom_concat(Prefix0, '_alias', Rnm)
	).	



% '$lgt_construct_protocol_functors'(+protocol_identifier, -atom, -atom, -atom)
%
% constructs functors used in the compiled code of a protocol

'$lgt_construct_protocol_functors'(Ptc, Prefix, Dcl, Rnm) :-
	(	'$lgt_current_protocol_'(Ptc, Prefix, Dcl, Rnm, _) ->
		true
	;	'$lgt_construct_entity_prefix'(Ptc, Prefix0, Prefix),
		atom_concat(Prefix0, '_dcl', Dcl),
		atom_concat(Prefix0, '_alias', Rnm)
	).



% '$lgt_construct_category_functors'(+category_identifier, -atom, -atom, -atom, -atom)
%
% constructs functors used in the compiled code of a category

'$lgt_construct_category_functors'(Ctg, Prefix, Dcl, Def, Rnm) :-
	(	'$lgt_current_category_'(Ctg, Prefix, Dcl, Def, Rnm, _) ->
		true
	;	'$lgt_construct_entity_prefix'(Ctg, Prefix0, Prefix),
		atom_concat(Prefix0, '_dcl', Dcl),
		atom_concat(Prefix0, '_def', Def),
		atom_concat(Prefix0, '_alias', Rnm)
	).



% '$lgt_construct_entity_prefix'(@entity_identifier, -atom, -atom)
%
% constructs the entity prefix used in the compiled code

'$lgt_construct_entity_prefix'(Entity, GenPrefix, UserPrefix) :-
	'$lgt_compiler_flag'(code_prefix, CodePrefix),
	(	atom(Entity) ->
		atom_concat(CodePrefix, Entity, GenPrefix0),
		atom_concat(GenPrefix0, '.', GenPrefix)
	;	functor(Entity, Functor, Arity),
		atom_concat(CodePrefix, Functor, GenPrefix0),
		atom_concat(GenPrefix0, '.', GenPrefix1),
		number_codes(Arity, ArityCodes),
		atom_codes(ArityAtom, ArityCodes),
		atom_concat(GenPrefix1, ArityAtom, GenPrefix2),
		atom_concat(GenPrefix2, '.', GenPrefix)
	),
	(	'$lgt_compiler_flag'(debug, on) ->
		atom_concat('.', GenPrefix, UserPrefix)
	;	UserPrefix = GenPrefix
	).



% '$lgt_reverse_entity_prefix'(+atom, -entity_identifier)
%
% reverses the entity prefix used in the compiled code

'$lgt_reverse_entity_prefix'(Prefix, Entity) :-
	'$lgt_compiler_flag'(code_prefix, CodePrefix0),
	(	CodePrefix = CodePrefix0
	;	atom_concat('.', CodePrefix0, CodePrefix)
	),
	atom_concat(CodePrefix, Entity0, Prefix),
	atom_concat(Entity1, '.', Entity0),
	(	atom_concat(FunctorDolar, ArityAtom, Entity1),
		atom_concat(Functor, '.', FunctorDolar) ->
		atom_codes(ArityAtom, ArityCodes),
		number_codes(Arity, ArityCodes)
	;	Functor = Prefix,
		Arity = 0
	),
	functor(Entity, Functor, Arity),
	!.



% '$lgt_compile_aux_clauses'(@list(clause))
%
% translates a list of auxiliary predicate clauses;
% used mainly in conjunction with goal_expansion/2 hooks

'$lgt_compile_aux_clauses'(Clauses) :-
	(	'$lgt_pp_object_'(_, Prefix, _, _, _, _, _, _, _, _, _) ->
		true
	;	'$lgt_pp_category_'(_, Prefix, _, _, _, _)
	),												% protocols cannot contain predicate clauses
	'$lgt_comp_ctx_prefix'(Ctx, Prefix),
	'$lgt_comp_ctx_mode'(Ctx, compile(aux)),		% avoid making a predicate discontiguous by accident
	'$lgt_compile_aux_clauses'(Clauses, Ctx).


'$lgt_compile_aux_clauses'([], _).

'$lgt_compile_aux_clauses'([Clause| Clauses], Ctx) :-
	!,
	'$lgt_tr_clause'(Clause, Ctx),
	'$lgt_compile_aux_clauses'(Clauses, Ctx).



% '$lgt_entity_prefix'(+entity_identifier, ?atom)
% '$lgt_entity_prefix'(-entity_identifier, +atom)
%
% converts between entity identifiers and internal entity prefixes;
% used mainly in hook objects for processing proprietary directives

'$lgt_entity_prefix'(Entity, Prefix) :-
	(	'$lgt_current_object_'(Entity, Prefix, _, _, _, _, _, _, _, _, _) ->
		true
	;	'$lgt_current_category_'(Entity, Prefix, _, _, _, _) ->
		true
	;	'$lgt_current_protocol_'(Entity, Prefix, _, _, _) ->
		true
	;	atom(Prefix) ->
		'$lgt_reverse_entity_prefix'(Prefix, Entity)
	;	callable(Entity),
		'$lgt_construct_entity_prefix'(Entity, _, Prefix)
	).



% '$lgt_compile_predicate_heads'(@list(callable), ?entity_identifier, -list(callable), @term)
% '$lgt_compile_predicate_heads'(@callable, ?entity_identifier, -callable, @term)
%
% translates a single predicate head, a conjunction of predicate heads, or a list of
% predicate heads; used mainly in hook objects for processing proprietary directives
%
% the predicate heads are compiled in the context of the specified entity or in the context
% of the entity being compiled when the entity argument is not instantiated

'$lgt_compile_predicate_heads'(_, Entity, _, _) :-
	nonvar(Entity),
	\+ callable(Entity),
	throw(type_error(entity_identifier, Entity)).

'$lgt_compile_predicate_heads'(Heads, Entity, THeads, Ctx) :-
	(	var(Entity) ->
		'$lgt_pp_entity'(_, Entity, Prefix, _, _)
	;	'$lgt_current_object_'(Entity, Prefix, _, _, _, _, _, _, _, _, _) ->
		true
	;	'$lgt_current_category_'(Entity, Prefix, _, _, _, _) ->
		true
	;	'$lgt_current_protocol_'(Entity, Prefix, _, _, _) ->
		true
	;	fail
	),
	'$lgt_compile_predicate_heads_aux'(Heads, Prefix, THeads, Ctx).


'$lgt_compile_predicate_heads_aux'(Heads, _, _, _) :-
	var(Heads),
	throw(instantiation_error).

'$lgt_compile_predicate_heads_aux'([], _, [], _) :-
	!.

'$lgt_compile_predicate_heads_aux'([Head| Heads], Prefix, [THead| THeads], Ctx) :-
	!,
	'$lgt_compile_predicate_heads_aux'(Head, Prefix, THead, Ctx),
	'$lgt_compile_predicate_heads_aux'(Heads, Prefix, THeads, Ctx).

'$lgt_compile_predicate_heads_aux'((Head, Heads), Prefix, (THead, THeads), Ctx) :-
	!,
	'$lgt_compile_predicate_heads_aux'(Head, Prefix, THead, Ctx),
	'$lgt_compile_predicate_heads_aux'(Heads, Prefix, THeads, Ctx).

'$lgt_compile_predicate_heads_aux'(Head, Prefix, THead, Ctx) :-
	(	callable(Head) ->
		functor(Head, Functor, Arity),
		'$lgt_construct_predicate_indicator'(Prefix, Functor/Arity, TFunctor/TArity),
		functor(THead, TFunctor, TArity),
		'$lgt_unify_head_thead_args'(Arity, Head, THead),
		arg(TArity, THead, Ctx)
	;	throw(type_error(callable, Head))
	).



% '$lgt_compile_predicate_heads'(@list(callable), -list(callable))
% '$lgt_compile_predicate_heads'(@callable, -callable)

'$lgt_compile_predicate_heads'(Heads, THeads) :-
	'$lgt_compile_predicate_heads'(Heads, _, THeads, _).



% '$lgt_compile_predicate_heads'(@list(callable), -list(callable), @term)
% '$lgt_compile_predicate_heads'(@callable, -callable, @term)

'$lgt_compile_predicate_heads'(Heads, THeads, Ctx) :-
	'$lgt_compile_predicate_heads'(Heads, _, THeads, Ctx).



% '$lgt_decompile_predicate_heads'(+list(callable), ?entity_identifier, ?atom, -list(callable))
% '$lgt_decompile_predicate_heads'(+callable, ?entity_identifier, ?atom, -callable)
%
% decompiles the predicate heads used for compiled predicates;
%
% all the compiled predicate heads must refer to the same entity 
% (which must be loaded) in order for this predicate to succeed

'$lgt_decompile_predicate_heads'(_, Entity, _, _) :-
	nonvar(Entity),
	\+ callable(Entity),
	throw(type_error(entity_identifier, Entity)).

'$lgt_decompile_predicate_heads'(THeads, Entity, Type, Heads) :-
	'$lgt_decompile_predicate_heads'(THeads, Entity, Type, _, Heads).


'$lgt_decompile_predicate_heads'(THeads, _, _, _, _) :-
	var(THeads),
	throw(instantiation_error).

'$lgt_decompile_predicate_heads'([], _, _, _, []) :-
	!.

'$lgt_decompile_predicate_heads'([THead| THeads], Entity, Type, Prefix, [Head| Heads]) :-
	!,
	'$lgt_decompile_predicate_heads'(THead, Entity, Type, Prefix, Head),
	'$lgt_decompile_predicate_heads'(THeads, Entity, Type, Prefix, Heads).

'$lgt_decompile_predicate_heads'(THead, Entity, Type, Prefix, Head) :-
	callable(THead),
	functor(THead, TFunctor, TArity),
	(	var(Prefix) ->
		(	'$lgt_current_object_'(Entity, Prefix, _, _, _, _, _, _, _, _, _),
			Type = object
		;	'$lgt_current_category_'(Entity, Prefix, _, _, _, _),
			Type = category
		;	'$lgt_current_protocol_'(Entity, Prefix, _, _, _),
			Type = protocol
		)
	;	true
	),
	atom_concat(Prefix, Functor, TFunctor),
	Arity is TArity - 1,		% subtract execution context argument
	Arity >= 0,
	functor(Head, Functor, Arity),
	'$lgt_unify_head_thead_args'(Arity, Head, THead),
	!.



% '$lgt_decompile_predicate_heads'(+list(callable), -list(callable))
% '$lgt_decompile_predicate_heads'(+callable, -callable)

'$lgt_decompile_predicate_heads'(THeads, Heads) :-
	'$lgt_decompile_predicate_heads'(THeads, _, _, Heads).



% '$lgt_decompile_predicate_heads'(+list(callable), ?entity_identifier, -list(callable))
% '$lgt_decompile_predicate_heads'(+callable, ?entity_identifier, -callable)

'$lgt_decompile_predicate_heads'(THeads, Entity, Heads) :-
	'$lgt_decompile_predicate_heads'(THeads, Entity, _, Heads).



% deprecated

'$lgt_decompile_predicate_head'(THead, Entity, Type, Head) :-
	'$lgt_decompile_predicate_heads'(THead, Entity, Type, _, Head).



% '$lgt_compile_predicate_indicators'(+list(predicate_indicator), ?entity_identifier, -list(predicate_indicator))
% '$lgt_compile_predicate_indicators'(+predicate_indicator, ?entity_identifier, -predicate_indicator)
%
% translates a single predicate indicator, a conjunction of predicate indicators, or a list
% of predicate indicators; used mainly in hook objects for processing proprietary directives
%
% the predicate indicators are compiled in the context of the specified entity or in the context
% of the entity being compiled when the entity argument is not instantiated

'$lgt_compile_predicate_indicators'(_, Entity, _) :-
	nonvar(Entity),
	\+ callable(Entity),
	throw(type_error(entity_identifier, Entity)).

'$lgt_compile_predicate_indicators'(PIs, Entity, TPIs) :-
	(	var(Entity) ->
		'$lgt_pp_entity'(_, Entity, Prefix, _, _)
	;	'$lgt_current_object_'(Entity, Prefix, _, _, _, _, _, _, _, _, _) ->
		true
	;	'$lgt_current_category_'(Entity, Prefix, _, _, _, _) ->
		true
	;	'$lgt_current_protocol_'(Entity, Prefix, _, _, _) ->
		true
	;	fail
	),
	'$lgt_compile_predicate_indicators_aux'(PIs, Prefix, TPIs).


'$lgt_compile_predicate_indicators_aux'(PIs, _, _) :-
	var(PIs),
	throw(instantiation_error).

'$lgt_compile_predicate_indicators_aux'([], _, []) :-
	!.

'$lgt_compile_predicate_indicators_aux'([PI| PIs], Prefix, [TPI| TPIs]) :-
	!,
	'$lgt_compile_predicate_indicators_aux'(PI, Prefix, TPI),
	'$lgt_compile_predicate_indicators_aux'(PIs, Prefix, TPIs).

'$lgt_compile_predicate_indicators_aux'((PI, PIs), Prefix, (TPI, TPIs)) :-
	!,
	'$lgt_compile_predicate_indicators_aux'(PI, Prefix, TPI),
	'$lgt_compile_predicate_indicators_aux'(PIs, Prefix, TPIs).

'$lgt_compile_predicate_indicators_aux'(PI, Prefix, TFunctor/TArity) :-
	(	'$lgt_valid_predicate_indicator'(PI, Functor, Arity) ->
		'$lgt_construct_predicate_indicator'(Prefix, Functor/Arity, TFunctor/TArity)
	;	'$lgt_valid_non_terminal_indicator'(PI, Functor, _, ExtArity) ->
		'$lgt_construct_predicate_indicator'(Prefix, Functor/ExtArity, TFunctor/TArity)
	;	throw(type_error(predicate_indicator, PI))
	).



% '$lgt_compile_predicate_indicators'(+list(predicate_indicator), -list(predicate_indicator))
% '$lgt_compile_predicate_indicators'(+predicate_indicator, -predicate_indicator)

'$lgt_compile_predicate_indicators'(Heads, THeads) :-
	'$lgt_compile_predicate_indicators'(Heads, _, THeads).



% '$lgt_construct_predicate_indicator'(+atom, +predicate_indicator, -predicate_indicator)
%
% constructs the predicate indicator used for a compiled predicate

'$lgt_construct_predicate_indicator'(Prefix, Functor/Arity, TFunctor/TArity) :-
	atom_concat(Prefix, Functor, TFunctor),
	TArity is Arity + 1.		% add execution context argument



% '$lgt_decompile_predicate_indicators'(+list(predicate_indicator), ?entity_identifier, ?atom, -list(predicate_indicator))
% '$lgt_decompile_predicate_indicators'(+predicate_indicator, ?entity_identifier, ?atom, -predicate_indicator)
%
% reverses the predicate indicator used for a compiled predicate or a list of compiled predicates;
%
% all the compiled predicate indicators must refer to the same entity
% (which must be loaded) in order for this predicate to succeed

'$lgt_decompile_predicate_indicators'(_, Entity, _, _) :-
	nonvar(Entity),
	\+ callable(Entity),
	throw(type_error(entity_identifier, Entity)).

'$lgt_decompile_predicate_indicators'(TPIs, Entity, Type, PIs) :-
	'$lgt_decompile_predicate_indicators'(TPIs, Entity, Type, _, PIs).


'$lgt_decompile_predicate_indicators'(TPIs, _, _, _, _) :-
	var(TPIs),
	throw(instantiation_error).

'$lgt_decompile_predicate_indicators'([], _, _, _, []) :-
	!.

'$lgt_decompile_predicate_indicators'([TPI| TPIs], Entity, Type, Prefix, [PI| PIs]) :-
	!,
	'$lgt_decompile_predicate_indicators'(TPI, Entity, Type, Prefix, PI),
	'$lgt_decompile_predicate_indicators'(TPIs, Entity, Type, Prefix, PIs).

'$lgt_decompile_predicate_indicators'(TFunctor/TArity, Entity, Type, Prefix, Functor/Arity) :-
	(	var(Prefix) ->
		(	'$lgt_current_object_'(Entity, Prefix, _, _, _, _, _, _, _, _, _),
			Type = object
		;	'$lgt_current_category_'(Entity, Prefix, _, _, _, _),
			Type = category
		;	'$lgt_current_protocol_'(Entity, Prefix, _, _, _),
			Type = protocol
		)
	;	true
	),
	atom_concat(Prefix, Functor, TFunctor),
	Arity is TArity - 1,		% subtract execution context argument
	Arity >= 0,
	!.



% '$lgt_decompile_predicate_indicators'(+list(predicate_indicator), -list(predicate_indicator))
% '$lgt_decompile_predicate_indicators'(+predicate_indicator, -predicate_indicator)

'$lgt_decompile_predicate_indicators'(TPIs, PIs) :-
	'$lgt_decompile_predicate_indicators'(TPIs, _, _, PIs).



% '$lgt_decompile_predicate_indicators'(+list(predicate_indicator), ?entity_identifier, -list(predicate_indicator))
% '$lgt_decompile_predicate_indicators'(+predicate_indicator, ?entity_identifier, -predicate_indicator)

'$lgt_decompile_predicate_indicators'(TPIs, Entity, PIs) :-
	'$lgt_decompile_predicate_indicators'(TPIs, Entity, _, PIs).



% deprecated

'$lgt_decompile_predicate_indicator'(TFunctor/TArity, Entity, Type, Functor/Arity) :-
	'$lgt_decompile_predicate_indicators'(TFunctor/TArity, Entity, Type, Functor/Arity).



% '$lgt_compile_hooks'(+callable)
%
% compiles the user-defined compiler hook

'$lgt_compile_hooks'(Obj) :-
	(	Obj == user ->
		TermExpansionGoal = term_expansion(Term, ExpandedTerm),
		GoalExpansionGoal = goal_expansion(Term, ExpandedTerm)
	;	'$lgt_tr_msg'(term_expansion(Term, ExpandedTerm), Obj, TermExpansionGoal, user),
		'$lgt_tr_msg'(goal_expansion(Term, ExpandedTerm), Obj, GoalExpansionGoal, user)
	),
	retractall('$lgt_hook_term_expansion_'(_, _)),
	assertz(('$lgt_hook_term_expansion_'(Term, ExpandedTerm) :- catch(TermExpansionGoal, _, fail))),
	retractall('$lgt_hook_goal_expansion_'(_, _)),
	assertz(('$lgt_hook_goal_expansion_'(Term, ExpandedTerm) :- catch(GoalExpansionGoal, _, fail))).



% '$lgt_built_in'(+callable)
%
% checks if the argument is either a Prolog or Logtalk built-in predicate

'$lgt_built_in'(Pred) :-
	(	'$lgt_pl_built_in'(Pred) ->
		true
	;	'$lgt_lgt_built_in'(Pred)
	).



% '$lgt_pl_built_in'(+callable)
%
% either host Prolog native built-ins or missing ISO built-ins
% that we have defined in the correspondent config file

'$lgt_pl_built_in'(Pred) :-
	\+ '$lgt_lgt_built_in'(Pred),	% Logtalk built-ins may also have the property built_in
	'$lgt_predicate_property'(Pred, built_in),
	!.

'$lgt_pl_built_in'(Pred) :-
	'$lgt_iso_predicate'(Pred).		% ISO Prolog built-in predicate (defined in the config files)



% logtalk built-in methods
%
% '$lgt_built_in_method'(+callable, ?scope, ?callable, ?integer)

% reflection methods
'$lgt_built_in_method'(current_predicate(_), p(p(p)), no, 1) :- !.
'$lgt_built_in_method'(predicate_property(_, _), p(p(p)), no, 1) :- !.
% database methods
'$lgt_built_in_method'(abolish(_), p(p(p)), abolish(::), 1) :- !.
'$lgt_built_in_method'(asserta(_), p(p(p)), asserta(::), 1) :- !.
'$lgt_built_in_method'(assertz(_), p(p(p)), assertz(::), 1) :- !.
'$lgt_built_in_method'(clause(_, _), p(p(p)), clause(::, *), 1) :- !.
'$lgt_built_in_method'(retract(_), p(p(p)), retract(::), 1) :- !.
'$lgt_built_in_method'(retractall(_), p(p(p)), retractall(::), 1) :- !.
% term expansion methods
'$lgt_built_in_method'(expand_term(_, _), p(p(p)), no, 1) :- !.
'$lgt_built_in_method'(expand_goal(_, _), p(p(p)), no, 1) :- !.
% DCGs methods
'$lgt_built_in_method'(phrase(_, _), p, phrase(2, *), 1) :- !.
'$lgt_built_in_method'(phrase(_, _, _), p, phrase(2, *, *), 1) :- !.
% meta-calls plus logic and control methods
'$lgt_built_in_method'(\+ _, p, \+ (0), 1) :- !.
'$lgt_built_in_method'(Method, p, Meta, 1) :-  % call/1-N
	compound(Method),
	functor(Method, call, Arity),
	Arity > 0,
	!,
	functor(Meta, call, Arity),
	Closure is Arity - 1,
	arg(1, Meta, Closure),
	'$lgt_lgt_meta_predicate_call_n_args'(Arity, Meta).
'$lgt_built_in_method'(once(_), p, once(0), 1) :- !.
'$lgt_built_in_method'(ignore(_), p, ignore(0), 1) :- !.
% exception handling methods
'$lgt_built_in_method'(catch(_, _, _), p, catch(0, *, 0), 1) :- !.
'$lgt_built_in_method'(throw(_), p, no, 1) :- !.
% execution context methods
'$lgt_built_in_method'(parameter(_, _), p, no, 1) :- !.
'$lgt_built_in_method'(self(_), p, no, 1) :- !.
'$lgt_built_in_method'(sender(_), p, no, 1) :- !.
'$lgt_built_in_method'(this(_), p, no, 1) :- !.
% all solutions methods
'$lgt_built_in_method'(bagof(_, _, _), p, bagof(*, 0, *), 1) :- !.
'$lgt_built_in_method'(findall(_, _, _), p, findall(*, 0, *), 1) :- !.
'$lgt_built_in_method'(forall(_, _), p, forall(0, 0), 1) :- !.
'$lgt_built_in_method'(setof(_, _, _), p, setof(*, 0, *), 1) :- !.


'$lgt_lgt_meta_predicate_call_n_args'(1, _) :-
	!.
'$lgt_lgt_meta_predicate_call_n_args'(N, Meta) :-
	arg(N, Meta, *),
	N2 is N - 1,
	'$lgt_lgt_meta_predicate_call_n_args'(N2, Meta).



%'$lgt_lgt_directive'(+atom, +integer)
%
% valid Logtalk directives

'$lgt_lgt_directive'(Functor, Arity) :-
	'$lgt_lgt_opening_directive'(Functor, Arity),
	!.

'$lgt_lgt_directive'(Functor, Arity) :-
	'$lgt_lgt_closing_directive'(Functor, Arity),
	!.

'$lgt_lgt_directive'(Functor, Arity) :-
	'$lgt_lgt_entity_directive'(Functor, Arity),
	!.

'$lgt_lgt_directive'(Functor, Arity) :-
	'$lgt_lgt_predicate_directive'(Functor, Arity).


'$lgt_lgt_opening_directive'(object, N) :-
	N >= 1, N =< 5.
'$lgt_lgt_opening_directive'(category, N) :-
	N >= 1, N =< 3.
'$lgt_lgt_opening_directive'(protocol, N) :-
	N >= 1, N =< 2.
'$lgt_lgt_opening_directive'(module, N) :-				% Prolog module directives; module/3 directives
	N >= 1, N =< 3.										% are not supported but must be recognized as 
														% entity opening directives

'$lgt_lgt_closing_directive'(end_object, 0).
'$lgt_lgt_closing_directive'(end_category, 0).
'$lgt_lgt_closing_directive'(end_protocol, 0).


'$lgt_lgt_entity_directive'(encoding, 1).
'$lgt_lgt_entity_directive'(calls, N) :-
	N >= 1.
'$lgt_lgt_entity_directive'(uses, N) :-
	N >= 1, N =< 2.
'$lgt_lgt_entity_directive'(use_module, 2).				% Prolog module directive
'$lgt_lgt_entity_directive'((initialization), 1).
'$lgt_lgt_entity_directive'((dynamic), 0).
'$lgt_lgt_entity_directive'(op, 3).
'$lgt_lgt_entity_directive'(info, 1).
'$lgt_lgt_entity_directive'(synchronized, 0).
'$lgt_lgt_entity_directive'(threaded, 0).
'$lgt_lgt_entity_directive'(set_logtalk_flag, 2).


'$lgt_lgt_predicate_directive'(synchronized, N) :-
	N >= 1.
'$lgt_lgt_predicate_directive'((dynamic), N) :-
	N >= 1.
'$lgt_lgt_predicate_directive'(metapredicate, N) :-		% deprecated Logtalk directive
	N >= 1.
'$lgt_lgt_predicate_directive'((meta_predicate), N) :-	% Logtalk directive
	N >= 1.
'$lgt_lgt_predicate_directive'((discontiguous), N) :-
	N >= 1.
'$lgt_lgt_predicate_directive'((public), N) :-
	N >= 1.
'$lgt_lgt_predicate_directive'(protected, N) :-
	N >= 1.
'$lgt_lgt_predicate_directive'(private, N) :-
	N >= 1.
'$lgt_lgt_predicate_directive'((export), N) :-			% Prolog module directive
	N >= 1.
'$lgt_lgt_predicate_directive'(reexport, 2).			% Prolog module directive
'$lgt_lgt_predicate_directive'((mode), 2).
'$lgt_lgt_predicate_directive'(info, 2).
'$lgt_lgt_predicate_directive'(alias, 3).
'$lgt_lgt_predicate_directive'((multifile), N) :-
	N >= 1.
'$lgt_lgt_predicate_directive'((coinductive), N) :-
	N >= 1.
'$lgt_lgt_predicate_directive'(annotation, N) :-		% experimental directive
	N >= 1.



% conditional compilation directives

'$lgt_lgt_cc_directive'(Term) :-
	nonvar(Term),
	Term = (:- Directive),
	nonvar(Directive),
	functor(Directive, Functor, Arity),
	'$lgt_lgt_cc_directive'(Functor, Arity).


'$lgt_lgt_cc_directive'(if,    1).
'$lgt_lgt_cc_directive'(elif,  1).
'$lgt_lgt_cc_directive'(else,  0).
'$lgt_lgt_cc_directive'(endif, 0).



% utility predicates used during compilation of Logtalk entities to store and
% access compilation context information (represented by a compound term)

'$lgt_comp_ctx'(ctx(_, _, _, _, _, _, _, _, _, _)).

'$lgt_comp_ctx'(ctx(Head, Sender, This, Self, Prefix, MetaVars, MetaCallCtx, ExCtx, Mode, Stack), Head, Sender, This, Self, Prefix, MetaVars, MetaCallCtx, ExCtx, Mode, Stack).

'$lgt_comp_ctx_head'(ctx(Head, _, _, _, _, _, _, _, _, _), Head).		% head of the clause being compiled

'$lgt_comp_ctx_sender'(ctx(_, Sender, _, _, _, _, _, _, _, _), Sender).

'$lgt_comp_ctx_this'(ctx(_, _, This, _, _, _, _, _, _, _), This).

'$lgt_comp_ctx_self'(ctx(_, _, _, Self, _, _, _, _, _, _), Self).

'$lgt_comp_ctx_prefix'(ctx(_, _, _, _, Prefix, _, _, _, _, _), Prefix).	% entity prefix used to avoid predicate name conflicts

'$lgt_comp_ctx_meta_vars'(ctx(_, _, _, _, _, MetaVars, _, _, _, _), MetaVars).

'$lgt_comp_ctx_meta_call_ctx'(ctx(_, _, _, _, _, _, MetaCallCtx, _, _, _), MetaCallCtx).

'$lgt_comp_ctx_exec_ctx'(ctx(_, _, _, _, _, _, _, ExCtx, _, _), ExCtx).

'$lgt_comp_ctx_mode'(ctx(_, _, _, _, _, _, _, _, Mode, _), Mode).		% mode is "compile(regular)", "compile(aux)", or "runtime"

'$lgt_comp_ctx_stack'(ctx(_, _, _, _, _, _, _, _, _, Stack), Stack).	% stack of coinductive hypothesis (ancestor goals)

'$lgt_comp_ctx_stack_new_stack'(ctx(Head, Sender, This, Self, Prefix, MetaVars, MetaCallCtx, _, Mode, _), NewStack, ctx(Head, Sender, This, Self, Prefix, MetaVars, MetaCallCtx, _, Mode, NewStack)).



% '$lgt_ctg_parameter'(This, Ctg, Arg, Value)
%
% runtime access to category parameters

'$lgt_ctg_parameter'(This, Ctg, Arg, Value) :-
	'$lgt_imports_category_'(This, Ctg, _), !,
	arg(Arg, Ctg, Value).



% '$lgt_entity_template'(@callable, -callable)
%
% constructs a template for an entity identifier;
% (which, in most cases, is an atom)

'$lgt_entity_template'(Term, Term) :-
	atom(Term),
	!.

'$lgt_entity_template'(Term, Template) :-
	functor(Term, Functor, Arity),
	functor(Template, Functor, Arity).



% '$lgt_term_template'(@callable, -callable)
%
% constructs a template for a callable term

'$lgt_term_template'(Term, Template) :-
	functor(Term, Functor, Arity),
	functor(Template, Functor, Arity).



% '$lgt_flatten_list'(+list, -list)
%
% flattens a list of terms

'$lgt_flatten_list'([[A|B]], [A|B]) :-		% list containing a single list
	!.

'$lgt_flatten_list'([[]], []) :-			% list containing a single empty list
	!.

'$lgt_flatten_list'([(A, B)], [A|BB]) :-	% list containing a single element,
	!,										% which is a sequence: (A, B, ...)
	'$lgt_flatten_list'([B], BB).

'$lgt_flatten_list'([A|B], [A|B]) :-		% already flattened list
	!.

'$lgt_flatten_list'([], []).				% empty  list



% '$lgt_valid_predicate_indicator'(+nonvar, -atom, -integer)
%
% valid predicate indicator

'$lgt_valid_predicate_indicator'(Functor/Arity, Functor, Arity) :-
	atom(Functor),
	integer(Arity),
	Arity >= 0.



% '$lgt_valid_non_terminal_indicator'(+nonvar, -atom, -integer, -integer)
%
% valid grammar rule non-terminal indicator; the last argument is the
% arity of the corresponding predicate

'$lgt_valid_non_terminal_indicator'(Functor//Arity, Functor, Arity, ExtArity) :-
	atom(Functor),
	integer(Arity),
	Arity >= 0,
	ExtArity is Arity + 2.



% '$lgt_valid_predicate_or_non_terminal_indicator'(+nonvar, -atom, -integer)
%
% valid predicate indicator or grammar rule indicator

'$lgt_valid_predicate_or_non_terminal_indicator'(Functor/Arity, Functor, Arity) :-
	atom(Functor),
	integer(Arity),
	Arity >= 0.

'$lgt_valid_predicate_or_non_terminal_indicator'(Functor//Arity, Functor, Arity) :-
	atom(Functor),
	integer(Arity),
	Arity >= 0.



% '$lgt_valid_ref_scope'(@nonvar, -atom)

'$lgt_valid_ref_scope'(Ref, Scope) :-
	(	Ref = (Scope::_) ->
		'$lgt_scope'(Scope, _)
	;	Scope = (public)
	).



% '$lgt_valid_protocol_ref'(@nonvar, -atom)

'$lgt_valid_protocol_ref'(Ref, Ptc) :-
	(	Ref = (_::Ptc) ->
		true
	;	Ptc = Ref
	),
	atom(Ptc).



% '$lgt_valid_object_ref'(@nonvar, -atom)

'$lgt_valid_object_ref'(Ref, Obj) :-
	(	Ref = (_::Obj) ->
		true
	;	Obj = Ref
	),
	callable(Obj).



% '$lgt_valid_category_ref'(@nonvar, -atom)

'$lgt_valid_category_ref'(Ref, Ctg) :-
	(	Ref = (_::Ctg) ->
		true
	;	Ctg = Ref
	),
	callable(Ctg).



% '$lgt_check_closure'(@nonvar, @compilation_context)
%
% checks that a closure meta-argument is valid

'$lgt_check_closure'(Closure, _) :-
	var(Closure),
	!.

'$lgt_check_closure'(Closure, _) :-
	\+ callable(Closure),
	throw(type_error(callable, Closure)).

'$lgt_check_closure'(Free/Goal, Ctx) :-
	'$lgt_check_lambda_expression'(Free/Goal, Ctx),
	!.

'$lgt_check_closure'(Parameters>>Goal, Ctx) :-
	'$lgt_check_lambda_expression'(Parameters>>Goal, Ctx),
	!.

'$lgt_check_closure'({Closure}, _) :-
	nonvar(Closure),
	\+ callable(Closure),
	throw(type_error(callable, Closure)).

'$lgt_check_closure'(Object::Closure, _) :-
	(	nonvar(Object),
		\+ callable(Object),
		throw(type_error(object_identifier, Object))
	;	nonvar(Closure),
		\+ callable(Closure),
		throw(type_error(callable, Closure))
	).

'$lgt_check_closure'(::Closure, _) :-
	nonvar(Closure),
	\+ callable(Closure),
	throw(type_error(callable, Closure)).

'$lgt_check_closure'(^^Closure, _) :-
	nonvar(Closure),
	\+ callable(Closure),
	throw(type_error(callable, Closure)).

'$lgt_check_closure'(Object<<Closure, _) :-
	(	nonvar(Object),
		\+ callable(Object),
		throw(type_error(object_identifier, Object))
	;	nonvar(Closure),
		\+ callable(Closure),
		throw(type_error(callable, Closure))
	).

'$lgt_check_closure'(':'(Module, Closure), _) :-
	(	nonvar(Module),
		\+ atom(Module),
		throw(type_error(module_identifier, Module))
	;	nonvar(Closure),
		\+ callable(Closure),
		throw(type_error(callable, Closure))
	).

'$lgt_check_closure'(_, _).



% '$lgt_check_lambda_expression'(@nonvar, @compilation_context)
%
% checks that a closure meta-argument is valid

'$lgt_check_lambda_expression'(Free/Parameters>>Goal, Ctx) :-
	!,
	(	nonvar(Free),
		\+ (functor(Free, {}, Arity), Arity =< 1),
		throw(type_error(curly_bracketed_term, Free))
	;	nonvar(Parameters),
		\+ '$lgt_is_proper_list'(Parameters),
		throw(type_error(list, Parameters))
	;	nonvar(Goal),
		\+ callable(Goal),
		throw(type_error(callable, Goal))
	;	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
		nonvar(Free),
		nonvar(Parameters),
		nonvar(Goal),
		'$lgt_check_lambda_expression_unclassified_vars'(Free/Parameters>>Goal),
		'$lgt_check_lambda_expression_mixed_up_vars'(Free/Parameters>>Goal)
	).

'$lgt_check_lambda_expression'(Free/Goal, _) :-
	(	nonvar(Free),
		\+ (functor(Free, {}, Arity), Arity =< 1),
		throw(type_error(curly_bracketed_term, Free))
	;	nonvar(Goal),
		\+ callable(Goal),
		throw(type_error(callable, Goal))
	).

'$lgt_check_lambda_expression'(Parameters>>Goal, Ctx) :-
	(	nonvar(Parameters),
		\+ '$lgt_is_proper_list'(Parameters),
		throw(type_error(list, Parameters))
	;	nonvar(Goal),
		\+ callable(Goal),
		throw(type_error(callable, Goal))
	;	'$lgt_comp_ctx_mode'(Ctx, compile(_)),
		nonvar(Parameters),
		nonvar(Goal),
		'$lgt_check_lambda_expression_unclassified_vars'(Parameters>>Goal)
	).

'$lgt_check_lambda_expression'(_, _).


'$lgt_check_lambda_expression_unclassified_vars'(Parameters>>Goal) :-
	'$lgt_check_lambda_expression_unclassified_vars'(Goal, GoalVars),
	term_variables(Parameters, ParameterVars),
	'$lgt_var_subtract'(GoalVars, ParameterVars, UnqualifiedVars),
	(	UnqualifiedVars \== [],
		\+ '$lgt_compiler_flag'(report, off) ->
		'$lgt_report_warning_in_new_line',
		'$lgt_inc_compile_warnings_counter',
		write('%         WARNING!  Unclassified variables in lambda expression: '), nl,
		\+ \+ (
			numbervars(Parameters>>Goal, 0, _),
			write('%                       '),
			write_term(Parameters>>Goal, [quoted(true), ignore_ops(false), numbervars(true)]), nl
		),
		'$lgt_pp_entity'(Type, Entity, _, _, _),
		'$lgt_report_warning_full_context'(Type, Entity)
	;	true
	).


'$lgt_check_lambda_expression_unclassified_vars'(Parameters>>Goal, UnqualifiedVars) :-
	!,
	'$lgt_check_lambda_expression_unclassified_vars'(Goal, GoalVars),
	term_variables(Parameters, ParameterVars),
	'$lgt_var_subtract'(GoalVars, ParameterVars, UnqualifiedVars).

'$lgt_check_lambda_expression_unclassified_vars'(Goal, UnqualifiedVars) :-
	term_variables(Goal, UnqualifiedVars).


'$lgt_var_subtract'([], _, []).

'$lgt_var_subtract'([Head| Tail], List, Rest) :-
	(	'$lgt_var_memberchk'(Head, List) ->
		'$lgt_var_subtract'(Tail, List, Rest)
	;	Rest = [Head| Tail2],
		'$lgt_var_subtract'(Tail, List, Tail2)
	).


'$lgt_var_memberchk'(Element, [Head| Tail]) :-
	(	Element == Head ->
		true
	;	'$lgt_var_memberchk'(Element, Tail)
	).


'$lgt_check_lambda_expression_mixed_up_vars'(Free/Parameters>>Goal) :-
	term_variables(Free, FreeVars),
	term_variables(Parameters, ParameterVars),
	'$lgt_intersection'(FreeVars, ParameterVars, MixedUpVars),
	(	MixedUpVars \== [],
		\+ '$lgt_compiler_flag'(report, off) ->
		'$lgt_report_warning_in_new_line',
		'$lgt_inc_compile_warnings_counter',
		write('%         WARNING!  Variables in lambda expression have dual role: '), nl,
		\+ \+ (
			numbervars(Free/Parameters>>Goal, 0, _),
			write('%                       '),
			write_term(Free/Parameters>>Goal, [quoted(true), ignore_ops(false), numbervars(true)]), nl
		),
		'$lgt_pp_entity'(Type, Entity, _, _, _),
		'$lgt_report_warning_full_context'(Type, Entity)
	;	true
	).


'$lgt_intersection'(_, [], []) :- !.

'$lgt_intersection'([], _, []) :- !.

'$lgt_intersection'([Head1| Tail1], [Head2| Tail2], Intersection) :-
	compare(Order, Head1, Head2),
	'$lgt_intersection'(Order, Head1, Tail1, Head2, Tail2, Intersection).


'$lgt_intersection'(=, Head,  Tail1, _,     Tail2, [Head| Intersection]) :-
	'$lgt_intersection'(Tail1, Tail2, Intersection).

'$lgt_intersection'(<, _,     Tail1, Head2, Tail2, Intersection) :-
	'$lgt_intersection'(Tail1, [Head2| Tail2], Intersection).

'$lgt_intersection'(>, Head1, Tail1, _,     Tail2, Intersection) :-
	'$lgt_intersection'([Head1|Tail1], Tail2, Intersection).



% '$lgt_check_op_directive_args'(@term, @term, @term)
%
% checks that the arguments of a op/3 directive are valid

'$lgt_check_op_directive_args'(Priority, _, _) :-
	var(Priority),
	throw(instantiation_error).

'$lgt_check_op_directive_args'(_, Spec, _) :-
	var(Spec),
	throw(instantiation_error).

'$lgt_check_op_directive_args'(_, _, Operators) :-
	var(Operators),
	throw(instantiation_error).

'$lgt_check_op_directive_args'(Priority, _, _) :-
	\+ integer(Priority),
	throw(type_error(integer, Priority)).

'$lgt_check_op_directive_args'(Priority, _, _) :-
	(Priority < 0; Priority > 1200),
	throw(domain_error(operator_priority, Priority)).

'$lgt_check_op_directive_args'(_, Spec, _) :-
	\+ atom(Spec),
	throw(type_error(atom, Spec)).

'$lgt_check_op_directive_args'(_, Spec, _) :-
	Spec \== fx,
	Spec \== fy,
	Spec \== xfx,
	Spec \== xfy,
	Spec \== yfx,
	Spec \== xf,
	Spec \== yf,
	throw(domain_error(operator_specifier, Spec)).

'$lgt_check_op_directive_args'(_, _, ',') :-
	throw(permission_error(modify, operator, ',')).

'$lgt_check_op_directive_args'(_, _, Operators) :-
	\+ atom(Operators),
	\+ '$lgt_is_proper_list'(Operators),
	throw(type_error(list, Operators)).

'$lgt_check_op_directive_args'(_, _, Operators) :-
	'$lgt_check_op_directive_operator_names'(Operators).


'$lgt_check_op_directive_operator_names'(Operator) :-
	var(Operator),
	throw(instantiation_error).

'$lgt_check_op_directive_operator_names'(',') :-
	throw(permission_error(modify, operator, ',')).

'$lgt_check_op_directive_operator_names'(Operator) :-
	atom(Operator),
	!.

'$lgt_check_op_directive_operator_names'([]) :-
	!.

'$lgt_check_op_directive_operator_names'([Operator| Operators]) :-
	!,
	'$lgt_check_op_directive_operator_names'(Operator),
	'$lgt_check_op_directive_operator_names'(Operators).

'$lgt_check_op_directive_operator_names'(Operator) :-
	throw(type_error(atom, Operator)).



% '$lgt_same_op_class'(?atom, ?atom)

'$lgt_same_op_class'(fx, fx).
'$lgt_same_op_class'(fx, fy).

'$lgt_same_op_class'(fy, fx).
'$lgt_same_op_class'(fy, fy).

'$lgt_same_op_class'(xf, xf).
'$lgt_same_op_class'(xf, yf).

'$lgt_same_op_class'(yf, xf).
'$lgt_same_op_class'(yf, yf).

'$lgt_same_op_class'(xfx, xfx).
'$lgt_same_op_class'(xfx, xfy).
'$lgt_same_op_class'(xfx, yfx).

'$lgt_same_op_class'(xfy, xfx).
'$lgt_same_op_class'(xfy, xfy).
'$lgt_same_op_class'(xfy, yfx).

'$lgt_same_op_class'(yfx, xfx).
'$lgt_same_op_class'(yfx, xfy).
'$lgt_same_op_class'(yfx, yfx).



% '$lgt_valid_meta_predicate_template'(+nonvar)

'$lgt_valid_meta_predicate_template'(Pred) :-
	Pred =.. [_| Args],
	'$lgt_valid_meta_predicate_template_args'(Args).


'$lgt_valid_meta_predicate_template_args'([]).

'$lgt_valid_meta_predicate_template_args'([Arg| Args]) :-
	once((
		Arg == (::)						% meta-argument but not called
	;	Arg == (*)						% non meta-argument
	;	integer(Arg), Arg >= 0			% goal or closure
	;	Arg == (/)						% predicate indicator
	;	Arg = [N], integer(N), N >= 0	% list of goals/closures
	;	Arg == [/]						% list of predicate indicators
	)),
	'$lgt_valid_meta_predicate_template_args'(Args).



% '$lgt_valid_annotation_template'(+nonvar)

'$lgt_valid_annotation_template'(Pred) :-
	Pred =.. [_, Arg1, Arg2],
	nonvar(Arg1),
	nonvar(Arg2),
	'$lgt_valid_annotation_template_args'(Arg1, Arg2).


'$lgt_valid_annotation_template_args'(*, 0).			% right annotation operand is a goal
'$lgt_valid_annotation_template_args'(0, *).			% left annotation operand is a goal
'$lgt_valid_annotation_template_args'(0, 0).			% both annotation operands are goals



% '$lgt_valid_mode_template'(+nonvar)

'$lgt_valid_mode_template'(Pred) :-
	Pred =.. [_| Args],
	'$lgt_valid_mode_template_args'(Args).


'$lgt_valid_mode_template_args'([]).

'$lgt_valid_mode_template_args'([Arg| Args]) :-
	nonvar(Arg),
	functor(Arg, Functor, Arity),
	Arity =< 1,
	'$lgt_pred_arg_instantiation_mode'(Functor),
	'$lgt_valid_mode_template_args'(Args).



% '$lgt_pred_arg_instantiation_mode'(@nonvar)

'$lgt_pred_arg_instantiation_mode'(?).					% unspecified, can be input, output or both input and output
'$lgt_pred_arg_instantiation_mode'(+).					% instantiated on predicate call, can be further instantiated by the predicate call
'$lgt_pred_arg_instantiation_mode'(-).					% non-instantiated (i.e. a variable) on predicate call
'$lgt_pred_arg_instantiation_mode'(@).					% not modified (i.e. not further instantiated) by the predicate call



% '$lgt_valid_number_of_solutions'(@term)

'$lgt_valid_number_of_solutions'(Solutions) :-
	atom(Solutions),
	'$lgt_pred_number_of_solutions'(Solutions).



% '$lgt_pred_number_of_solutions'(+atom)

'$lgt_pred_number_of_solutions'(zero).					% calling the predicate using the specified mode always fails
'$lgt_pred_number_of_solutions'(one).					% calling the predicate using the specified mode always succeeds once
'$lgt_pred_number_of_solutions'(zero_or_one).			% calling the predicate using the specified mode may succeed once or fail
'$lgt_pred_number_of_solutions'(zero_or_more).			% calling the predicate using the specified mode may fail or succeed multiple times
'$lgt_pred_number_of_solutions'(one_or_more).			% calling the predicate using the specified mode always succeed at least once
'$lgt_pred_number_of_solutions'(error).					% calling the predicate using the specified mode throws an error



% '$lgt_valid_predicate_property'(@nonvar)

'$lgt_valid_predicate_property'((public)).				% public predicate
'$lgt_valid_predicate_property'(protected).				% protected predicate
'$lgt_valid_predicate_property'(private).				% private predicate
'$lgt_valid_predicate_property'((dynamic)).				% dynamic predicate
'$lgt_valid_predicate_property'(static).				% static predicate
'$lgt_valid_predicate_property'(logtalk).				% predicate is defined in Logtalk
'$lgt_valid_predicate_property'(prolog).				% predicate is defined in Prolog
'$lgt_valid_predicate_property'(declared_in(_)).		% entity containing the predicate scope directive
'$lgt_valid_predicate_property'(defined_in(_)).			% object or category containing the predicate definition
'$lgt_valid_predicate_property'(meta_predicate(_)).		% meta-predicate template
'$lgt_valid_predicate_property'(built_in).				% built-in predicate
'$lgt_valid_predicate_property'(alias_of(_)).			% predicate is an alias of another predicate
'$lgt_valid_predicate_property'((multifile)).			% clauses for the predicate can be defined within multiple entities
'$lgt_valid_predicate_property'(non_terminal(_)).		% predicate version of a non-terminal
'$lgt_valid_predicate_property'(synchronized).			% calls to the predicate are synchronized



% '$lgt_valid_object_property'(@nonvar)

'$lgt_valid_object_property'(built_in).					% built-in object
'$lgt_valid_object_property'((dynamic)).				% dynamic object (can be abolished at runtime)
'$lgt_valid_object_property'(static).					% static object
'$lgt_valid_object_property'(synchronized).				% all object predicates are synchronized (using the same mutex)
'$lgt_valid_object_property'(threaded).					% object contains calls to the built-in multi-threading predicates
'$lgt_valid_object_property'(file(_, _)).				% source file name plus file directory 
'$lgt_valid_object_property'(lines(_, _)).				% start and end lines in a source file
'$lgt_valid_object_property'(context_switching_calls).	% object allows the use of the <</2 control construct
'$lgt_valid_object_property'(dynamic_declarations).		% object supports dynamic declaration of new predicates
'$lgt_valid_object_property'(events).					% messages sent from the object using the ::/2 control construct generate events
'$lgt_valid_object_property'(complements).				% object can be complemented by categories
'$lgt_valid_object_property'(public(_)).				% list of predicate indicators
'$lgt_valid_object_property'(parameter_names(_)).		% list of atoms



% '$lgt_valid_protocol_property'(@nonvar)

'$lgt_valid_protocol_property'(built_in).				% built-in protocol
'$lgt_valid_protocol_property'((dynamic)).				% dynamic protocol (can be abolished at runtime)
'$lgt_valid_protocol_property'(static).					% static protocol
'$lgt_valid_protocol_property'(file(_, _)).				% source file name plus file directory
'$lgt_valid_protocol_property'(lines(_, _)).			% start and end lines in a source file
'$lgt_valid_protocol_property'(public(_)).				% list of predicate indicators
'$lgt_valid_protocol_property'(parameter_names(_)).		% list of atoms



% '$lgt_valid_category_property'(@nonvar)

'$lgt_valid_category_property'(built_in).				% built-in category
'$lgt_valid_category_property'((dynamic)).				% dynamic category (can be abolished at runtime)
'$lgt_valid_category_property'(static).					% static category
'$lgt_valid_category_property'(synchronized).			% all category predicates are synchronized (using the same mutex)
'$lgt_valid_category_property'(file(_, _)).				% source file name plus file directory
'$lgt_valid_category_property'(lines(_, _)).			% start and end lines in a source file
'$lgt_valid_category_property'(events).					% messages sent from the category using the ::/2 control construct generate events
'$lgt_valid_category_property'(public(_)).				% list of predicate indicators
'$lgt_valid_category_property'(parameter_names(_)).		% list of atoms



% '$lgt_valid_flag'(@nonvar)
%
% true if the argument is a valid Logtalk flag name

% documenting compilation flags:
'$lgt_valid_flag'(xmldocs).
'$lgt_valid_flag'(xslfile).
'$lgt_valid_flag'(xmlspec).
'$lgt_valid_flag'(xmlsref).
% lint compilation flags:
'$lgt_valid_flag'(unknown).
'$lgt_valid_flag'(singletons).
'$lgt_valid_flag'(misspelt).
'$lgt_valid_flag'(lgtredef).
'$lgt_valid_flag'(plredef).
'$lgt_valid_flag'(underscore_variables).
'$lgt_valid_flag'(portability).
'$lgt_valid_flag'(missing_directives).
% directories compilation flags:
'$lgt_valid_flag'(altdirs).
'$lgt_valid_flag'(tmpdir).
'$lgt_valid_flag'(xmldir).
% optional features compilation flags:
'$lgt_valid_flag'(complements).
'$lgt_valid_flag'(dynamic_declarations).
'$lgt_valid_flag'(events).
'$lgt_valid_flag'(context_switching_calls).
% other compilation flags:
'$lgt_valid_flag'(report).
'$lgt_valid_flag'(smart_compilation).
'$lgt_valid_flag'(reload).
'$lgt_valid_flag'(hook).
'$lgt_valid_flag'(code_prefix).
'$lgt_valid_flag'(optimize).
'$lgt_valid_flag'(debug).
'$lgt_valid_flag'(startup_message).
'$lgt_valid_flag'(clean).
% read-only compilation flags:
'$lgt_valid_flag'(version).
% back-end Prolog features:
'$lgt_valid_flag'(prolog_dialect).
'$lgt_valid_flag'(prolog_version).
'$lgt_valid_flag'(prolog_compatible_version).
'$lgt_valid_flag'(break_predicate).
'$lgt_valid_flag'(encoding_directive).
'$lgt_valid_flag'(threads).
'$lgt_valid_flag'(modules).
'$lgt_valid_flag'(tabling).
'$lgt_valid_flag'(coinduction).



% '$lgt_read_only_flag'(@nonvar)
%
% true if the argument is a read only Logtalk flag name

% Logtalk version flag:
'$lgt_read_only_flag'(version).
% back-end Prolog features:
'$lgt_read_only_flag'(prolog_dialect).
'$lgt_read_only_flag'(prolog_version).
'$lgt_read_only_flag'(prolog_compatible_version).
'$lgt_read_only_flag'(break_predicate).
'$lgt_read_only_flag'(encoding_directive).
'$lgt_read_only_flag'(threads).
'$lgt_read_only_flag'(modules).
'$lgt_read_only_flag'(tabling).
'$lgt_read_only_flag'(coinduction).



% '$lgt_valid_flag_value'(@atom, @nonvar)

'$lgt_valid_flag_value'(xmldocs, on) :- !.
'$lgt_valid_flag_value'(xmldocs, off) :- !.

'$lgt_valid_flag_value'(xslfile, File) :-
	atom(File).

'$lgt_valid_flag_value'(xmlsref, standalone) :- !.
'$lgt_valid_flag_value'(xmlsref, (local)) :- !.
'$lgt_valid_flag_value'(xmlsref, web) :- !.

'$lgt_valid_flag_value'(xmlspec, dtd) :- !.
'$lgt_valid_flag_value'(xmlspec, xsd) :- !.

'$lgt_valid_flag_value'(unknown, silent) :- !.
'$lgt_valid_flag_value'(unknown, warning) :- !.

'$lgt_valid_flag_value'(singletons, silent) :- !.
'$lgt_valid_flag_value'(singletons, warning) :- !.

'$lgt_valid_flag_value'(misspelt, silent) :- !.
'$lgt_valid_flag_value'(misspelt, warning) :- !.
'$lgt_valid_flag_value'(misspelt, error) :- !.

'$lgt_valid_flag_value'(lgtredef, silent) :- !.
'$lgt_valid_flag_value'(lgtredef, warning) :- !.

'$lgt_valid_flag_value'(plredef, silent) :- !.
'$lgt_valid_flag_value'(plredef, warning) :- !.

'$lgt_valid_flag_value'(missing_directives, silent) :- !.
'$lgt_valid_flag_value'(missing_directives, warning) :- !.

'$lgt_valid_flag_value'(portability, silent) :- !.
'$lgt_valid_flag_value'(portability, warning) :- !.

'$lgt_valid_flag_value'(report, on) :- !.
'$lgt_valid_flag_value'(report, warnings) :- !.
'$lgt_valid_flag_value'(report, off) :- !.

'$lgt_valid_flag_value'(smart_compilation, on) :- !.
'$lgt_valid_flag_value'(smart_compilation, off) :- !.

'$lgt_valid_flag_value'(clean, on) :- !.
'$lgt_valid_flag_value'(clean, off) :- !.

'$lgt_valid_flag_value'(reload, always) :- !.
'$lgt_valid_flag_value'(reload, skip) :- !.

'$lgt_valid_flag_value'(underscore_variables, dont_care) :- !.
'$lgt_valid_flag_value'(underscore_variables, singletons) :- !.

'$lgt_valid_flag_value'(code_prefix, Prefix) :-
	atom(Prefix).

'$lgt_valid_flag_value'(optimize, on) :- !.
'$lgt_valid_flag_value'(optimize, off) :- !.

'$lgt_valid_flag_value'(debug, on) :- !.
'$lgt_valid_flag_value'(debug, off) :- !.

'$lgt_valid_flag_value'(complements, allow) :- !.
'$lgt_valid_flag_value'(complements, deny) :- !.

'$lgt_valid_flag_value'(dynamic_declarations, allow) :- !.
'$lgt_valid_flag_value'(dynamic_declarations, deny) :- !.

'$lgt_valid_flag_value'(context_switching_calls, allow) :- !.
'$lgt_valid_flag_value'(context_switching_calls, deny) :- !.

'$lgt_valid_flag_value'(events, allow) :- !.
'$lgt_valid_flag_value'(events, deny) :- !.

'$lgt_valid_flag_value'(startup_message, flags(compact)) :- !.
'$lgt_valid_flag_value'(startup_message, flags(verbose)) :- !.
'$lgt_valid_flag_value'(startup_message, banner) :- !.
'$lgt_valid_flag_value'(startup_message, none) :- !.

'$lgt_valid_flag_value'(hook, Obj) :-
	callable(Obj).

'$lgt_valid_flag_value'(altdirs, on) :- !.
'$lgt_valid_flag_value'(altdirs, off) :- !.

'$lgt_valid_flag_value'(xmldir, Directory) :-
	atom(Directory).
'$lgt_valid_flag_value'(tmpdir, Directory) :-
	atom(Directory).



% '$lgt_valid_entity_parameter'(@term)
%
% valid predicate argument documentation on info/2 directive

'$lgt_valid_entity_parameter'(Name - Description) :-
	atom(Name),
	atom(Description).



% '$lgt_valid_predicate_argument'(@term)
%
% valid predicate argument documentation on info/2 directive

'$lgt_valid_predicate_argument'(Name - Description) :-
	atom(Name),
	atom(Description).


% '$lgt_valid_predicate_allocation'(@nonvar)
%
% valid predicate allocation on info/2 directive

'$lgt_valid_predicate_allocation'(container).			% predicate defined in the object containing its scope directive 
'$lgt_valid_predicate_allocation'(descendants).			% predicate should be defined in the descendant objects 
'$lgt_valid_predicate_allocation'(instances).			% predicate should be defined in the class instances
'$lgt_valid_predicate_allocation'(classes).				% predicate should be defined in the class and its subclasses
'$lgt_valid_predicate_allocation'(subclasses).			% predicate should be defined in the class subclasses
'$lgt_valid_predicate_allocation'(any).					% no restrictions on where the predicate should be defined



% '$lgt_valid_predicate_redefinition'(@nonvar)
%
% valid predicate redefinition on info/2 directive

'$lgt_valid_predicate_redefinition'(never).				% predicate should not be redefined
'$lgt_valid_predicate_redefinition'(free).				% predicate can be freely redefined
'$lgt_valid_predicate_redefinition'(specialize).		% predicate redefinition must call the inherited definition
'$lgt_valid_predicate_redefinition'(call_super_first).	% predicate redefinition must call the inherited definition as the first body goal
'$lgt_valid_predicate_redefinition'(call_super_last).	% predicate redefinition must call the inherited definition as the last body goal



% '$lgt_valid_predicate_exception'(@term)
%
% valid predicate exception documentation on info/2 directive

'$lgt_valid_predicate_exception'(Description - Term) :-
	atom(Description),
	nonvar(Term).



% '$lgt_valid_predicate_call_example'(@term)
%
% valid predicate call example documentation on info/1 directive

'$lgt_valid_predicate_call_example'(Description - Call - {Bindings}) :-
	atom(Description),
	callable(Call),
	nonvar(Bindings),
	(	Bindings == no -> true
	;	Bindings == yes -> true
	;	'$lgt_valid_example_var_bindings'(Bindings)
	).



% '$lgt_valid_predicate_call_example'(@term, +atom, +integer)
%
% valid predicate call example documentation on info/2 directive

'$lgt_valid_predicate_call_example'((Description - Call - {Bindings}), Functor, Arity) :-
	atom(Description),
	nonvar(Call),
	functor(Pred, Functor, Arity),
	Call = Pred,
	nonvar(Bindings),
	(	Bindings == no -> true
	;	Bindings == yes -> true
	;	'$lgt_valid_example_var_bindings'(Bindings)
	).



'$lgt_valid_example_var_bindings'((Binding, Bindings)) :-
	!,
	'$lgt_valid_example_var_binding'(Binding),
	'$lgt_valid_example_var_bindings'(Bindings).

'$lgt_valid_example_var_bindings'(Binding) :-
	'$lgt_valid_example_var_binding'(Binding).


'$lgt_valid_example_var_binding'(Binding) :-
	nonvar(Binding),
	Binding = (Var = _),
	var(Var).



% '$lgt_xml_encoding'(-atom)
%
% returns the text encoding that should be used on the XML documenting file;
% default encoding is UTF-8

'$lgt_xml_encoding'(Encoding) :-
	(	'$lgt_pp_file_encoding_'(Encoding, _) ->
		true
	;	Encoding = 'UTF-8'
	).



% Logtalk built-in predicates
%
% '$lgt_lgt_built_in'(?callable)

'$lgt_lgt_built_in'(_ :: _).

'$lgt_lgt_built_in'(forall(_, _)).
'$lgt_lgt_built_in'(retractall(_)).

'$lgt_lgt_built_in'(logtalk_compile(_)).
'$lgt_lgt_built_in'(logtalk_compile(_, _)).
'$lgt_lgt_built_in'(logtalk_load(_)).
'$lgt_lgt_built_in'(logtalk_load(_, _)).
'$lgt_lgt_built_in'(logtalk_load_context(_, _)).
'$lgt_lgt_built_in'(logtalk_library_path(_, _)).

'$lgt_lgt_built_in'(protocol_property(_, _)).
'$lgt_lgt_built_in'(category_property(_, _)).
'$lgt_lgt_built_in'(object_property(_, _)).

'$lgt_lgt_built_in'(current_protocol(_)).
'$lgt_lgt_built_in'(current_category(_)).
'$lgt_lgt_built_in'(current_object(_)).

'$lgt_lgt_built_in'(create_object(_, _, _, _)).
'$lgt_lgt_built_in'(create_category(_, _, _, _)).
'$lgt_lgt_built_in'(create_protocol(_, _, _)).

'$lgt_lgt_built_in'(abolish_object(_)).
'$lgt_lgt_built_in'(abolish_category(_)).
'$lgt_lgt_built_in'(abolish_protocol(_)).

'$lgt_lgt_built_in'(implements_protocol(_, _)).
'$lgt_lgt_built_in'(implements_protocol(_, _, _)).
'$lgt_lgt_built_in'(imports_category(_, _)).
'$lgt_lgt_built_in'(imports_category(_, _, _)).
'$lgt_lgt_built_in'(instantiates_class(_, _)).
'$lgt_lgt_built_in'(instantiates_class(_, _, _)).
'$lgt_lgt_built_in'(specializes_class(_, _)).
'$lgt_lgt_built_in'(specializes_class(_, _, _)).
'$lgt_lgt_built_in'(extends_protocol(_, _)).
'$lgt_lgt_built_in'(extends_protocol(_, _, _)).
'$lgt_lgt_built_in'(extends_object(_, _)).
'$lgt_lgt_built_in'(extends_object(_, _, _)).
'$lgt_lgt_built_in'(extends_category(_, _)).
'$lgt_lgt_built_in'(extends_category(_, _, _)).
'$lgt_lgt_built_in'(complements_object(_, _)).

'$lgt_lgt_built_in'(conforms_to_protocol(_, _)).
'$lgt_lgt_built_in'(conforms_to_protocol(_, _, _)).

'$lgt_lgt_built_in'(abolish_events(_, _, _, _, _)).
'$lgt_lgt_built_in'(define_events(_, _, _, _, _)).
'$lgt_lgt_built_in'(current_event(_, _, _, _, _)).

'$lgt_lgt_built_in'(current_logtalk_flag(_, _)).
'$lgt_lgt_built_in'(set_logtalk_flag(_, _)).

'$lgt_lgt_built_in'(threaded(_)).
'$lgt_lgt_built_in'(threaded_call(_, _)).
'$lgt_lgt_built_in'(threaded_call(_)).
'$lgt_lgt_built_in'(threaded_once(_, _)).
'$lgt_lgt_built_in'(threaded_once(_)).
'$lgt_lgt_built_in'(threaded_ignore(_)).
'$lgt_lgt_built_in'(threaded_exit(_, _)).
'$lgt_lgt_built_in'(threaded_exit(_)).
'$lgt_lgt_built_in'(threaded_peek(_, _)).
'$lgt_lgt_built_in'(threaded_peek(_)).
'$lgt_lgt_built_in'(threaded_wait(_)).
'$lgt_lgt_built_in'(threaded_notify(_)).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  DCG rule conversion
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% '$lgt_dcg_rule_to_clause'(@dcgrule, -clause)
%
% converts a grammar rule into a normal clause

'$lgt_dcg_rule_to_clause'(Rule, Clause) :-
	catch(
		'$lgt_dcg_rule'(Rule, Clause),
		Error,
		throw(error(Error, grammar_rule(Rule)))).



% '$lgt_dcg_rule'(@dcgrule, -clause)
%
% converts a grammar rule into a normal clause:

'$lgt_dcg_rule'(Rule, Clause) :-
	'$lgt_dcg_rule'(Rule, S0, S, Expansion),
	(	'$lgt_compiler_flag'(optimize, on) ->
		'$lgt_dcg_simplify'(Expansion, S0, S, Clause)
	;	Clause = Expansion
	).


'$lgt_dcg_rule'((RHead --> _), _, _, _) :-
	var(RHead),
	throw(instantiation_error).

'$lgt_dcg_rule'((RHead, _ --> _), _, _, _) :-
	var(RHead),
	throw(instantiation_error).

'$lgt_dcg_rule'((_, Terminals --> _), _, _, _) :-
	var(Terminals),
	throw(instantiation_error).

'$lgt_dcg_rule'((Entity::_ --> _), _, _, _) :-
	var(Entity),
	throw(instantiation_error).

'$lgt_dcg_rule'((Entity::_, _ --> _), _, _, _) :-
	var(Entity),
	throw(instantiation_error).

'$lgt_dcg_rule'((':'(Module, _) --> _), _, _, _) :-
	var(Module),
	throw(instantiation_error).

'$lgt_dcg_rule'(((':'(Module, _), _) --> _), _, _, _) :-
	var(Module),
	throw(instantiation_error).

'$lgt_dcg_rule'((_::Pred --> _), _, _, _) :-
	var(Pred),
	throw(instantiation_error).

'$lgt_dcg_rule'((_::Pred, _ --> _), _, _, _) :-
	var(Pred),
	throw(instantiation_error).

'$lgt_dcg_rule'((':'(_, Pred) --> _), _, _, _) :-
	var(Pred),
	throw(instantiation_error).

'$lgt_dcg_rule'(((':'(_, Pred), _) --> _), _, _, _) :-
	var(Pred),
	throw(instantiation_error).

'$lgt_dcg_rule'((Entity::_ --> _), _, _, _) :-
	\+ callable(Entity),
	throw(type_error(entity_identifier, Entity)).

'$lgt_dcg_rule'((Entity::_, _ --> _), _, _, _) :-
	\+ callable(Entity),
	throw(type_error(entity_identifier, Entity)).

'$lgt_dcg_rule'((':'(Module, _) --> _), _, _, _) :-
	\+ atom(Module),
	throw(type_error(atom, Module)).

'$lgt_dcg_rule'(((':'(Module, _), _) --> _), _, _, _) :-
	\+ atom(Module),
	throw(type_error(atom, Module)).

'$lgt_dcg_rule'((Entity::NonTerminal, Terminals --> GRBody), S0, S, (Entity::Head :- Body)) :-
	!,
	'$lgt_dcg_rule'((NonTerminal, Terminals --> GRBody), S0, S, (Head :- Body)).

'$lgt_dcg_rule'((':'(Module, NonTerminal), Terminals --> GRBody), S0, S, (':'(Module, Head) :- Body)) :-
	!,
	'$lgt_dcg_rule'((NonTerminal, Terminals --> GRBody), S0, S, (Head :- Body)).

'$lgt_dcg_rule'((NonTerminal, Terminals --> GRBody), S0, S, (Head :- Body)) :-
	!,
	'$lgt_dcg_non_terminal'(NonTerminal, S0, S, Head),
	'$lgt_dcg_body'(GRBody, S0, S1, Goal1),
	'$lgt_dcg_terminals'(Terminals, S, S1, Goal2),
	Body = (Goal1, Goal2),
	functor(NonTerminal, Functor, Arity),
	(	'$lgt_pp_defines_non_terminal_'(Functor, Arity) ->
		true
	;	assertz('$lgt_pp_defines_non_terminal_'(Functor, Arity))
	).

'$lgt_dcg_rule'((Entity::NonTerminal --> GRBody), S0, S, (Entity::Head :- Body)) :-
	!,
	'$lgt_dcg_rule'((NonTerminal --> GRBody), S0, S, (Head :- Body)).

'$lgt_dcg_rule'((':'(Module, NonTerminal) --> GRBody), S0, S, (':'(Module, Head) :- Body)) :-
	!,
	'$lgt_dcg_rule'((NonTerminal --> GRBody), S0, S, (Head :- Body)).

'$lgt_dcg_rule'((call(_) --> _), _, _, _) :-
	throw(permission_error(modify, built_in_non_terminal, call//1)).

'$lgt_dcg_rule'((NonTerminal --> GRBody), S0, S, (Head :- Body)) :-
	!,
	'$lgt_dcg_non_terminal'(NonTerminal, S0, S, Head),
	'$lgt_dcg_body'(GRBody, S0, S, Body),
	functor(NonTerminal, Functor, Arity),
	(	'$lgt_pp_defines_non_terminal_'(Functor, Arity) ->
		true
	;	assertz('$lgt_pp_defines_non_terminal_'(Functor, Arity))
	).

'$lgt_dcg_rule'(Term, _, _, _) :-
	throw(type_error(grammar_rule, Term)).



% '$lgt_dcg_non_terminal'(+callable, @var, @var, -goal)
%
% translates a grammar goal non-terminal:

'$lgt_dcg_non_terminal'(NonTerminal, S0, S, Goal) :-
	'$lgt_must_be'(callable, NonTerminal),
	NonTerminal =.. NonTerminalUniv,
	'$lgt_append'(NonTerminalUniv, [S0, S], GoalUniv),
	Goal =.. GoalUniv.



% '$lgt_dcg_terminals'(+list, @var, @var, -goal)
%
% translates a list of terminals:

'$lgt_dcg_terminals'(Terminals, S0, S, S0 = List) :-
	'$lgt_must_be'(list, Terminals),
	'$lgt_append'(Terminals, S, List).



% '$lgt_dcg_msg'(@dcgbody @object_identifier, @var, @var, -body)
%
% translates a grammar rule message to an object into a predicate message:

'$lgt_dcg_msg'(Var, Obj, S0, S, phrase(Obj::Var, S0, S)) :-
	var(Var),
	!.

'$lgt_dcg_msg'((GRIf -> GRThen), Obj, S0, S, (If -> Then)) :-
	!,
	'$lgt_dcg_msg'(GRIf, Obj, S0, S1, If),
	'$lgt_dcg_msg'(GRThen, Obj, S1, S, Then).

'$lgt_dcg_msg'((GREither; GROr), Obj, S0, S, (Either; Or)) :-
	!,
	'$lgt_dcg_msg'(GREither, Obj, S0, S, Either),
	'$lgt_dcg_msg'(GROr, Obj, S0, S, Or).

'$lgt_dcg_msg'((GRFirst, GRSecond), Obj, S0, S, (First, Second)) :-
	!,
	'$lgt_dcg_msg'(GRFirst, Obj, S0, S1, First),
	'$lgt_dcg_msg'(GRSecond, Obj, S1, S, Second).

'$lgt_dcg_msg'(!, _, S0, S, (!, '$lgt_gr_pair'(S0 = S))) :-
	!.

'$lgt_dcg_msg'(NonTerminal, Obj, S0, S, Obj::Pred) :-
	'$lgt_dcg_non_terminal'(NonTerminal, S0, S, Pred).



% '$lgt_dcg_self_msg'(@dcgbody, @var, @var, -body, -body)
%
% translates a grammar rule message to an object into a predicate message:

'$lgt_dcg_self_msg'(Var, S0, S, phrase(::Var, S0, S), phrase(::Var, S0, S)) :-
	var(Var),
	!.

'$lgt_dcg_self_msg'((GRIf -> GRThen), S0, S, (If -> Then)) :-
	!,
	'$lgt_dcg_self_msg'(GRIf, S0, S1, If),
	'$lgt_dcg_self_msg'(GRThen, S1, S, Then).

'$lgt_dcg_self_msg'((GREither; GROr), S0, S, (Either; Or)) :-
	!,
	'$lgt_dcg_self_msg'(GREither, S0, S, Either),
	'$lgt_dcg_self_msg'(GROr, S0, S, Or).

'$lgt_dcg_self_msg'((GRFirst, GRSecond), S0, S, (First, Second)) :-
	!,
	'$lgt_dcg_self_msg'(GRFirst, S0, S1, First),
	'$lgt_dcg_self_msg'(GRSecond, S1, S, Second).

'$lgt_dcg_self_msg'(!, S0, S, (!, '$lgt_gr_pair'(S0 = S))) :-
	!.

'$lgt_dcg_self_msg'(NonTerminal, S0, S, ::Pred) :-
	'$lgt_dcg_non_terminal'(NonTerminal, S0, S, Pred).



% '$lgt_dcg_body'(@dcgbody, @var, @var, -body)
%
% translates a grammar rule body into a Prolog clause body:

'$lgt_dcg_body'(Var, S0, S, phrase(Var, S0, S)) :-
	var(Var),
	!.

'$lgt_dcg_body'(::RGoal, S0, S, CGoal) :-
	!,
	'$lgt_dcg_self_msg'(RGoal, S0, S, CGoal).

'$lgt_dcg_body'(Obj::RGoal, S0, S, CGoal) :-
	!,
	'$lgt_dcg_msg'(RGoal, Obj, S0, S, CGoal).

'$lgt_dcg_body'(':'(Module, RGoal), S0, S, ':'(Module, phrase(RGoal, S0, S))) :-
	!.

'$lgt_dcg_body'((GRIf -> GRThen), S0, S, (If -> Then)) :-
	!,
	'$lgt_dcg_body'(GRIf, S0, S1, If),
	'$lgt_dcg_body'(GRThen, S1, S, Then).

'$lgt_dcg_body'((GREither; GROr), S0, S, (Either; Or)) :-
	!,
	'$lgt_dcg_body'(GREither, S0, S, Either),
	'$lgt_dcg_body'(GROr, S0, S, Or).

'$lgt_dcg_body'((GRFirst, GRSecond), S0, S, (First, Second)) :-
	!,
	'$lgt_dcg_body'(GRFirst, S0, S1, First),
	'$lgt_dcg_body'(GRSecond, S1, S, Second).

'$lgt_dcg_body'(!, S0, S, (!, '$lgt_gr_pair'(S0 = S))) :-
	!.

'$lgt_dcg_body'({}, S0, S, '$lgt_gr_pair'(S0 = S)) :-
	!.

'$lgt_dcg_body'({Goal}, S0, S, (call(Goal), '$lgt_gr_pair'(S0 = S))) :-
	var(Goal),
	!.

'$lgt_dcg_body'({Goal}, _, _, _) :-
	\+ callable(Goal),
	throw(type_error(callable, Goal)).

'$lgt_dcg_body'({Goal}, S0, S, (Goal, '$lgt_gr_pair'(S0 = S))) :-
	!.

'$lgt_dcg_body'(\+ GRBody, S0, S, (\+ Goal, '$lgt_gr_pair'(S0 = S))) :-
	!,
	'$lgt_dcg_body'(GRBody, S0, _, Goal).

'$lgt_dcg_body'(call(Closure), _, _, _) :-
	nonvar(Closure),
	\+ callable(Closure),
	throw(type_error(callable, Closure)).

'$lgt_dcg_body'(call(Closure), S0, S, call(Closure, S0, S)) :-
	!.

'$lgt_dcg_body'([], S0, S, '$lgt_gr_pair'(S0 = S)) :-
	!.

'$lgt_dcg_body'([T| Ts], S0, S, Goal) :-
	!,
	'$lgt_dcg_terminals'([T| Ts], S0, S, Goal).

'$lgt_dcg_body'(NonTerminal, S0, S, Goal) :-
	'$lgt_pp_uses_non_terminal_'(Obj, Original, NonTerminal),
	!,
	'$lgt_dcg_body'(Obj::Original, S0, S, Goal).

'$lgt_dcg_body'(NonTerminal, S0, S, Goal) :-
	'$lgt_pp_use_module_non_terminal_'(Module, Original, NonTerminal),
	!,
	'$lgt_dcg_body'(':'(Module, Original), S0, S, Goal).

'$lgt_dcg_body'(NonTerminal, S0, S, Goal) :-
	'$lgt_dcg_non_terminal'(NonTerminal, S0, S, Goal),
	functor(NonTerminal, Functor, Arity),
	(	'$lgt_pp_calls_non_terminal_'(Functor, Arity) ->
		true
	;	assertz('$lgt_pp_calls_non_terminal_'(Functor, Arity))
	).



% '$lgt_dcg_simplify'(+clause, @var, @var, -clause)
%
% simplifies the clause resulting from a grammar rule translation:

'$lgt_dcg_simplify'((Head :- Body), _, _, Clause) :-
	'$lgt_dcg_flatten_conjunctions'(Body, Flatted),
	'$lgt_dcg_fold_left_unifications'(Flatted, Folded),
	'$lgt_dcg_fold_unification_pairs'(Folded, FoldedPairs),
	(	FoldedPairs == true ->
		Clause = Head
	;	Clause = (Head :- FoldedPairs)
	).



% '$lgt_dcg_flatten_conjunctions'(+goal, -goal)
%
% removes redundant calls to true/0 and flattens conjunction of goals:

'$lgt_dcg_flatten_conjunctions'(Goal, Goal) :-
	var(Goal),
	!.

'$lgt_dcg_flatten_conjunctions'((Goal1 -> Goal2), (SGoal1 -> SGoal2)) :-
	!,
	'$lgt_dcg_flatten_conjunctions'(Goal1, SGoal1),
	'$lgt_dcg_flatten_conjunctions'(Goal2, SGoal2).

'$lgt_dcg_flatten_conjunctions'((Goal1; Goal2), (SGoal1; SGoal2)) :-
	!,
	'$lgt_dcg_flatten_conjunctions'(Goal1, SGoal1),
	'$lgt_dcg_flatten_conjunctions'(Goal2, SGoal2).

'$lgt_dcg_flatten_conjunctions'((Goal1, Goal2), (Goal1, SGoal2)) :-
	var(Goal1),
	!,
	'$lgt_dcg_flatten_conjunctions'(Goal2, SGoal2).

'$lgt_dcg_flatten_conjunctions'(((Goal1, Goal2), Goal3), Body) :-
	!,
	'$lgt_dcg_flatten_conjunctions'((Goal1, (Goal2, Goal3)), Body).

'$lgt_dcg_flatten_conjunctions'((true, Goal), Body) :-
	!,
	'$lgt_dcg_flatten_conjunctions'(Goal, Body).

'$lgt_dcg_flatten_conjunctions'((Goal, true), Body) :-
	!,
	'$lgt_dcg_flatten_conjunctions'(Goal, Body).

'$lgt_dcg_flatten_conjunctions'((Goal1, Goal2), (Goal1, Goal3)) :-
	!,
	'$lgt_dcg_flatten_conjunctions'(Goal2, Goal3).

'$lgt_dcg_flatten_conjunctions'(\+ Goal, \+ SGoal) :-
	!,
	'$lgt_dcg_flatten_conjunctions'(Goal, SGoal).

'$lgt_dcg_flatten_conjunctions'(::Goal, ::SGoal) :-
	!,
	'$lgt_dcg_flatten_conjunctions'(Goal, SGoal).

'$lgt_dcg_flatten_conjunctions'(Object::Goal, Object::SGoal) :-
	!,
	'$lgt_dcg_flatten_conjunctions'(Goal, SGoal).

'$lgt_dcg_flatten_conjunctions'(Goal, Goal).



% '$lgt_dcg_fold_left_unifications'(+goal, -goal)
%
% folds left unifications; right unifications cannot
% be folded otherwise we may loose steadfastness

'$lgt_dcg_fold_left_unifications'(Goal, Goal) :-
	var(Goal),
	!.

'$lgt_dcg_fold_left_unifications'((Term1 = Term2), Folded) :-
	!,
	(	Term1 = Term2 ->
		Folded = true
	;	Folded = fail
	).

'$lgt_dcg_fold_left_unifications'((Goal1, Goal2), (Goal1, Goal2)) :-
	var(Goal1),
	!.

'$lgt_dcg_fold_left_unifications'(((Term1 = Term2), Goal), Folded) :-
	!,
	(	Term1 = Term2 ->
		'$lgt_dcg_fold_left_unifications'(Goal, Folded)
	;	Folded = fail
	).

'$lgt_dcg_fold_left_unifications'(Goal, Goal).



% '$lgt_dcg_fold_unification_pairs'(+goal, -goal)
%
% folds pairs of consecutive variable unifications (Var1 = Var2, Var2 = Var3)
% that are generated as a by-product of the compilation of grammar rules

'$lgt_dcg_fold_unification_pairs'(Goal, Goal) :-
	var(Goal),
	!.

'$lgt_dcg_fold_unification_pairs'((Goal1 -> Goal2), (SGoal1 -> SGoal2)) :-
	!,
	'$lgt_dcg_fold_unification_pairs'(Goal1, SGoal1),
	'$lgt_dcg_fold_unification_pairs'(Goal2, SGoal2).

'$lgt_dcg_fold_unification_pairs'((Goal1; Goal2), (SGoal1; SGoal2)) :-
	!,
	'$lgt_dcg_fold_unification_pairs'(Goal1, SGoal1),
	'$lgt_dcg_fold_unification_pairs'(Goal2, SGoal2).

'$lgt_dcg_fold_unification_pairs'((Goal1, Goal2), (Goal1, SGoal2)) :-
	var(Goal1),
	!,
	'$lgt_dcg_fold_unification_pairs'(Goal2, SGoal2).

'$lgt_dcg_fold_unification_pairs'(('$lgt_gr_pair'(Var1 = Var2a), '$lgt_gr_pair'(Var2b = Var3), Goal), SGoal) :-
	Var2a == Var2b,
	'$lgt_dcg_fold_unification_pairs'(('$lgt_gr_pair'(Var1 = Var3), Goal), SGoal),
	!.

'$lgt_dcg_fold_unification_pairs'(('$lgt_gr_pair'(Var1 = Var2a), '$lgt_gr_pair'(Var2b = Var3)), (Var1 = Var3)) :-
	Var2a == Var2b,
	!.

'$lgt_dcg_fold_unification_pairs'(('$lgt_gr_pair'(Var1 = Var2), Goal), (Var1 = Var2, SGoal)) :-
	!,
	'$lgt_dcg_fold_unification_pairs'(Goal, SGoal).

'$lgt_dcg_fold_unification_pairs'((Goal1, Goal2), (Goal1, SGoal2)) :-
	!,
	'$lgt_dcg_fold_unification_pairs'(Goal2, SGoal2).

'$lgt_dcg_fold_unification_pairs'(\+ Goal, \+ SGoal) :-
	!,
	'$lgt_dcg_fold_unification_pairs'(Goal, SGoal).

'$lgt_dcg_fold_unification_pairs'(::Goal, ::SGoal) :-
	!,
	'$lgt_dcg_fold_unification_pairs'(Goal, SGoal).

'$lgt_dcg_fold_unification_pairs'(Object::Goal, Object::SGoal) :-
	!,
	'$lgt_dcg_fold_unification_pairs'(Goal, SGoal).

'$lgt_dcg_fold_unification_pairs'('$lgt_gr_pair'(Var1 = Var2), (Var1 = Var2)) :-
	!.

'$lgt_dcg_fold_unification_pairs'(Goal, Goal).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  xml
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% '$lgt_write_xml_file'(@stream)
%
% writes a XML file containing the documentation of a compiled entity

'$lgt_write_xml_file'(Stream) :-
	'$lgt_write_xml_header'(Stream),
	'$lgt_write_xml_entity'(Stream),
	'$lgt_write_xml_relations'(Stream),
	'$lgt_write_xml_predicates'(Stream),
	'$lgt_write_xml_operators'(Stream),
	'$lgt_write_xml_remarks'(Stream),
	'$lgt_write_xml_footer'(Stream).



'$lgt_write_xml_header'(Stream) :-
	'$lgt_compiler_flag'(xmlspec, XMLSpec),
	'$lgt_compiler_flag'(xmlsref, XMLSRef),
	'$lgt_write_xml_header'(XMLSRef, XMLSpec, Stream).



'$lgt_write_xml_header'(local, XMLSpec, Stream) :-
	'$lgt_xml_encoding'(Encoding),
	'$lgt_xml_header_text'('1.0', Encoding, no, Text),
	'$lgt_write_xml_open_tag'(Stream, Text, []),
	(	XMLSpec == dtd ->
		write(Stream, '<!DOCTYPE logtalk SYSTEM "logtalk.dtd">'), nl(Stream)
	;	true
	),
	'$lgt_compiler_flag'(xslfile, XSL),
	write(Stream, '<?xml-stylesheet type="text/xsl" href="'),
	write(Stream, XSL),
	write(Stream, '"?>'), nl(Stream),
	(	XMLSpec == dtd ->
		'$lgt_write_xml_open_tag'(Stream, logtalk, [])
	;	'$lgt_write_xml_open_tag'(Stream, logtalk,
			['xmlns:xsi'-'http://www.w3.org/2001/XMLSchema-instance',
			 'xsi:noNamespaceSchemaLocation'-'logtalk.xsd'])
	).

'$lgt_write_xml_header'(web, XMLSpec, Stream) :-
	'$lgt_xml_encoding'(Encoding),
	'$lgt_xml_header_text'('1.0', Encoding, no, Text),
	'$lgt_write_xml_open_tag'(Stream, Text, []),
	(	XMLSpec == dtd ->
		write(Stream, '<!DOCTYPE logtalk SYSTEM "http://logtalk.org/xml/2.0/logtalk.dtd">'), nl(Stream)
	;	true
	),
	'$lgt_compiler_flag'(xslfile, XSL),
	write(Stream, '<?xml-stylesheet type="text/xsl" href="'),
	write(Stream, XSL),
	write(Stream, '"?>'), nl(Stream),
	(	XMLSpec == dtd ->
		'$lgt_write_xml_open_tag'(Stream, logtalk, [])
	;	'$lgt_write_xml_open_tag'(Stream, logtalk,
			['xmlns:xsi'-'http://www.w3.org/2001/XMLSchema-instance',
			 'xsi:noNamespaceSchemaLocation'-'http://logtalk.org/xml/2.0/logtalk.xsd'])
	).

'$lgt_write_xml_header'(standalone, _, Stream) :-
	'$lgt_xml_encoding'(Encoding),
	'$lgt_xml_header_text'('1.0', Encoding, yes, Text),
	'$lgt_write_xml_open_tag'(Stream, Text, []),
	'$lgt_compiler_flag'(xslfile, XSL),
	write(Stream, '<?xml-stylesheet type="text/xsl" href="'),
	write(Stream, XSL),
	write(Stream, '"?>'), nl(Stream),
	'$lgt_write_xml_open_tag'(Stream, logtalk, []).


'$lgt_xml_header_text'(Version, Encoding, Standalone, Text) :-
	atom_concat('?xml version="', Version, Aux1),
	atom_concat(Aux1, '" encoding="', Aux2),
	atom_concat(Aux2, Encoding, Aux3),
	atom_concat(Aux3, '" standalone="', Aux4),
	atom_concat(Aux4, Standalone, Aux5),
	atom_concat(Aux5, '"?', Text).



'$lgt_write_xml_footer'(Stream) :-
	'$lgt_write_xml_close_tag'(Stream, logtalk).



'$lgt_write_xml_entity'(Stream) :-
	'$lgt_pp_entity'(Type, Entity, _, _, _),
	'$lgt_write_xml_open_tag'(Stream, entity, []),
	'$lgt_entity_to_xml_term'(Entity),
	'$lgt_write_xml_cdata_element'(Stream, name, [], Entity),
	'$lgt_write_xml_element'(Stream, (type), [], Type),
	'$lgt_xml_entity_compilation_text'(Compilation),
	'$lgt_write_xml_element'(Stream, compilation, [], Compilation),
	(	'$lgt_pp_info_'(Info) ->
		'$lgt_write_xml_entity_info'(Stream, Info)
	;	true
	),
	'$lgt_write_xml_close_tag'(Stream, entity).



% '$lgt_xml_entity_compilation_text'(-atom)

'$lgt_xml_entity_compilation_text'(Flags) :-
	'$lgt_pp_entity'(_, _, _, _, Mode),
	(	'$lgt_compiler_flag'(context_switching_calls, allow) ->
		atom_concat(Mode, ', context_switching_calls', Flags1)
	;	Flags1 = Mode
	),
	(	'$lgt_compiler_flag'(dynamic_declarations, allow) ->
		atom_concat(Flags1, ', dynamic_declarations', Flags2)
	;	Flags2 = Flags1
	),
	(	'$lgt_compiler_flag'(complements, allow) ->
		atom_concat(Flags2, ', complements', Flags3)
	;	Flags3 = Flags2
	),
	(	'$lgt_compiler_flag'(events, allow) ->
		atom_concat(Flags3, ', events', Flags4)
	;	Flags4 = Flags3
	),
	(	'$lgt_pp_threaded_' ->
		atom_concat(Flags4, ', threaded', Flags5)
	;	Flags5 = Flags4
	),
	(	'$lgt_pp_synchronized_' ->
		atom_concat(Flags5, ', synchronized', Flags)
	;	Flags = Flags5
	).



% '$lgt_write_xml_entity_info'(+stream, +list)
%
% outputs the contents of entity info/1 directive
% in the order specified in the Logtalk DTD file

'$lgt_write_xml_entity_info'(Stream, Info) :-
	(	'$lgt_member'(comment is Comment, Info) ->
		'$lgt_write_xml_cdata_element'(Stream, comment, [], Comment)
	;	true
	), 
	(	'$lgt_member'(parameters is Parameters, Info) ->
		'$lgt_write_xml_open_tag'(Stream, parameters, []),
		forall(
			'$lgt_member'(Parname-Description, Parameters),
			('$lgt_write_xml_open_tag'(Stream, parameter, []),
			 '$lgt_write_xml_cdata_element'(Stream, name, [], Parname),
			 '$lgt_write_xml_cdata_element'(Stream, description, [], Description),
			 '$lgt_write_xml_close_tag'(Stream, parameter))),
		 	'$lgt_write_xml_close_tag'(Stream, parameters)
	;	true
	),
	(	'$lgt_member'(author is Author, Info) ->
		(	atom(Author) ->
			'$lgt_write_xml_cdata_element'(Stream, author, [], Author)
		;	'$lgt_entity_name_to_xml_entity'(Author, AuthorEntity),
			'$lgt_write_xml_element'(Stream, author, [], AuthorEntity)
		)
	;	true
	),
	(	'$lgt_member'(version is Version, Info) ->
		number_codes(Version, VersionCodes),
		atom_codes(VersionAtom, VersionCodes),
		'$lgt_write_xml_element'(Stream, version, [], VersionAtom)
	;	true
	),
	(	'$lgt_member'(date is Date, Info) ->
		'$lgt_write_xml_element'(Stream, date, [], Date)
	;	true
	),
	(	'$lgt_member'(copyright is Copyright, Info) ->
		(	atom(Copyright) ->
			'$lgt_write_xml_element'(Stream, copyright, [], Copyright)
		;	'$lgt_entity_name_to_xml_entity'(Copyright, CopyrightEntity),
			'$lgt_write_xml_element'(Stream, copyright, [], CopyrightEntity)
		)
	;	true
	),
	(	'$lgt_member'(license is License, Info) ->
		(	atom(License) ->
			'$lgt_write_xml_element'(Stream, license, [], License)
		;	'$lgt_entity_name_to_xml_entity'(License, LicenseEntity),
			'$lgt_write_xml_element'(Stream, license, [], LicenseEntity)
		)
	;	true
	),
	forall(
		('$lgt_member'(Key is Value, Info),
		 \+ '$lgt_member'(Key, [comment, author, version, date, parameters, parnames, copyright, license, remarks])),
		('$lgt_write_xml_open_tag'(Stream, info, []),
		 '$lgt_write_xml_element'(Stream, key, [], Key),
		 '$lgt_write_xml_cdata_element'(Stream, value, [], Value),
		 '$lgt_write_xml_close_tag'(Stream, info))).



% '$lgt_entity_name_to_xml_entity'(+nonvar, -atom)
%
% converts and entity name reference into an atom
% representing the corresponding XML entity

'$lgt_entity_name_to_xml_entity'({EntityName}, XMLEntity) :-
	atom_concat('&', EntityName, Aux),
	atom_concat(Aux, ';', XMLEntity).



% '$lgt_entity_to_xml_term'(+entity)
%
% instantiates the parameters in a parametric object
% to either user-defined names or to '$VAR'(N) terms

'$lgt_entity_to_xml_term'(Entity) :-
	'$lgt_pp_info_'(List),
	(	'$lgt_member'(parnames is Names, List) ->
		true
	;	'$lgt_member'(parameters is Parameters, List),
		findall(Name, '$lgt_member'(Name - _, Parameters), Names)
	),
	!,
	Entity =.. [_| Args],
	'$lgt_vars_to_atoms'(Args, Args, Names).

'$lgt_entity_to_xml_term'(Entity) :-
	numbervars(Entity, 0, _).



% '$lgt_relation_to_xml_term'(+entity, +entity)
%
% instantiates the parameters in a related entity taking
% in account the parameter sharing with the original entity

'$lgt_relation_to_xml_term'(Entity, Relation) :-
	'$lgt_entity_to_xml_term'(Entity),
	Relation =.. [_| Args],
	'$lgt_vars_to_underscore'(Args).



% '$lgt_pred_call_to_xml_term'(+atom, +integer, +nonvar, +nonvar, -nonvar, -nonvar)
%
% instantiates the arguments in a predicate call to user defined names or to the atom '_'

'$lgt_pred_call_to_xml_term'(Functor, Arity, Call, Bindings, QCall, QBindings) :-
	'$lgt_double_quote_atoms'(Call, QCall),
	'$lgt_double_quote_atoms'(Bindings, QBindings),
	'$lgt_pred_qcall_to_xml_term'(Functor, Arity, QCall, QBindings).


'$lgt_pred_qcall_to_xml_term'(Functor, Arity, Call, Bindings) :-
	(	'$lgt_pp_info_'(Functor/Arity, List) ->
		true
	;	'$lgt_pp_info_'(Functor//Arity, List)
	),
	(	'$lgt_member'(argnames is Names, List) ->
		true
	;	'$lgt_member'(arguments is Arguments, List),
		findall(Name, '$lgt_member'(Name - _, Arguments), Names)
	),
	!,
	Call =.. [Functor| Args],
	'$lgt_binding_vars'(Bindings, Vars),
	'$lgt_vars_to_atoms'(Args, Vars, Names).

'$lgt_pred_qcall_to_xml_term'(Functor, _, Call, _) :-
	Call =.. [Functor| Args],
	'$lgt_vars_to_underscore'(Args).



'$lgt_double_quote_atoms'(Var, Var) :-
	var(Var),
	!.

'$lgt_double_quote_atoms'((Call1, Call2), (QCall1, QCall2)) :-
	!,
	'$lgt_double_quote_atoms'(Call1, QCall1),
	'$lgt_double_quote_atoms'(Call2, QCall2).

'$lgt_double_quote_atoms'((Call1; Call2), (QCall1; QCall2)) :-
	!,
	'$lgt_double_quote_atoms'(Call1, QCall1),
	'$lgt_double_quote_atoms'(Call2, QCall2).

'$lgt_double_quote_atoms'((Call1 -> Call2), (QCall1 -> QCall2)) :-
	!,
	'$lgt_double_quote_atoms'(Call1, QCall1),
	'$lgt_double_quote_atoms'(Call2, QCall2).

'$lgt_double_quote_atoms'(\+ Call, \+ QCall) :-
	!,
	'$lgt_double_quote_atoms'(Call, QCall).

'$lgt_double_quote_atoms'([], []) :-
	!.

'$lgt_double_quote_atoms'([Arg| Args], [QArg| QArgs]) :-
	!,
	'$lgt_double_quote_atoms'(Arg, QArg),
	'$lgt_double_quote_atoms'(Args, QArgs).

'$lgt_double_quote_atoms'(Atom, QAtom) :-
	atom(Atom),
	!,
	(	'$lgt_atom_needs_quotes'(Atom) ->
		atom_concat('''', Atom, Aux),
		atom_concat(Aux, '''', QAtom)
	;	Atom = QAtom
	).

'$lgt_double_quote_atoms'(Number, Number) :-
	number(Number),
	!.

'$lgt_double_quote_atoms'(Term, QTerm) :-
	Term =.. [Functor| Args],
	(	'$lgt_built_in'(Term) ->
		QFunctor = Functor
	;	'$lgt_double_quote_atoms'(Functor, QFunctor)
	),
	'$lgt_double_quote_atoms'(Args, QArgs),
	QTerm =.. [QFunctor| QArgs].


'$lgt_atom_needs_quotes'(Atom) :-
	atom_chars(Atom, [First| Rest]),
	(	First @< a
	;	First @> z
	;	'$lgt_member'(Char, Rest),
		\+ '$lgt_alpha_numeric_char'(Char)
	),
	!.


'$lgt_alpha_numeric_char'('_').
'$lgt_alpha_numeric_char'(Char) :-
	Char @>= a, Char @=< z.
'$lgt_alpha_numeric_char'(Char) :-
	Char @>= 'A', Char @=< 'Z'.
'$lgt_alpha_numeric_char'(Char) :-
	Char @>= '0', Char @=< '9'.



% '$lgt_binding_vars'(@nonvar, -list)
%
% returns a list of all binding variables

'$lgt_binding_vars'(Bindings, Vars) :-
	(	atom(Bindings) ->		% no bindings, just "no", "yes", or equivalent answers
		Vars = []
	;	'$lgt_binding_vars_list'(Bindings, Vars)
	).


'$lgt_binding_vars_list'((Var = _), [Var]).
	
'$lgt_binding_vars_list'(((Var = _), Bindings), [Var| Vars]) :-
	'$lgt_binding_vars_list'(Bindings, Vars).



% '$lgt_vars_to_atoms'(+list, +list, +list)
%
% instantiates the variables in the input list to either a name or the atom '_'

'$lgt_vars_to_atoms'([], _, []).

'$lgt_vars_to_atoms'([Arg| Args], Vars, [Name| Names]) :-
	(	var(Arg) ->
		(	'$lgt_member_var'(Arg, Vars) ->
			Arg = Name
		;	Arg = '_'
		)
	;	true
	),
	'$lgt_vars_to_atoms'(Args, Vars, Names).



% '$lgt_vars_to_underscore'(+list)
%
% instantiates the variables in the input list to the atom '_'

'$lgt_vars_to_underscore'([]).

'$lgt_vars_to_underscore'([Arg| Args]) :-
	(	var(Arg) ->
		Arg = '_'
	;	true
	),
	'$lgt_vars_to_underscore'(Args).



% '$lgt_relation_to_xml_filename'(+entity, -atom)
%
% needed to build filenames in links to parametric objects

'$lgt_relation_to_xml_filename'(Relation, File) :-
	functor(Relation, Functor, Arity),
	number_codes(Arity, Codes),
	atom_codes(Atom, Codes),
	atom_concat(Functor, '_', Aux),
	atom_concat(Aux, Atom, File).



% '$lgt_write_xml_predicates'(@stream)
%
% writes the predicate documentation

'$lgt_write_xml_predicates'(Stream) :-
	'$lgt_write_xml_open_tag'(Stream, predicates, []),
	'$lgt_write_xml_public_predicates'(Stream),
	'$lgt_write_xml_protected_predicates'(Stream),
	'$lgt_write_xml_private_predicates'(Stream),
	'$lgt_write_xml_close_tag'(Stream, predicates).



% '$lgt_write_xml_public_predicates'(@stream)
%
% writes the documentation of public predicates

'$lgt_write_xml_public_predicates'(Stream) :-
	'$lgt_write_xml_open_tag'(Stream, (public), []),
	'$lgt_pp_public_'(Functor, Arity),
	(	'$lgt_pp_non_terminal_'(Functor, Args, Arity) ->
		'$lgt_write_xml_non_terminal'(Stream, Functor, Args, Arity, (public))
	;	'$lgt_write_xml_predicate'(Stream, Functor, Arity, (public))
	),
	fail.

'$lgt_write_xml_public_predicates'(Stream) :-
	'$lgt_write_xml_close_tag'(Stream, (public)).



% '$lgt_write_xml_protected_predicates'(@stream)
%
% writes the documentation protected predicates

'$lgt_write_xml_protected_predicates'(Stream) :-
	'$lgt_write_xml_open_tag'(Stream, protected, []),
	'$lgt_pp_protected_'(Functor, Arity),
	(	'$lgt_pp_non_terminal_'(Functor, Args, Arity) ->
		'$lgt_write_xml_non_terminal'(Stream, Functor, Args, Arity, protected)
	;	'$lgt_write_xml_predicate'(Stream, Functor, Arity, protected)
	),
	fail.

'$lgt_write_xml_protected_predicates'(Stream) :-
	'$lgt_write_xml_close_tag'(Stream, protected).



% '$lgt_write_xml_private_predicates'(@stream)
%
% writes the documentation of private predicates

'$lgt_write_xml_private_predicates'(Stream) :-
	'$lgt_write_xml_open_tag'(Stream, private, []),
	'$lgt_pp_private_'(Functor, Arity),
	(	'$lgt_pp_non_terminal_'(Functor, Args, Arity) ->
		'$lgt_write_xml_non_terminal'(Stream, Functor, Args, Arity, private)
	;	'$lgt_write_xml_predicate'(Stream, Functor, Arity, private)
	),
	fail.

'$lgt_write_xml_private_predicates'(Stream) :-
	'$lgt_write_xml_close_tag'(Stream, private).



% '$lgt_write_xml_predicate'(@stream, +atom, +integer, +term)
%
% writes the documentation of a predicate

'$lgt_write_xml_predicate'(Stream, Functor, Arity, Scope) :-
	'$lgt_write_xml_open_tag'(Stream, predicate, []),
	'$lgt_write_xml_predicate_data'(Stream, Functor, Arity, Functor/Arity, Scope),
	'$lgt_write_xml_predicate_meta'(Stream, Functor, Arity),
	'$lgt_write_xml_predicate_mode'(Stream, Functor, Arity),
	(	'$lgt_pp_info_'(Functor/Arity, Info) ->
		'$lgt_write_xml_predicate_info'(Stream, Functor, Arity, Info)
	;	true
	),
	'$lgt_write_xml_close_tag'(Stream, predicate).


'$lgt_write_xml_predicate_data'(Stream, Functor, Arity, Name, Scope) :-
	'$lgt_write_xml_cdata_element'(Stream, name, [], Name),
	'$lgt_write_xml_element'(Stream, scope, [], Scope),
	(	('$lgt_pp_entity'(_, _, _, _, (dynamic)); '$lgt_pp_dynamic_'(Functor, Arity)) ->
		Compilation = (dynamic)
	;	functor(Head, Functor, Arity), '$lgt_pp_synchronized_'(Head, _) ->
		Compilation = 'static, synchronized'
	;	Compilation = static
	),
	'$lgt_write_xml_element'(Stream, compilation, [], Compilation).


'$lgt_write_xml_predicate_meta'(Stream, Functor, Arity) :-
	functor(Meta, Functor, Arity),
	(	'$lgt_pp_meta_predicate_'(Meta) ->
		'$lgt_write_xml_cdata_element'(Stream, meta, [], Meta)
	;	true
	).


'$lgt_write_xml_predicate_mode'(Stream, Functor, Arity) :-
	functor(Template, Functor, Arity),
	forall(
		'$lgt_pp_mode_'(Template, Solutions),
		('$lgt_write_xml_open_tag'(Stream, (mode), []),
		 '$lgt_write_xml_cdata_element'(Stream, template, [], Template),
		 '$lgt_write_xml_element'(Stream, solutions, [], Solutions),
		 '$lgt_write_xml_close_tag'(Stream, (mode)))).


'$lgt_write_xml_predicate_info'(Stream, Functor, Arity, Info) :-
	(	'$lgt_member'(comment is Comment, Info) ->
		'$lgt_write_xml_cdata_element'(Stream, comment, [], Comment)
	;	true
	),
	(	'$lgt_member'(arguments is Arguments, Info) ->
		findall(Name, '$lgt_member'(Name - _, Arguments), Names),
		Template =.. [Functor| Names],
		'$lgt_write_xml_cdata_element'(Stream, template, [], Template),
		'$lgt_write_xml_open_tag'(Stream, arguments, []),
		forall(
			'$lgt_member'(Name-Description, Arguments),
		 	('$lgt_write_xml_open_tag'(Stream, argument, []),
	 		 '$lgt_write_xml_cdata_element'(Stream, name, [], Name),
		 	 '$lgt_write_xml_cdata_element'(Stream, description, [], Description),
		 	 '$lgt_write_xml_close_tag'(Stream, argument))),
		 '$lgt_write_xml_close_tag'(Stream, arguments)
	;	true
	),
	(	'$lgt_member'(argnames is Names, Info) ->
		Template =.. [Functor| Names],
		'$lgt_write_xml_cdata_element'(Stream, template, [], Template)
	;	true
	),
	(	'$lgt_member'(exceptions is Exceptions, Info) ->
		'$lgt_write_xml_open_tag'(Stream, exceptions, []),
		forall(
			'$lgt_member'(Cond-Term, Exceptions),
		 	('$lgt_write_xml_open_tag'(Stream, exception, []),
		 	 '$lgt_write_xml_cdata_element'(Stream, condition, [], Cond),
		 	 '$lgt_write_xml_cdata_element'(Stream, term, [], Term),
			 '$lgt_write_xml_close_tag'(Stream, exception))),
		 '$lgt_write_xml_close_tag'(Stream, exceptions)
	;	true
	),
	forall(
		('$lgt_member'(Key is Value, Info),
		 \+ '$lgt_member'(Key, [comment, arguments, argnames, exceptions, examples])),
		('$lgt_write_xml_open_tag'(Stream, info, []),
		 '$lgt_write_xml_element'(Stream, key, [], Key),
		 '$lgt_write_xml_cdata_element'(Stream, value, [], Value),
		 '$lgt_write_xml_close_tag'(Stream, info))),
	(	'$lgt_member'(examples is Examples, Info) ->
		'$lgt_write_xml_open_tag'(Stream, examples, []),
		forall(
			'$lgt_member'((Description - Call - {Bindings}), Examples),
			('$lgt_pred_call_to_xml_term'(Functor, Arity, Call, Bindings, QCall, QBindings),
			 '$lgt_write_xml_open_tag'(Stream, example, []),
			 '$lgt_write_xml_cdata_element'(Stream, description, [], Description),
			 '$lgt_write_xml_cdata_element'(Stream, call, [], QCall),
			 '$lgt_write_xml_cdata_element'(Stream, bindings, [], QBindings),
		 	 '$lgt_write_xml_close_tag'(Stream, example))),
		'$lgt_write_xml_close_tag'(Stream, examples)
	;	true
	).



% '$lgt_write_xml_non_terminal'(@stream, +atom, +atom, +integer, +term)
%
% writes the documentation of a grammar rule non-terminal

'$lgt_write_xml_non_terminal'(Stream, Functor, Args, Arity, Scope) :-
	'$lgt_write_xml_open_tag'(Stream, predicate, []),
	'$lgt_write_xml_predicate_data'(Stream, Functor, Arity, Functor//Args, Scope),
	'$lgt_write_xml_predicate_mode'(Stream, Functor, Args),
	(	'$lgt_pp_info_'(Functor//Args, Info) ->
		'$lgt_write_xml_predicate_info'(Stream, Functor, Args, Info)
	;	true
	),
	'$lgt_write_xml_close_tag'(Stream, predicate).



'$lgt_write_xml_relations'(Stream) :-
	'$lgt_write_xml_open_tag'(Stream, relations, []),
	fail.

'$lgt_write_xml_relations'(Stream) :-
	'$lgt_pp_relation_clause_'('$lgt_implements_protocol_'(Entity, Ptc, Scope)),
		'$lgt_write_xml_relation'(Stream, Entity, Ptc, implements, Scope),
	fail.

'$lgt_write_xml_relations'(Stream) :-
	'$lgt_pp_relation_clause_'('$lgt_imports_category_'(Entity, Ctg, Scope)),
		'$lgt_write_xml_relation'(Stream, Entity, Ctg, imports, Scope),
	fail.

'$lgt_write_xml_relations'(Stream) :-
	'$lgt_pp_relation_clause_'('$lgt_extends_object_'(Entity, Parent, Scope)),
		'$lgt_write_xml_relation'(Stream, Entity, Parent, extends, Scope),
	fail.

'$lgt_write_xml_relations'(Stream) :-
	'$lgt_pp_relation_clause_'('$lgt_instantiates_class_'(Entity, Class, Scope)),
		'$lgt_write_xml_relation'(Stream, Entity, Class, instantiates, Scope),
	fail.

'$lgt_write_xml_relations'(Stream) :-
	'$lgt_pp_relation_clause_'('$lgt_specializes_class_'(Entity, Superclass, Scope)),
		'$lgt_write_xml_relation'(Stream, Entity, Superclass, specializes, Scope),
	fail.

'$lgt_write_xml_relations'(Stream) :-
	'$lgt_pp_relation_clause_'('$lgt_extends_protocol_'(Entity, Ptc, Scope)),
		'$lgt_write_xml_relation'(Stream, Entity, Ptc, extends, Scope),
	fail.

'$lgt_write_xml_relations'(Stream) :-
	'$lgt_pp_relation_clause_'('$lgt_extends_category_'(Entity, Ctg, Scope)),
		'$lgt_write_xml_relation'(Stream, Entity, Ctg, extends, Scope),
	fail.

'$lgt_write_xml_relations'(Stream) :-
	'$lgt_pp_entity'(_, Entity, _, _, _),
		'$lgt_pp_uses_'(Obj),
		'$lgt_write_xml_relation'(Stream, Entity, Obj, uses),
	fail.

'$lgt_write_xml_relations'(Stream) :-
	'$lgt_pp_entity'(_, Entity, _, _, _),
		'$lgt_pp_calls_'(Ptc),
		'$lgt_write_xml_relation'(Stream, Entity, Ptc, calls),
	fail.

'$lgt_write_xml_relations'(Stream) :-
	'$lgt_pp_predicate_alias_'(Entity, Pred, Alias),
		Entity =.. [_| Args],				% take care of parametric entities
		'$lgt_vars_to_underscore'(Args),
		functor(Pred, PFunctor, PArity),
		functor(Alias, AFunctor, AArity),
		'$lgt_write_xml_open_tag'(Stream, alias, []),
		'$lgt_write_xml_cdata_element'(Stream, name, [], Entity),
		'$lgt_write_xml_cdata_element'(Stream, original, [], PFunctor/PArity),
		'$lgt_write_xml_cdata_element'(Stream, alternative, [], AFunctor/AArity),
		'$lgt_write_xml_close_tag'(Stream, alias),
	fail.

'$lgt_write_xml_relations'(Stream) :-
	'$lgt_write_xml_close_tag'(Stream, relations).



'$lgt_write_xml_relation'(Stream, Entity, Relation, Tag, Scope) :-
	'$lgt_relation_to_xml_term'(Entity, Relation),
	'$lgt_relation_to_xml_filename'(Relation, File),
	'$lgt_write_xml_open_tag'(Stream, Tag, []),
	'$lgt_write_xml_cdata_element'(Stream, name, [], Relation),
	'$lgt_write_xml_element'(Stream, scope, [], Scope),
	'$lgt_write_xml_cdata_element'(Stream, file, [], File),
	'$lgt_write_xml_close_tag'(Stream, Tag).



'$lgt_write_xml_relation'(Stream, Entity, Relation, Tag) :-
	'$lgt_relation_to_xml_term'(Entity, Relation),
	'$lgt_relation_to_xml_filename'(Relation, File),
	'$lgt_write_xml_open_tag'(Stream, Tag, []),
	'$lgt_write_xml_cdata_element'(Stream, name, [], Relation),
	'$lgt_write_xml_cdata_element'(Stream, file, [], File),
	'$lgt_write_xml_close_tag'(Stream, Tag).



'$lgt_write_xml_operators'(Stream) :-
	'$lgt_write_xml_open_tag'(Stream, operators, []),
	(	('$lgt_pp_entity_op_'(_, _, _, Scope), Scope \= (local)) ->
		forall(
			'$lgt_pp_entity_op_'(Priority, Specifier, Operator, (public)),
			('$lgt_write_xml_open_tag'(Stream, operator, []),
			 '$lgt_write_xml_cdata_element'(Stream, term, [], op(Priority, Specifier, Operator)),
			 '$lgt_write_xml_cdata_element'(Stream, scope, [], (public)),
		 	 '$lgt_write_xml_close_tag'(Stream, operator))),
		forall(
			'$lgt_pp_entity_op_'(Priority, Specifier, Operator, protected),
			('$lgt_write_xml_open_tag'(Stream, operator, []),
			 '$lgt_write_xml_cdata_element'(Stream, term, [], op(Priority, Specifier, Operator)),
			 '$lgt_write_xml_cdata_element'(Stream, scope, [], protected),
		 	 '$lgt_write_xml_close_tag'(Stream, operator))),
		forall(
			'$lgt_pp_entity_op_'(Priority, Specifier, Operator, (private)),
			('$lgt_write_xml_open_tag'(Stream, operator, []),
			 '$lgt_write_xml_cdata_element'(Stream, term, [], op(Priority, Specifier, Operator)),
			 '$lgt_write_xml_cdata_element'(Stream, scope, [], (private)),
		 	 '$lgt_write_xml_close_tag'(Stream, operator)))
	;	true
	),
	'$lgt_write_xml_close_tag'(Stream, operators).



'$lgt_write_xml_remarks'(Stream) :-
	'$lgt_write_xml_open_tag'(Stream, remarks, []),
	(	'$lgt_pp_info_'(Info), '$lgt_member'(remarks is Remarks, Info) ->
		forall(
			'$lgt_member'((Topic - Text), Remarks),
			('$lgt_write_xml_open_tag'(Stream, remark, []),
			 '$lgt_write_xml_cdata_element'(Stream, topic, [], Topic),
			 '$lgt_write_xml_cdata_element'(Stream, text, [], Text),
		 	 '$lgt_write_xml_close_tag'(Stream, remark)))
	;	true
	),
	'$lgt_write_xml_close_tag'(Stream, remarks).



% '$lgt_write_xml_open_tag'(@stream, @atom, @list)
%
% writes <Tag Att1="V1" Att2="V2" ...>

'$lgt_write_xml_open_tag'(Stream, Tag, Atts) :-
	write(Stream, '<'),
	write(Stream, Tag),
	'$lgt_write_xml_tag_attributes'(Stream, Atts),
	write(Stream, '>'), nl(Stream).



% '$lgt_write_xml_element'(@stream, @atom, @list, @term)
%
% writes <Tag Att1="V1" Att2="V2" ...>Text</Tag>

'$lgt_write_xml_element'(Stream, Tag, Atts, Text) :-
	write(Stream, '<'),
	write(Stream, Tag),
	'$lgt_write_xml_tag_attributes'(Stream, Atts),
	write(Stream, '>'),
	write(Stream, Text),
	write(Stream, '</'),
	write(Stream, Tag),
	write(Stream, '>'), nl(Stream).



% '$lgt_writeq_xml_cdata_element'(@stream, @atom, @list, @term)
%
% writes <Tag Att1="V1" Att2="V2" ...><![CDATA[Text]]></Tag> (quoted)

'$lgt_writeq_xml_cdata_element'(Stream, Tag, Atts, Text) :-
	write(Stream, '<'),
	write(Stream, Tag),
	'$lgt_write_xml_tag_attributes'(Stream, Atts),
	write(Stream, '><![CDATA['),
	'$lgt_pretty_print_vars_quoted'(Stream, Text),
	write(Stream, ']]></'),
	write(Stream, Tag),
	write(Stream, '>'), nl(Stream).



% '$lgt_write_xml_cdata_element'(@stream, @atom, @list, @term)
%
% writes <Tag Att1="V1" Att2="V2" ...><![CDATA[Text]]></Tag>

'$lgt_write_xml_cdata_element'(Stream, Tag, Atts, Text) :-
	write(Stream, '<'),
	write(Stream, Tag),
	'$lgt_write_xml_tag_attributes'(Stream, Atts),
	write(Stream, '><![CDATA['),
	'$lgt_pretty_print_vars'(Stream, Text),
	write(Stream, ']]></'),
	write(Stream, Tag),
	write(Stream, '>'), nl(Stream).



% '$lgt_write_xml_tag_attributes'(@stream, @list)

'$lgt_write_xml_tag_attributes'(_, []) :-
	!.

'$lgt_write_xml_tag_attributes'(Stream, [Attribute-Value| Rest]) :-
	write(Stream, ' '),
	write(Stream, Attribute),
	write(Stream, '="'),
	write(Stream, Value),
	write(Stream, '"'),
	'$lgt_write_xml_tag_attributes'(Stream, Rest).



% '$lgt_write_xml_close_tag'(@stream, @atom)
%
% writes </Tag>

'$lgt_write_xml_close_tag'(Stream, Tag) :-
	write(Stream, '</'),
	write(Stream, Tag),
	write(Stream, '>'),
	nl(Stream).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  table of ISO specified predicates
%
%  (used for portability checking)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% control constructs
'$lgt_iso_spec_pred'(true).
'$lgt_iso_spec_pred'(fail).
'$lgt_iso_spec_pred'(call(_)).
'$lgt_iso_spec_pred'(!).
'$lgt_iso_spec_pred'((Goal; _)) :-
	(	var(Goal) ->
		true
	;	Goal \= '*->'(_, _)
	).
'$lgt_iso_spec_pred'((_, _)).
'$lgt_iso_spec_pred'((_ -> _)).
'$lgt_iso_spec_pred'(catch(_, _, _)).
'$lgt_iso_spec_pred'(throw(_)).
% term unification
'$lgt_iso_spec_pred'((_ = _)).
'$lgt_iso_spec_pred'((_ \= _)).
'$lgt_iso_spec_pred'(unify_with_occurs_check(_, _)).
% term testing
'$lgt_iso_spec_pred'(var(_)).
'$lgt_iso_spec_pred'(nonvar(_)).
'$lgt_iso_spec_pred'(atom(_)).
'$lgt_iso_spec_pred'(atomic(_)).
'$lgt_iso_spec_pred'(number(_)).
'$lgt_iso_spec_pred'(integer(_)).
'$lgt_iso_spec_pred'(float(_)).
'$lgt_iso_spec_pred'(compound(_)).
% term comparison
'$lgt_iso_spec_pred'((_ @=< _)).
'$lgt_iso_spec_pred'((_ @< _)).
'$lgt_iso_spec_pred'((_ @>= _)).
'$lgt_iso_spec_pred'((_ @> _)).
'$lgt_iso_spec_pred'((_ == _)).
'$lgt_iso_spec_pred'((_ \== _)).
% term creation and decomposition
'$lgt_iso_spec_pred'(functor(_, _, _)).
'$lgt_iso_spec_pred'(arg(_, _, _)).
'$lgt_iso_spec_pred'(_ =.. _).
'$lgt_iso_spec_pred'(copy_term(_, _)).
% arithmetic evaluation
'$lgt_iso_spec_pred'(_ is _).
% arithmetic comparison
'$lgt_iso_spec_pred'((_ =< _)).
'$lgt_iso_spec_pred'((_ < _)).
'$lgt_iso_spec_pred'((_ >= _)).
'$lgt_iso_spec_pred'((_ > _)).
'$lgt_iso_spec_pred'((_ =:= _)).
'$lgt_iso_spec_pred'((_ =\= _)).
% database
'$lgt_iso_spec_pred'(clause(_, _)).
'$lgt_iso_spec_pred'(current_predicate(_)).
'$lgt_iso_spec_pred'(asserta(_)).
'$lgt_iso_spec_pred'(assertz(_)).
'$lgt_iso_spec_pred'(retract(_)).
'$lgt_iso_spec_pred'(abolish(_)).
% all solutions
'$lgt_iso_spec_pred'(findall(_, _, _)).
'$lgt_iso_spec_pred'(bagof(_, _, _)).
'$lgt_iso_spec_pred'(setof(_, _, _)).
% stream selection and control
'$lgt_iso_spec_pred'(current_input(_)).
'$lgt_iso_spec_pred'(current_output(_)).
'$lgt_iso_spec_pred'(set_input(_)).
'$lgt_iso_spec_pred'(set_output(_)).
'$lgt_iso_spec_pred'(open(_, _, _, _)).
'$lgt_iso_spec_pred'(open(_, _, _)).
'$lgt_iso_spec_pred'(close(_, _)).
'$lgt_iso_spec_pred'(close(_)).
'$lgt_iso_spec_pred'(flush_output(_)).
'$lgt_iso_spec_pred'(flush_output).
'$lgt_iso_spec_pred'(stream_property(_, _)).
'$lgt_iso_spec_pred'(at_end_of_stream).
'$lgt_iso_spec_pred'(at_end_of_stream(_)).
'$lgt_iso_spec_pred'(set_stream_position(_, _)).
% character and byte input/output
'$lgt_iso_spec_pred'(get_char(_, _)).
'$lgt_iso_spec_pred'(get_char(_)).
'$lgt_iso_spec_pred'(get_code(_, _)).
'$lgt_iso_spec_pred'(get_code(_)).
'$lgt_iso_spec_pred'(peek_char(_, _)).
'$lgt_iso_spec_pred'(peek_char(_)).
'$lgt_iso_spec_pred'(peek_code(_, _)).
'$lgt_iso_spec_pred'(peek_code(_)).
'$lgt_iso_spec_pred'(put_char(_, _)).
'$lgt_iso_spec_pred'(put_char(_)).
'$lgt_iso_spec_pred'(put_code(_, _)).
'$lgt_iso_spec_pred'(put_code(_)).
'$lgt_iso_spec_pred'(nl).
'$lgt_iso_spec_pred'(nl(_)).
'$lgt_iso_spec_pred'(get_byte(_, _)).
'$lgt_iso_spec_pred'(get_byte(_)).
'$lgt_iso_spec_pred'(peek_byte(_, _)).
'$lgt_iso_spec_pred'(peek_byte(_)).
'$lgt_iso_spec_pred'(put_byte(_, _)).
'$lgt_iso_spec_pred'(put_byte(_)).
% term input/output
'$lgt_iso_spec_pred'(read_term(_, _, _)).
'$lgt_iso_spec_pred'(read_term(_, _)).
'$lgt_iso_spec_pred'(read(_)).
'$lgt_iso_spec_pred'(read(_, _)).
'$lgt_iso_spec_pred'(write_term(_, _, _)).
'$lgt_iso_spec_pred'(write_term(_, _)).
'$lgt_iso_spec_pred'(write(_)).
'$lgt_iso_spec_pred'(write(_, _)).
'$lgt_iso_spec_pred'(writeq(_)).
'$lgt_iso_spec_pred'(writeq(_, _)).
'$lgt_iso_spec_pred'(write_canonical(_)).
'$lgt_iso_spec_pred'(write_canonical(_, _)).
'$lgt_iso_spec_pred'(op(_, _, _)).
'$lgt_iso_spec_pred'(current_op(_, _, _)).
'$lgt_iso_spec_pred'(char_conversion(_, _)).
'$lgt_iso_spec_pred'(current_char_conversion(_, _)).
% logic and control
'$lgt_iso_spec_pred'(\+ _).
'$lgt_iso_spec_pred'(once(_)).
'$lgt_iso_spec_pred'(repeat).
% atomic term processing
'$lgt_iso_spec_pred'(atom_length(_, _)).
'$lgt_iso_spec_pred'(atom_concat(_, _, _)).
'$lgt_iso_spec_pred'(sub_atom(_, _, _, _, _)).
'$lgt_iso_spec_pred'(atom_chars(_, _)).
'$lgt_iso_spec_pred'(atom_codes(_, _)).
'$lgt_iso_spec_pred'(char_code(_, _)).
'$lgt_iso_spec_pred'(number_chars(_, _)).
'$lgt_iso_spec_pred'(number_codes(_, _)).
% implementation defined hooks functions
'$lgt_iso_spec_pred'(set_prolog_flag(_, _)).
'$lgt_iso_spec_pred'(current_prolog_flag(_, _)).
'$lgt_iso_spec_pred'(halt).
'$lgt_iso_spec_pred'(halt(_)).

% the following predicates are not part of the ISO/IEC 13211-1 Prolog standard
% but can be found on the Core Revision standardization proposal; more important,
% these predicates are built-in predicates in most, if not all, supported Prolog
% compilers

% database
'$lgt_iso_spec_pred'(retractall(_)).
% term testing
'$lgt_iso_spec_pred'(callable(_)).
'$lgt_iso_spec_pred'(ground(_)).
% term comparison
'$lgt_iso_spec_pred'(compare(_, _, _)).
% sorting
'$lgt_iso_spec_pred'(keysort(_, _)).
'$lgt_iso_spec_pred'(sort(_, _)).
% term creation and decomposition
'$lgt_iso_spec_pred'(numbervars(_, _, _)).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  table of ISO specified arithmetic functions
%
%  (used for portability checking)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



'$lgt_iso_spec_function'('-'(_)).
'$lgt_iso_spec_function'('+'(_, _)).
'$lgt_iso_spec_function'('-'(_, _)).
'$lgt_iso_spec_function'('*'(_, _)).
'$lgt_iso_spec_function'('/'(_, _)).
'$lgt_iso_spec_function'('//'(_, _)).
'$lgt_iso_spec_function'(rem(_, _)).
'$lgt_iso_spec_function'(mod(_, _)).
'$lgt_iso_spec_function'('/\\'(_, _)).
'$lgt_iso_spec_function'('\\/'(_, _)).
'$lgt_iso_spec_function'('\\'(_)).
'$lgt_iso_spec_function'('<<'(_, _)).
'$lgt_iso_spec_function'('>>'(_, _)).
'$lgt_iso_spec_function'('**'(_, _)).

'$lgt_iso_spec_function'(abs(_)).
'$lgt_iso_spec_function'(sign(_)).
'$lgt_iso_spec_function'(sqrt(_)).
'$lgt_iso_spec_function'(atan(_)).
'$lgt_iso_spec_function'(cos(_)).
'$lgt_iso_spec_function'(sin(_)).
'$lgt_iso_spec_function'(exp(_)).
'$lgt_iso_spec_function'(log(_)).
'$lgt_iso_spec_function'(float(_)).
'$lgt_iso_spec_function'(ceiling(_)).
'$lgt_iso_spec_function'(floor(_)).
'$lgt_iso_spec_function'(round(_)).
'$lgt_iso_spec_function'(truncate(_)).
'$lgt_iso_spec_function'(float_fractional_part(_)).
'$lgt_iso_spec_function'(float_integer_part(_)).

% the following functions are not part of the ISO/IEC 13211-1 Prolog standard
% but can be found on the Core Revision standardization proposal; more important,
% these functions are built-in functions in most, if not all, supported Prolog
% compilers

'$lgt_iso_spec_function'(pi).
'$lgt_iso_spec_function'(e).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Multi-threading support
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% '$lgt_init_object_message_queue'(+atom)
%
% creates a message queue for an object given its prefix
% (assume that any exception generated is due to the fact that the message
% queue already exists, which may happen when reloading threaded objects;
% there is no standard predicate for testing message queue existence)

'$lgt_init_object_message_queue'(ObjPrefix) :-
	catch(message_queue_create(_, [alias(ObjPrefix)]), _, true).



% '$lgt_threaded_wait_synch_ctg'(+mutex_identifier, @term, @object_identifier)

'$lgt_threaded_wait_synch_ctg'(Mutex, Msg, This) :-
	'$lgt_current_object_'(This, Prefix, _, _, _, _, _, _, _, _, _),
	mutex_unlock(Mutex),
	'$lgt_threaded_wait'(Msg, Prefix),
	mutex_lock(Mutex).



% '$lgt_threaded_wait_synch'(+mutex_identifier, @term, +entity_prefix)

'$lgt_threaded_wait_synch'(Mutex, Msg, Prefix) :-
	mutex_unlock(Mutex),
	'$lgt_threaded_wait'(Msg, Prefix),
	mutex_lock(Mutex).



% '$lgt_threaded_wait_ctg'(@term, @object_identifier)

'$lgt_threaded_wait_ctg'(Msg, This) :-
	'$lgt_current_object_'(This, Prefix, _, _, _, _, _, _, _, _, _),
	'$lgt_threaded_wait'(Msg, Prefix).



% '$lgt_threaded_wait'(@term, +entity_prefix)

'$lgt_threaded_wait'(Msg, Prefix) :-
	var(Msg),
	!,
	thread_get_message(Prefix, '$lgt_notification'(Msg)).

'$lgt_threaded_wait'([], _) :-
	!.

'$lgt_threaded_wait'([Msg| Msgs], Prefix) :-
	!,
	thread_get_message(Prefix, '$lgt_notification'(Msg)),
	'$lgt_threaded_wait'(Msgs, Prefix).

'$lgt_threaded_wait'(Msg, Prefix) :-
	thread_get_message(Prefix, '$lgt_notification'(Msg)).



% '$lgt_threaded_notify'(@term, @object_identifier)

'$lgt_threaded_notify_ctg'(Msg, This) :-
	'$lgt_current_object_'(This, Prefix, _, _, _, _, _, _, _, _, _),
	'$lgt_threaded_notify'(Msg, Prefix).



% '$lgt_threaded_notify'(@term, +entity_prefix)

'$lgt_threaded_notify'(Msg, Prefix) :-
	var(Msg),
	!,
	thread_send_message(Prefix, '$lgt_notification'(Msg)).

'$lgt_threaded_notify'([], _) :-
	!.

'$lgt_threaded_notify'([Msg| Msgs], Prefix) :-
	!,
	thread_send_message(Prefix, '$lgt_notification'(Msg)),
	'$lgt_threaded_notify'(Msgs, Prefix).

'$lgt_threaded_notify'(Msg, Prefix) :-
	thread_send_message(Prefix, '$lgt_notification'(Msg)).



% '$lgt_threaded_ignore'(@callable)

'$lgt_threaded_ignore'(Goal) :-
	thread_create(catch(Goal, _, true), _, [detached(true)]).



% '$lgt_threaded_call'(@callable, +object_identifier, +object_identifier)

'$lgt_threaded_call'(Goal, This, Self) :-
	'$lgt_current_object_'(This, Queue, _, _, _, _, _, _, _, _, _),
	'$lgt_threaded_call'(Queue, Goal, This, Self).



% '$lgt_threaded_call'(+message_queue_identifier, @callable, +object_identifier, +object_identifier)

'$lgt_threaded_call'(Queue, Goal, This, Self) :-
	thread_create('$lgt_mt_non_det_goal'(Queue, Goal, This, Self, []), Id, []),
	thread_send_message(Queue, '$lgt_thread_id'(call, Goal, This, Self, [], Id)).



% '$lgt_threaded_once'(@callable, +object_identifier, +object_identifier)

'$lgt_threaded_once'(Goal, This, Self) :-
	'$lgt_current_object_'(This, Queue, _, _, _, _, _, _, _, _, _),
	'$lgt_threaded_once'(Queue, Goal, This, Self).



% '$lgt_threaded_once'(+message_queue_identifier, @callable, +object_identifier, +object_identifier)

'$lgt_threaded_once'(Queue, Goal, This, Self) :-
	thread_create('$lgt_mt_det_goal'(Queue, Goal, This, Self, []), Id, []),
	thread_send_message(Queue, '$lgt_thread_id'(once, Goal, This, Self, [], Id)).



% '$lgt_threaded_call_tagged'(@callable, +object_identifier, +object_identifier, -nonvar)

'$lgt_threaded_call_tagged'(Goal, This, Self, Tag) :-
	'$lgt_current_object_'(This, Queue, _, _, _, _, _, _, _, _, _),
	'$lgt_threaded_call_tagged'(Queue, Goal, This, Self, Tag).



% '$lgt_threaded_call_tagged'(+message_queue_identifier, @callable, +object_identifier, +object_identifier, -nonvar)

'$lgt_threaded_call_tagged'(Queue, Goal, This, Self, Tag) :-
	'$lgt_new_threaded_tag'(Tag),
	thread_create('$lgt_mt_non_det_goal'(Queue, Goal, This, Self, Tag), Id, []),
	thread_send_message(Queue, '$lgt_thread_id'(call, Goal, This, Self, Tag, Id)).



% '$lgt_threaded_once_tagged'(@callable, +object_identifier, +object_identifier, -nonvar)

'$lgt_threaded_once_tagged'(Goal, This, Self, Tag) :-
	'$lgt_current_object_'(This, Queue, _, _, _, _, _, _, _, _, _),
	'$lgt_threaded_once_tagged'(Queue, Goal, This, Self, Tag).



% '$lgt_threaded_once_tagged'(+message_queue_identifier, @callable, +object_identifier, +object_identifier, -nonvar)

'$lgt_threaded_once_tagged'(Queue, Goal, This, Self, Tag) :-
	'$lgt_new_threaded_tag'(Tag),
	thread_create('$lgt_mt_det_goal'(Queue, Goal, This, Self, Tag), Id, []),
	thread_send_message(Queue, '$lgt_thread_id'(once, Goal, This, Self, Tag, Id)).



% '$lgt_mt_det_goal'(+message_queue_identifier, +callable, +object_identifier, +object_identifier, @nonvar)
%
% processes a deterministic message received by an object's message queue

'$lgt_mt_det_goal'(Queue, Goal, This, Self, Tag) :-
	thread_self(Id),
	(	catch(Goal, Error, thread_send_message(Queue, '$lgt_reply'(Goal, This, Self, Tag, Error, Id))) ->
		(	var(Error) ->
			thread_send_message(Queue, '$lgt_reply'(Goal, This, Self, Tag, success, Id))
		;	% Goal generated an exception, which was already reported
			true
		)
	;	thread_send_message(Queue, '$lgt_reply'(Goal, This, Self, Tag, failure, Id))
	).



% '$lgt_mt_non_det_goal'(+atom, +callable, +object_identifier, +object_identifier, @nonvar)
%
% processes a non-deterministic message received by an object's message queue

'$lgt_mt_non_det_goal'(Queue, Goal, This, Self, Tag) :-
	thread_self(Id),
	(	catch(Goal, Error, thread_send_message(Queue, '$lgt_reply'(Goal, This, Self, Tag, Error, Id))),
		(   var(Error) ->
			thread_send_message(Queue, '$lgt_reply'(Goal, This, Self, Tag, success, Id)),
			thread_get_message(Message),
			(	Message == '$lgt_next' ->
				fail	% backtrack to the catch(Goal, ...) to try to find an alternative solution
			;	true	% otherwise assume Message = '$lgt_exit' and terminate thread
			)
		;	% Goal generated an exception, which was already reported
			true
		)
	;	% no (more) solutions
		thread_send_message(Queue, '$lgt_reply'(Goal, This, Self, Tag, failure, Id))
	).



% '$lgt_threaded_peek'(+callable, +object_identifier, +object_identifier, +object_identifier)

'$lgt_threaded_peek'(Goal, Sender, This, Self) :-
	'$lgt_current_object_'(This, Queue, _, _, _, _, _, _, _, _, _),
	'$lgt_threaded_peek'(Queue, Goal, Sender, This, Self, []).



% '$lgt_threaded_peek'(+atom, +callable, +object_identifier, +object_identifier, +object_identifier)

'$lgt_threaded_peek'(Queue, Goal, _, This, Self) :-
	thread_peek_message(Queue, '$lgt_reply'(Goal, This, Self, [], _, _)).



% '$lgt_threaded_peek_tagged'(+callable, +object_identifier, +object_identifier, +object_identifier, @nonvar)

'$lgt_threaded_peek_tagged'(Goal, Sender, This, Self, Tag) :-
	'$lgt_current_object_'(This, Queue, _, _, _, _, _, _, _, _, _),
	'$lgt_threaded_peek_tagged'(Queue, Goal, Sender, This, Self, Tag).



% '$lgt_threaded_peek_tagged'(+atom, +callable, +object_identifier, +object_identifier, +object_identifier, @nonvar)

'$lgt_threaded_peek_tagged'(Queue, Goal, Sender, This, Self, Tag) :-
	(	var(Tag) ->
		throw(error(instantiation_error, This::threaded_peek(Goal, Tag), Sender))	
	;	thread_peek_message(Queue, '$lgt_reply'(Goal, This, Self, Tag, _, _))
	).



% '$lgt_threaded_exit'(+callable, +object_identifier, +object_identifier, +object_identifier)

'$lgt_threaded_exit'(Goal, Sender, This, Self) :-
	'$lgt_current_object_'(This, Queue, _, _, _, _, _, _, _, _, _),
	'$lgt_threaded_exit'(Queue, Goal, Sender, This, Self).



% '$lgt_threaded_exit'(+message_queue_identifier, +callable, +object_identifier, +object_identifier, +object_identifier)

'$lgt_threaded_exit'(Queue, Goal, Sender, This, Self) :-
	(	% first check if there is a thread running for proving the goal before proceeding:
		thread_peek_message(Queue, '$lgt_thread_id'(Type, Goal, This, Self, [], Id)) ->
		% answering thread exists; go ahead and retrieve the solution(s):
		thread_get_message(Queue, '$lgt_thread_id'(Type, Goal, This, Self, [], Id)),
		(	Type == once ->
			setup_call_cleanup(
				true,
				'$lgt_mt_det_reply'(Queue, Goal, This, Self, [], Id),
				thread_join(Id, _))
		;   setup_call_cleanup(
				true,
				'$lgt_mt_non_det_reply'(Queue, Goal, This, Self, [], Id),
				((	thread_property(Id, status(running)) ->
					% thread still running, suspended waiting for a request to an alternative proof; tell it to exit
					catch(thread_send_message(Id, '$lgt_exit'), _, true)
				 ;	true
				 ),
				 thread_join(Id, _))
			)
		)
	;	% answering thread don't exist; generate an exception (failing is not an option as it could simply mean goal failure)
		throw(error(existence_error(goal_thread, Goal), This::threaded_exit(Goal), Sender))
	).



% '$lgt_threaded_exit_tagged'(+callable, +object_identifier, +object_identifier, +object_identifier, @nonvar)

'$lgt_threaded_exit_tagged'(Goal, Sender, This, Self, Tag) :-
	(	var(Tag) ->
		throw(error(instantiation_error, This::threaded_exit(Goal, Tag), Sender))	
	;	'$lgt_current_object_'(This, Queue, _, _, _, _, _, _, _, _, _),
		'$lgt_threaded_exit_tag_cheked'(Queue, Goal, Sender, This, Self, Tag)
	).



% '$lgt_threaded_exit_tagged'(+message_queue_identifier, +callable, +object_identifier, +object_identifier, +object_identifier, @nonvar)

'$lgt_threaded_exit_tagged'(Queue, Goal, Sender, This, Self, Tag) :-
	(	var(Tag) ->
		throw(error(instantiation_error, This::threaded_exit(Goal, Tag), Sender))	
	;	'$lgt_threaded_exit_tag_cheked'(Queue, Goal, Sender, This, Self, Tag)
	).



'$lgt_threaded_exit_tag_cheked'(Queue, Goal, Sender, This, Self, Tag) :-
	(	% first check if there is a thread running for proving the goal before proceeding:
		thread_peek_message(Queue, '$lgt_thread_id'(Type, Goal, This, Self, Tag, Id)) ->
		% answering thread exists; go ahead and retrieve the solution(s):
		thread_get_message(Queue, '$lgt_thread_id'(Type, Goal, This, Self, Tag, Id)),
		(	Type == once ->
			setup_call_cleanup(
				true,
				'$lgt_mt_det_reply'(Queue, Goal, This, Self, Tag, Id),
				thread_join(Id, _))
		;   setup_call_cleanup(
				true,
				'$lgt_mt_non_det_reply'(Queue, Goal, This, Self, Tag, Id),
				((	thread_property(Id, status(running)) ->
					% thread still running, suspended waiting for a request to an alternative proof; tell it to exit
					catch(thread_send_message(Id, '$lgt_exit'), _, true)
				 ;	true
				 ),
				 thread_join(Id, _))
			)
		)
	;	% answering thread don't exist; generate an exception (failing is not an option as it could simply mean goal failure)
		throw(error(existence_error(goal_thread, Goal), This::threaded_exit(Goal, Tag), Sender))
	).



% return the solution found:

'$lgt_mt_det_reply'(Queue, Goal, This, Self, Tag, Id) :-
	thread_get_message(Queue, '$lgt_reply'(Reply, This, Self, Tag, Result, Id)),
	(	Result == success ->
		Goal = Reply
	;	Result == failure ->
		fail
	;	throw(Result)
	).


% return current solution; on backtracking, ask working thread for and get from it the next solution:

'$lgt_mt_non_det_reply'(Queue, Goal, This, Self, Tag, Id) :-
	thread_get_message(Queue, '$lgt_reply'(Reply, This, Self, Tag, Result, Id)),
	(	Result == success ->
		Goal = Reply
	;	Result == failure ->
		!,
		fail
	;	throw(Result)
	).

'$lgt_mt_non_det_reply'(Queue, Goal, This, Self, Tag, Id) :-
	catch(thread_send_message(Id, '$lgt_next'), _, fail),
	'$lgt_mt_non_det_reply'(Queue, Goal, This, Self, Tag, Id).



% '$lgt_threaded_or'(-var, +callable, +list)
%
% implements the threaded/1 built-in predicate when the argument is a disjunction

'$lgt_threaded_or'(Queue, MTGoals, Results) :-
	thread_self(Queue),
	catch((MTGoals, '$lgt_mt_threaded_or_exit'(Results)), '$lgt_terminated', fail).



% '$lgt_threaded_and'(-var, +callable, +list)
%
% implements the threaded/1 built-in predicate when the argument is a conjunction

'$lgt_threaded_and'(Queue, MTGoals, Results) :-
	thread_self(Queue),
	catch((MTGoals, '$lgt_mt_threaded_and_exit'(Results)), '$lgt_terminated', fail).



% '$lgt_threaded_goal'(+callable, -list(var), +message_queue_identifier, -thread_identifier)
%
% implements the call to an individual goal in the threaded/1 built-in predicate

'$lgt_threaded_goal'(TGoal, TVars, Queue, Id) :-
	term_variables(TGoal, TVars),
	thread_create('$lgt_mt_threaded_call'(TGoal, TVars, Queue), Id, [at_exit('$lgt_mt_exit_handler'(Id, Queue))]).



% '$lgt_mt_threaded_call'(+callable, +list(var), +message_queue_identifier)
%
% proves an individual goal from a threaded/1 predicate call and
% sends the result back to the message queue associated to the call

'$lgt_mt_threaded_call'(TGoal, TVars, Queue) :-
	thread_self(Id),
	(   call(TGoal) ->
		thread_send_message(Queue, '$lgt_result'(Id, true(TVars)))
	;	thread_send_message(Queue, '$lgt_result'(Id, false))
	).



% '$lgt_mt_exit_handler'(@nonvar, +message_queue_identifier)
%
% error handler for threaded/1 individual thread calls; an error generated 
% by the thread_send_message/2 call is interpreted as meaning that the 
% master/parent thread (Queue) no longer exists leading to the detaching of
% the worker thread

'$lgt_mt_exit_handler'(Id, Queue) :-
	thread_property(Id, status(exception(Error))),
	catch(thread_send_message(Queue, '$lgt_result'(Id, exception(Error))), _, thread_detach(Id)).



% '$lgt_mt_threaded_and_exit'(+list)
%
% retrieves the result of proving a conjunction of goals using a threaded/1 predicate call
% by collecting the individual thread results posted to the master thread message queue

'$lgt_mt_threaded_and_exit'(Results) :-
	thread_get_message('$lgt_result'(Id, Result)),
	'$lgt_mt_threaded_and_exit'(Result, Id, Results).


'$lgt_mt_threaded_and_exit'(exception(Error), Id, Results) :-
	'$lgt_mt_threaded_record_result'(Results, Id, exception(Error)),
	(	Error == '$lgt_terminated' ->
		% messages can arrive out-of-order; if that's the case we need to keep looking
		% for the thread result that lead to the termination of the other threads
		'$lgt_mt_threaded_and_exit'(Results)
	;	Error == '$lgt_aborted' ->
		'$lgt_mt_threaded_call_cancel'(Results),
		throw('$lgt_terminated')
	;	'$lgt_mt_threaded_call_cancel'(Results),
		throw(Error)
	).

'$lgt_mt_threaded_and_exit'(true(TVars), Id, Results) :-
	(	'$lgt_mt_threaded_and_add_result'(Results, Id, TVars, Continue) ->
		(	Continue == false ->
			'$lgt_mt_threaded_call_join'(Results)
		;	'$lgt_mt_threaded_and_exit'(Results)
		)
	;	% adding a successful result can fail if the individual thread goals
		% are not independent (i.e. they share variables with the same or
		% partially the same role leading to unification failures)
		'$lgt_mt_threaded_and_exit'(false, Id, Results)
	).

'$lgt_mt_threaded_and_exit'(false, Id, Results) :-
	'$lgt_mt_threaded_record_result'(Results, Id, false),
	'$lgt_mt_threaded_call_cancel'(Results),
	fail.



% '$lgt_mt_threaded_and_add_result'(+list, +thread_identifier, @callable, -atom)
%
% adds the result of proving a goal and checks if all other goals have succeeded:

'$lgt_mt_threaded_and_add_result'([id(Id, TVars, true)| Results], Id, TVars, Continue) :-
	!,
	(	var(Continue) ->
		% we still don't know if there are any pending results
		'$lgt_mt_threaded_continue'(Results, Continue)
	;	true
	).

'$lgt_mt_threaded_and_add_result'([id(_, _, Done)| Results], Id, TVars, Continue) :-
	(	var(Done) ->
		% we found a thread whose result is still pending
		Continue = true
	;	% otherwise continue examining the remaining thread results
		true
	),
	'$lgt_mt_threaded_and_add_result'(Results, Id, TVars, Continue).



% '$lgt_mt_threaded_or_exit'(+message_queue_identifier, +list)
%
% retrieves the result of proving a disjunction of goals using a threaded/1 predicate
% call by collecting the individual thread results posted to the call message queue

'$lgt_mt_threaded_or_exit'(Results) :-
	thread_get_message('$lgt_result'(Id, Result)),
	'$lgt_mt_threaded_or_exit'(Result, Id, Results).


'$lgt_mt_threaded_or_exit'(exception(Error), Id, Results) :-
	'$lgt_mt_threaded_record_result'(Results, Id, exception(Error)),
	(	Error == '$lgt_terminated' ->
		% messages can arrive out-of-order; if that's the case we need to keep looking
		% for the thread result that lead to the termination of the other threads
		'$lgt_mt_threaded_or_exit'(Results)
	;	Error == '$lgt_aborted' ->
		'$lgt_mt_threaded_call_cancel'(Results),
		throw('$lgt_terminated')
	;	'$lgt_mt_threaded_call_cancel'(Results),
		throw(Error)
	).

'$lgt_mt_threaded_or_exit'(true(TVars), Id, Results) :-
	'$lgt_mt_threaded_or_exit_unify'(Results, Id, TVars),
	'$lgt_mt_threaded_call_cancel'(Results).

'$lgt_mt_threaded_or_exit'(false, Id, Results) :-
	'$lgt_mt_threaded_or_record_failure'(Results, Id, Continue),
	(	Continue == true ->
		'$lgt_mt_threaded_or_exit'(Results)
	;	% all goals failed
		'$lgt_mt_threaded_call_join'(Results),
		fail
	).



% unifies the successful thread goal result with the original call

'$lgt_mt_threaded_or_exit_unify'([id(Id, TVars, true)| _], Id, TVars) :-
	!.

'$lgt_mt_threaded_or_exit_unify'([_| Results], Id, TVars) :-
	'$lgt_mt_threaded_or_exit_unify'(Results, Id, TVars).



% '$lgt_mt_threaded_or_record_failure'(+list, +thread_identifier, -atom)
%
% records a thread goal failure and checks if all other thread goals have failed:

'$lgt_mt_threaded_or_record_failure'([id(Id, _, false)| Results], Id, Continue) :-
	!,
	(	var(Continue) ->	% we still don't know if there are any pending results
		'$lgt_mt_threaded_continue'(Results, Continue)
	;	true
	).

'$lgt_mt_threaded_or_record_failure'([id(_, _, Done)| Results], Id, Continue) :-
	(	var(Done) ->
		% we found a thread whose result is still pending
		Continue = true
	;	% otherwise continue examining the remaining thread results
		true
	),
	'$lgt_mt_threaded_or_record_failure'(Results, Id, Continue).



% '$lgt_mt_threaded_continue'(+list, -atom)
%
% checks if there are results still pending for a threaded/1 call:

'$lgt_mt_threaded_continue'([], false).

'$lgt_mt_threaded_continue'([id(_, _, Done)| Results], Continue) :-
	(	var(Done) ->
		% we found a thread whose result is still pending
		Continue = true
	;	% otherwise continue looking for a thread with a still pending result
		'$lgt_mt_threaded_continue'(Results, Continue)
	).



% '$lgt_mt_threaded_record_result'(+list, +thread_identifier, +callable)
%
% records a thread goal result:

'$lgt_mt_threaded_record_result'([id(Id, _, Result)| _], Id, Result) :-
	!.

'$lgt_mt_threaded_record_result'([_| Results], Id, Result) :-
	'$lgt_mt_threaded_record_result'(Results, Id, Result).



% '$lgt_mt_threaded_call_cancel'(+list)
%
% aborts a threaded call by aborting and joining all individual threads;
% we must use catch/3 as some threads may already be terminated

'$lgt_mt_threaded_call_cancel'(Results) :-
	'$lgt_mt_threaded_call_abort'(Results),
	'$lgt_mt_threaded_call_join'(Results).



% '$lgt_mt_threaded_call_abort'(+list)
%
% signals all individual threads to abort; we must use catch/3 as some threads may no longer exist

'$lgt_mt_threaded_call_abort'([]).

'$lgt_mt_threaded_call_abort'([id(Id, _, _)| Ids]) :-
	catch(thread_signal(Id, throw('$lgt_aborted')), _, true),
	'$lgt_mt_threaded_call_abort'(Ids).



% '$lgt_mt_threaded_call_join'(+list)
%
% joins all individual threads; we must use catch/3 as some threads may no longer exist

'$lgt_mt_threaded_call_join'([]).

'$lgt_mt_threaded_call_join'([id(Id, _, Result)| Results]) :-
	(	var(Result) ->
		% don't leak thread results as threads may reuse identifiers
		thread_get_message('$lgt_result'(Id, _))
	;	true
	),
	catch(thread_join(Id, _), _, true),
	'$lgt_mt_threaded_call_join'(Results).



% '$lgt_new_threaded_tag'(-integer)
%
% generates a new multi-threading tag; used in the built-in assynchronous
% multi-threading predicates

'$lgt_new_threaded_tag'(New) :-
	with_mutex('$lgt_threaded_tag',
		(retract('$lgt_threaded_tag_counter_'(Old)),
		 New is Old + 1,
		 asserta('$lgt_threaded_tag_counter_'(New)))).



% '$lgt_create_mutexes'(+list(mutex_identifier))
%
% creates entity mutexes (called when loading an entity); use catch/3 as
% we may be reloading an entity and the mutex may be already created

'$lgt_create_mutexes'([]).

'$lgt_create_mutexes'([Mutex| Mutexes]) :-
	catch(mutex_create(_, [alias(Mutex)]), _, true),
	'$lgt_create_mutexes'(Mutexes).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  static binding supporting predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% '$lgt_obj_static_binding_cache'(@object_identifier, @callable, @object_identifier -callable)

'$lgt_obj_static_binding_cache'(Obj, Pred, Sender, Call) :-
	(	'$lgt_obj_static_binding_cache_'(Obj, Pred, Sender, Call) ->
		true
	;	'$lgt_static_binding_entity_'(Obj),
		'$lgt_current_object_'(Obj, _, Dcl, Def, _, _, _, _, _, _, _),
		call(Dcl, Pred, p(p(p)), Meta, Flags, _, DclCtn), !,
		(	Flags /\ 2 =:= 0 ->
			% Type == static
			true
		;	% Type == (dynamic)
			Obj = DclCtn
		),
		'$lgt_entity_template'(Obj, GObj),
		'$lgt_term_template'(Pred, GPred),
		'$lgt_pred_meta_vars'(GPred, Meta, GMetaVars),
		'$lgt_exec_ctx'(GExCtx, GSender, GObj, GObj, GMetaVars, []),
		call(Def, GPred, GExCtx, GCall, DefCtn),
		!,
		% found the predicate definition; use it only if it's safe
		'$lgt_safe_static_binding_paths'(GObj, DclCtn, DefCtn),
		(	Meta == no ->
			% cache only normal predicates
			assertz('$lgt_obj_static_binding_cache_'(GObj, GPred, GSender, GCall)),
			Obj = GObj, Pred = GPred, Sender = GSender, Call = GCall
		;	% meta-predicates cannot be cached as they require translation of the meta-arguments
			Meta =.. [PredFunctor| MArgs],
			Pred =.. [PredFunctor| Args],
			% next we cannot call '$lgt_current_object_'/11 to find the Prefix as Sender may not be
			% instantiated (e.g. when meta-predicate calls are made within other meta-predicate calls)
			'$lgt_pp_entity'(_, _, Prefix, _, _),
			'$lgt_comp_ctx'(Ctx, _, Sender, Sender, Obj, Prefix, [], _, ExCtx, _, []),
			'$lgt_exec_ctx'(ExCtx, Sender, Sender, Obj, [], []),
			'$lgt_tr_static_binding_meta_args'(Args, MArgs, Ctx, TArgs, _),
			TPred =.. [PredFunctor| TArgs],
			Obj = GObj, TPred = GPred, Sender = GSender, Call = GCall
		)
	).


'$lgt_tr_static_binding_meta_args'([], [], _, [], []).

'$lgt_tr_static_binding_meta_args'([Arg| Args], [MArg| MArgs], Ctx, [TArg| TArgs], [DArg| DArgs]) :-
	'$lgt_tr_static_binding_meta_arg'(MArg, Arg, Ctx, TArg, DArg),
	'$lgt_tr_static_binding_meta_args'(Args, MArgs, Ctx, TArgs, DArgs).


'$lgt_tr_static_binding_meta_arg'(N, Arg, Ctx, {Arg}, {Arg}) :-
	% the {}/1 construct signals a pre-compiled metacall
	integer(N), N > 0,			% closure
	!,
	nonvar(Arg),				% not using the {}/1 control
	\+ functor(Arg, {}, 1),		% construct already
	'$lgt_comp_ctx_sender'(Ctx, Sender), Sender == user.

'$lgt_tr_static_binding_meta_arg'((*), Arg, _, Arg, Arg).

'$lgt_tr_static_binding_meta_arg'(0, Arg, Ctx, {FTArg}, {FDArg}) :-
	% the {}/1 construct signals a pre-compiled metacall
	'$lgt_tr_body'(Arg, TArg, DArg, Ctx),
	'$lgt_fix_predicate_calls'(TArg, FTArg, false),
	'$lgt_fix_predicate_calls'(DArg, FDArg, false).



% '$lgt_ctg_static_binding_cache'(@category_identifier, @callable, @execution_context -callable)

'$lgt_ctg_static_binding_cache'(Ctg, Pred, ExCtx, Call) :-
	(	'$lgt_ctg_static_binding_cache_'(Ctg, Pred, ExCtx, Call) ->
		true
	;	'$lgt_static_binding_entity_'(Ctg),
		'$lgt_current_category_'(Ctg, _, Dcl, Def, _, _),
		call(Dcl, Pred, _, _, Flags, DclCtn), !,
		Flags /\ 2 =:= 0,
		'$lgt_term_template'(Pred, GPred),
		call(Def, GPred, GExCtx, GCall, DefCtn), !,
		'$lgt_safe_static_binding_paths'(Ctg, DclCtn, DefCtn),
		assertz('$lgt_ctg_static_binding_cache_'(Ctg, GPred, GExCtx, GCall)),
		Pred = GPred, ExCtx = GExCtx, Call = GCall
	).



% '$lgt_safe_static_binding_paths'(@object_identifier, @object_identifier, @object_identifier)
%
% all entities in the inheritance-chain (from the object receiving the message
% to both the declaration container and the definition container) should be
% static-binding entities but this is not currently checked

'$lgt_safe_static_binding_paths'(_, _, _).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Utility error-checking predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



'$lgt_must_be'(variable, Term, Goal) :-
	(	var(Term) ->
		true
	;	throw(error(type_error(variable, Term), Goal))
	).

'$lgt_must_be'(nonvar, Term, Goal) :-
	(	nonvar(Term) ->
		true
	;	throw(error(instantiation_error, Goal))
	).

'$lgt_must_be'(atom, Term, Goal) :-
	(	var(Term) ->
		throw(error(instantiation_error, Goal))
	;	atom(Term) ->
		true
	;	throw(error(type_error(atom, Term), Goal))
	).

'$lgt_must_be'(atomic, Term, Goal) :-
	(	var(Term) ->
		throw(error(instantiation_error, Goal))
	;	atomic(Term) ->
		true
	;	throw(error(type_error(atomic, Term), Goal))
	).

'$lgt_must_be'(callable, Term, Goal) :-
	(	var(Term) ->
		throw(error(instantiation_error, Goal))
	;	callable(Term) ->
		true
	;	throw(error(type_error(callable, Term), Goal))
	).

'$lgt_must_be'(var_or_callable, Term, Goal) :-
	(	var(Term) ->
		true
	;	callable(Term) ->
		true
	;	throw(error(type_error(callable, Term), Goal))
	).

'$lgt_must_be'(list, Term, Goal) :-
	(	var(Term) ->
		throw(error(instantiation_error, Goal))
	;	'$lgt_is_proper_list'(Term) ->
		true
	;	throw(error(type_error(list, Term), Goal))
	).

'$lgt_must_be'(object_identifier, Term, Goal) :-
	(	var(Term) ->
		throw(error(instantiation_error, Goal))
	;	callable(Term) ->
		true
	;	throw(error(type_error(object_identifier, Term), Goal))
	).

'$lgt_must_be'(var_or_object_identifier, Term, Goal) :-
	(	var(Term) ->
		true
	;	callable(Term) ->
		true
	;	throw(error(type_error(object_identifier, Term), Goal))
	).

'$lgt_must_be'(protocol_identifier, Term, Goal) :-
	(	var(Term) ->
		throw(error(instantiation_error, Goal))
	;	atom(Term) ->
		true
	;	throw(error(type_error(protocol_identifier, Term), Goal))
	).

'$lgt_must_be'(var_or_protocol_identifier, Term, Goal) :-
	(	var(Term) ->
		true
	;	atom(Term) ->
		true
	;	throw(error(type_error(protocol_identifier, Term), Goal))
	).

'$lgt_must_be'(category_identifier, Term, Goal) :-
	(	var(Term) ->
		throw(error(instantiation_error, Goal))
	;	callable(Term) ->
		true
	;	throw(error(type_error(category_identifier, Term), Goal))
	).

'$lgt_must_be'(var_or_category_identifier, Term, Goal) :-
	(	var(Term) ->
		true
	;	callable(Term) ->
		true
	;	throw(error(type_error(category_identifier, Term), Goal))
	).

'$lgt_must_be'(predicate_indicator, Term, Goal) :-
	(	var(Term) ->
		throw(error(instantiation_error, Goal))
	;	Term \= _/_,
		throw(error(type_error(predicate_indicator, Term), Goal))
	;	Term = Functor/Arity,
		'$lgt_must_be'(atom, Functor),
		'$lgt_must_be'(integer, Arity),
		(	Arity < 0,
			throw(error(domain_error(not_less_than_zero, Arity), Goal))
		;	true
		)
	).

'$lgt_must_be'(scope, Term, Goal) :-
	(	var(Term) ->
		throw(error(instantiation_error, Goal))
	;	Scope \== (public),
		Scope \== protected,
		Scope \== private ->
		throw(error(type_error(scope, Scope), Goal))
	;	true
	).

'$lgt_must_be'(var_or_scope, Term, Goal) :-
	(	var(Term) ->
		true
	;	Scope \== (public),
		Scope \== protected,
		Scope \== private ->
		throw(error(type_error(scope, Scope), Goal))
	;	true
	).

'$lgt_must_be'(var_or_event, Term, Goal) :-
	(	var(Term) ->
		true
	;	Term \== before,
		Term \== after ->
		throw(error(type_error(event, Term), Goal))
	;	true
	).


'$lgt_must_be'(Type, Term) :-
	catch('$lgt_must_be'(Type, Term, _), error(Error, _), throw(Error)).



'$lgt_must_be'(Type, Term, Goal, Sender) :-
	catch('$lgt_must_be'(Type, Term, _), error(Error, _), throw(error(Error, Goal, Sender))).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk startup messages (banner and default flags)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



'$lgt_startup_message' :-
	'$lgt_compiler_flag'(startup_message, Flag),
	'$lgt_startup_message'(Flag).


'$lgt_startup_message'(flags(Option)) :-
	'$lgt_banner',
	'$lgt_default_flags'(Option).

'$lgt_startup_message'(banner) :-
	'$lgt_banner'.

'$lgt_startup_message'(none).



'$lgt_banner' :-
	current_logtalk_flag(version, version(Major, Minor, Patch)),
	nl, write('Logtalk '), write(Major), write('.'), write(Minor), write('.'), write(Patch), nl,
	write('Copyright (c) 1998-2011 Paulo Moura'), nl, nl.



'$lgt_default_flags'(compact) :-
	write('Default lint compilation flags: '), nl,
	'$lgt_compiler_flag'(unknown, Unknown), write('  unknown: '), write(Unknown),
	'$lgt_compiler_flag'(misspelt, Misspelt), write(', misspelt: '), write(Misspelt),
	'$lgt_compiler_flag'(lgtredef, Lgtredef), write(', lgtredef: '), write(Lgtredef),
	'$lgt_compiler_flag'(plredef, Plredef), write(', plredef: '), write(Plredef), nl,
	'$lgt_compiler_flag'(portability, Portability), write('  portability: '), write(Portability),
	'$lgt_compiler_flag'(missing_directives, Missing), write(', missing_directives: '), write(Missing), nl,
	'$lgt_compiler_flag'(singletons, Singletons), write('  singletons: '), write(Singletons),
	'$lgt_compiler_flag'(underscore_variables, Underscore), write(', underscore_variables: '), write(Underscore), nl,
	write('Default documenting compilation flags:'), nl,
	'$lgt_compiler_flag'(xmldocs, XMLDocs), write('  xmldocs: '), write(XMLDocs),
	'$lgt_compiler_flag'(xmlspec, XMLSpec), write(', xmlspec: '), write(XMLSpec),
	'$lgt_compiler_flag'(xmlsref, XMLSRef), write(', xmlsref: '), write(XMLSRef),
	'$lgt_compiler_flag'(xslfile, XSLFile), write(', xslfile: '), write(XSLFile), nl,
	write('Default directories compiler flags:'), nl,
	'$lgt_compiler_flag'(altdirs, Altdirs), write('  altdirs: '), write(Altdirs),
	'$lgt_compiler_flag'(tmpdir, TmpDir), write(', tmpdir: '), write(TmpDir),
	'$lgt_compiler_flag'(xmldir, XMLDir), write(', xmldir: '), write(XMLDir), nl,
	write('Default optional features compiler flags:'), nl,
	'$lgt_compiler_flag'(complements, Complements), write('  complements: '), write(Complements),
	'$lgt_compiler_flag'(dynamic_declarations, DynamicDeclarations), write(', dynamic_declarations: '), write(DynamicDeclarations), nl,
	'$lgt_compiler_flag'(context_switching_calls, ContextCalls), write('  context_switching_calls: '), write(ContextCalls),
	'$lgt_compiler_flag'(events, Events), write(', events: '), write(Events), nl,
	write('Other default compilation flags:'), nl,
	'$lgt_compiler_flag'(startup_message, Startup), write('  startup_message: '), write(Startup),
	'$lgt_compiler_flag'(report, Report), write(', report: '), write(Report), nl,
	'$lgt_compiler_flag'(code_prefix, Code), write('  code_prefix: '), writeq(Code),
	(	'$lgt_compiler_flag'(hook, Hook) -> true
	;	Hook = '(none defined)'
	),
	write(', hook: '), write(Hook), nl,
	'$lgt_compiler_flag'(optimize, Optimize), write('  optimize: '), writeq(Optimize),
	'$lgt_compiler_flag'(debug, Debug), write(', debug: '), writeq(Debug),
	'$lgt_compiler_flag'(clean, Clean), write(', clean: '), writeq(Clean),
	'$lgt_compiler_flag'(smart_compilation, Smart), write(', smart_compilation: '), write(Smart),
	'$lgt_compiler_flag'(reload, Reload), write(', reload: '), write(Reload), nl,
	write('Read-only compilation flags (back-end Prolog compiler features):'), nl,
	'$lgt_compiler_flag'(prolog_dialect, Prolog), write('  prolog_dialect: '), write(Prolog),
	'$lgt_compiler_flag'(break_predicate, Break), write(', break_predicate: '), write(Break),
	'$lgt_compiler_flag'(modules, Modules), write(', modules: '), write(Modules), nl,
	'$lgt_compiler_flag'(threads, Threads), write('  threads: '), write(Threads),
	'$lgt_compiler_flag'(encoding_directive, Encodings), write(', encoding_directive: '), write(Encodings),
	'$lgt_compiler_flag'(tabling, Tabling), write(', tabling: '), write(Tabling), nl,
	'$lgt_compiler_flag'(coinduction, Coinduction), write('  coinduction: '), write(Coinduction), nl, nl.

'$lgt_default_flags'(verbose) :-
	write('Default lint compilation flags:'), nl,
	'$lgt_compiler_flag'(unknown, Unknown),
	write('  Unknown entities (unknown):                                 '), write(Unknown), nl,
	'$lgt_compiler_flag'(misspelt, Misspelt),
	write('  Misspelt predicate calls (misspelt):                        '), write(Misspelt), nl,
	'$lgt_compiler_flag'(lgtredef, Lgtredef),
	write('  Logtalk built-in predicates redefinition (lgtredef):        '), write(Lgtredef), nl,
	'$lgt_compiler_flag'(plredef, Plredef),
	write('  Prolog built-in predicates redefinition (plredef):          '), write(Plredef), nl,
	'$lgt_compiler_flag'(missing_directives, Missing),
	write('  Missing predicate directives (missing_directives):          '), write(Missing), nl,
	'$lgt_compiler_flag'(portability, Portability),
	write('  Non portable calls (portability):                           '), write(Portability), nl,
	'$lgt_compiler_flag'(singletons, Singletons),
	write('  Singletons variables (singletons):                          '), write(Singletons), nl,
	'$lgt_compiler_flag'(underscore_variables, Underscore),
	write('  Underscore variables interpretation (underscore_variables): '), write(Underscore), nl,
	write('Default documenting compilation flags:'), nl,
	'$lgt_compiler_flag'(xmldocs, XMLDocs),
	write('  XML documenting files (xmldocs):                            '), write(XMLDocs), nl,
	'$lgt_compiler_flag'(xmlspec, XMLSpec),
	write('  XML specification file (xmlspec):                           '), write(XMLSpec), nl,
	'$lgt_compiler_flag'(xmlsref, XMLSRef),
	write('  XML specification reference (xmlsref):                      '), write(XMLSRef), nl,
	'$lgt_compiler_flag'(xslfile, XSLFile),
	write('  XSL stylesheet file (xslfile):                              '), write(XSLFile), nl,
	write('Default directories compiler flags:'), nl,
	'$lgt_compiler_flag'(altdirs, Altdirs),
	write('  Alternative compilation directories (altdirs):              '), write(Altdirs), nl,
	'$lgt_compiler_flag'(tmpdir, TmpDir),
	write('  Directory for compiler generated temporary files (tmpdir):  '), write(TmpDir), nl,
	'$lgt_compiler_flag'(xmldir, XMLDir),
	write('  XML documenting files directory (xmldir):                   '), write(XMLDir), nl,
	write('Default optional features compiler flags:'), nl,
	'$lgt_compiler_flag'(complements, Complements),
	write('  Complementing category support (complements):               '), write(Complements), nl,
	'$lgt_compiler_flag'(dynamic_declarations, DynamicDeclarations),
	write('  Dynamic declarations support (dynamic_declarations):        '), write(DynamicDeclarations), nl,
	'$lgt_compiler_flag'(events, Events),
	write('  Compile messages with event support (events):               '), write(Events), nl,
	'$lgt_compiler_flag'(context_switching_calls, ContextCalls),
	write('  Context-switching calls (context_switching_calls):          '), write(ContextCalls), nl,
	write('Other default compilation flags:'), nl,
	'$lgt_compiler_flag'(startup_message, Startup),
	write('  Startup message (startup_message):                          '), write(Startup), nl,
	'$lgt_compiler_flag'(report, Report),
	write('  Compilation report (report):                                '), write(Report), nl,
	'$lgt_compiler_flag'(code_prefix, Code),
	write('  Compiled code functors prefix (code_prefix):                '), writeq(Code), nl,
	'$lgt_compiler_flag'(optimize, Optimize),
	write('  Optimize compiled code (optimize):                          '), writeq(Optimize), nl,
	'$lgt_compiler_flag'(debug, Debug),
	write('  Compile entities in debug mode (debug):                     '), writeq(Debug), nl,
	'$lgt_compiler_flag'(reload, Reload),
	write('  Reloading of already loaded source files (reload):          '), write(Reload), nl,
	'$lgt_compiler_flag'(clean, Clean),
	write('  Cleaning of intermediate Prolog files (clean):              '), write(Clean), nl,
	'$lgt_compiler_flag'(smart_compilation, Smart),
	write('  Smart compilation (smart_compilation):                      '), write(Smart), nl,
	(	'$lgt_compiler_flag'(hook, Hook) -> true
	;	Hook = '(none defined)'
	),
	write('  Compiler hook object (hook):                                '), write(Hook), nl,
	write('Read-only compilation flags (back-end Prolog compiler features):'), nl,
	'$lgt_compiler_flag'(prolog_dialect, Prolog),
	write('  Name of the back-end Prolog compiler (prolog_dialect):      '), write(Prolog), nl,
	'$lgt_compiler_flag'(break_predicate, Break),
	write('  Support for break/0 predicate (break_predicate):            '), write(Break), nl,
	'$lgt_compiler_flag'(modules, Modules),
	write('  Support for modules (modules):                              '), write(Modules), nl,
	'$lgt_compiler_flag'(encoding_directive, Encodings),
	write('  Support for encoding/1 directive (encoding_directive):      '), write(Encodings), nl,
	'$lgt_compiler_flag'(tabling, Tabling),
	write('  Support for tabling (tabling):                              '), write(Tabling), nl,
	'$lgt_compiler_flag'(threads, Threads),
	write('  Multi-threading programming support (threads):              '), write(Threads), nl,
	'$lgt_compiler_flag'(coinduction, Coinduction),
	write('  Support for coinductive predicates (coinduction):           '), write(Coinduction), nl, nl.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk startup initialization 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% '$lgt_load_settings_file'
%
% loads any settings file defined by the user; settings files are compiled
% and loaded silently, ignoring any errors; the intermediated Prolog files
% are deleted using the clean/1 compiler flag in order to prevent problems
% when switching between back-end Prolog compilers

'$lgt_load_settings_file' :-
	'$lgt_current_directory'(Current),
	(	% first lookup for a settings file in the startup directory
		'$lgt_startup_directory'(Startup),
		'$lgt_change_directory'(Startup),
		'$lgt_file_exists'('settings.lgt') ->
		catch((
			logtalk_load(settings, [altdirs(off), report(off), smart_compilation(off), clean(on)]),
			assertz('$lgt_settings_file_loaded_'(Startup))),
			_,
			true
		)
	;	% if not found, lookup for a settings file in the Logtalk user folder
		'$lgt_user_directory'(User),			
		'$lgt_change_directory'(User),
		'$lgt_file_exists'('settings.lgt') ->
		catch((
			logtalk_load(settings, [altdirs(off), report(off), smart_compilation(off), clean(on)]),
			assertz('$lgt_settings_file_loaded_'(User))),
			_,
			true
		)
	;	true
	),
	'$lgt_change_directory'(Current).



% '$lgt_report_settings_file'
%
% loads any settings file defined by the user;
% settings files are compiled and loaded silently, ignoring any errors

'$lgt_report_settings_file' :-
	(	'$lgt_compiler_flag'(report, off) ->
		true
	;	\+ '$lgt_compiler_flag'(startup_message, flags(_)) ->
		true
	;	'$lgt_settings_file_loaded_'(Path) ->
		write('Loaded settings file found on directory '), write(Path), write('.'), nl, nl
	;	write('No settings file found or unable to load settings files due to limitations'), nl,
		write('of the back-end Prolog compiler.'), nl, nl
	).



% '$lgt_assert_default_hooks'
%
% asserts the compiler hook goal specified on the config file

'$lgt_assert_default_hooks' :-
	(	'$lgt_compiler_flag'(hook, Hook) ->
		'$lgt_compile_hooks'(Hook)
	;	true
	).



% '$lgt_start_runtime_threading'
%
% creates the message queue for the pseudo-object "user" and initializes the asynchronous 
% threaded calls tag counter support for compilers supporting multi-threading programming

'$lgt_start_runtime_threading' :-
	(	'$lgt_compiler_flag'(threads, supported),
		'$lgt_current_object_'(user, Prefix, _, _, _, _, _, _, _, _, _) ->
		'$lgt_init_object_message_queue'(Prefix),
		mutex_create(_, [alias('$lgt_threaded_tag')]),
		(	current_prolog_flag(bounded, true) ->		% initialization of threaded call tags;
			current_prolog_flag(min_integer, Min),		% curently we use integers, which impose
			assertz('$lgt_threaded_tag_counter_'(Min))	% a limitation on the maximum number of tags
		;	assertz('$lgt_threaded_tag_counter_'(0))	% on Prolog compilers with bounded integers
		)
	;	true
	).



% '$lgt_check_prolog_version'
%
% checks for a compatible back-end Prolog compiler version

'$lgt_check_prolog_version' :-
	(	'$lgt_compiler_flag'(report,  off) ->
		true
	;	'$lgt_compiler_flag'(prolog_version, Current),
		'$lgt_compiler_flag'(prolog_compatible_version, Check),
		Check =.. [Operator, Compatible] ->
		(	call(Operator, Current, Compatible) ->
			true
		;	write('%         WARNING!  Possibly incompatible Prolog version detected!'), nl,
			write('%                   Running Prolog version: '), write(Current), nl,
			write('%                   Advised Prolog version: '), write(Compatible), nl
		)
	;	true
	).



% Logtalk runtime initialization goal

:- initialization((
	'$lgt_load_settings_file',
	'$lgt_startup_message',
	'$lgt_assert_default_hooks',
	'$lgt_start_runtime_threading',
	'$lgt_debugger.reset_invocation_number',
	'$lgt_report_settings_file',
	'$lgt_check_prolog_version'
)).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  end!
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
