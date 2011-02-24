
:- object(profiler,
	implements(profilerp)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2011/02/24,
		comment is 'Simple wrapper for the SICStus Prolog profiler.']).

	load(File) :-
		current_prolog_flag(compiling, Current),
		set_prolog_flag(compiling, profiledcode),
		logtalk_load(File),
		set_prolog_flag(compiling, Current).

	load(File, Options) :-
		current_prolog_flag(compiling, Current),
		set_prolog_flag(compiling, profiledcode),
		logtalk_load(File, Options),
		set_prolog_flag(compiling, Current).

	:- meta_predicate(profile(0)).
	profile(Goal) :-
		call(Goal).

	data :-
		data(_:_, CallsData, ChoicePointsData, InstructionsData),
		(	setof(
				Calls-[Predicate,ChoicePoints,Instructions],
				(member(Predicate-Calls, CallsData), Calls > 0,
			 	member(Predicate-ChoicePoints, ChoicePointsData),
			 	member(Predicate-Instructions, InstructionsData)),
				Data) ->
			write_profile_data(Data)
		;	write_profile_data([])
		).

	data(Spec, CallsData, ChoicePointsData, InstructionsData) :-
		{profile_data(Spec, calls, predicate, CallsData),
		 profile_data(Spec, choice_points, predicate, ChoicePointsData),
		 profile_data(Spec, instructions, predicate, InstructionsData)}.


	write_profile_data(Data) :-
		format("~*c~n",[82,0'-]),
		format('~w ~40+~t~w~14+~t~w~14+~t~w~14+~n', ['Predicate', 'Calls', 'Choice-points', 'Instructions']),
		format("~*c~n",[82,0'-]),
		(	Data == [] ->
			format('~w~n', ['(no profiling data available)'])
		;	write_profile_data_rows(Data)
		),
		format("~*c~n",[82,0'-]).

	write_profile_data_rows([]).
	write_profile_data_rows([Calls-[Predicate,ChoicePoints,Instructions]| Rest]) :-
		predicate_label(Predicate, Label),
		format('~w ~40+~t~d~14+~t~d~14+~t~d~14+~n', [Label, Calls, ChoicePoints, Instructions]),
		write_profile_data_rows(Rest).

	predicate_label(TFunctor/TArity, Label) :-
		predicate_label(user:TFunctor/TArity, Label).
	predicate_label(Module:TFunctor/TArity, Label) :-
		(	Module == user,
			logtalk::decompile_predicate_indicators(TFunctor/TArity, Entity, Functor/Arity) ->
			(	atom(Entity) ->
				atomic_list_concat([Entity, '::', Functor, '/', Arity], Label)
			;	functor(Entity, EntityFunctor, EntityArity),
				atomic_list_concat([EntityFunctor, '/', EntityArity, '::', Functor, '/', Arity], Label)
			)
		;	Label = Module:TFunctor/TArity
		).

	data(Entity) :-
		nonvar(Entity),
		data(user:_, CallsData, ChoicePointsData, InstructionsData),
		(	setof(
				Calls-[Functor/Arity,ChoicePoints,Instructions],
				Type^TFunctor^TArity^(member((user:TFunctor/TArity)-Calls, CallsData), Calls > 0,
				 logtalk::decompile_predicate_indicators(TFunctor/TArity, Entity, Type, Functor/Arity),
				 member((user:TFunctor/TArity)-ChoicePoints, ChoicePointsData),
				 member((user:TFunctor/TArity)-Instructions, InstructionsData)),
				Data) ->
			write_entity_profile_data(Entity, Data)
		;	write_entity_profile_data(Entity, [])
		).

	write_entity_profile_data(Entity, Data) :-
		format("~*c~n",[82,0'-]),
		(	atom(Entity) ->
			EntityLabel = Entity
		;	functor(Entity, EntityFunctor, EntityArity),
			atomic_list_concat([EntityFunctor, '/', EntityArity], EntityLabel)
		),
		format("~w~n",[EntityLabel]),
		format('~w ~40+~t~w~14+~t~w~14+~t~w~14+~n', ['Predicate', 'Calls', 'Choice-points', 'Instructions']),
		format("~*c~n",[82,0'-]),
		(	Data == [] ->
			format('~w~n', ['(no profiling data available for this entity)'])
		;	write_entity_profile_data_rows(Data)
		),
		format("~*c~n",[82,0'-]).

	write_entity_profile_data_rows([]).
	write_entity_profile_data_rows([Calls-[Functor/Arity,ChoicePoints,Instructions]| Rest]) :-
		atomic_list_concat([Functor, '/', Arity], Label),
		format('~w ~40+~t~d~14+~t~d~14+~t~d~14+~n', [Label, Calls, ChoicePoints, Instructions]),
		write_entity_profile_data_rows(Rest).

	reset :-
		{profile_reset(_:_)}.

	atomic_list_concat(List, Atom) :-
		atomic_list_concat(List, '', Atom).

	atomic_list_concat([], Atom, Atom).
	atomic_list_concat([Atomic| Atomics], Acc, Atom) :-
		(	atom(Atomic) ->
			atom_concat(Acc, Atomic, Acc2)
		;	number_codes(Atomic, Codes),
			atom_codes(Converted, Codes),
			atom_concat(Acc, Converted, Acc2)
		),
		atomic_list_concat(Atomics, Acc2, Atom).

:- end_object.
