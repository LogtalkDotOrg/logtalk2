
:- object(meta_compiler,
	implements(expanding)).

	:- info([
		version is 0.5,
		date is 2011/01/13,
		author is 'Paulo Moura',
		comment is 'Compiler for the "meta" object meta-predicates. Generates auxiliary predicates in order to avoid meta-call overheads.']).

	:- uses(list, [append/3, length/2]).
	:- uses(gensym, [gensym/2]).

	:- private(generated_predicate/1).
	:- dynamic(generated_predicate/1).

	term_expansion((:- Directive), [(:- Directive)]) :-
		nonvar(Directive),
		functor(Directive, Functor, Arity),
		Arity >= 1,
		(	Functor == object, Arity =< 5 ->
			true
		;	Functor == category, Arity =< 3
		),
		retractall(generated_predicate(_)).

	term_expansion((:- end_object), [(:- end_object)]) :-
		retractall(generated_predicate(_)).

	term_expansion((:- end_protocol), [(:- end_protocol)]) :-
		retractall(generated_predicate(_)).

	term_expansion((:- end_category), [(:- end_category)]) :-
		retractall(generated_predicate(_)).

	goal_expansion(meta::callable(Term), callable(Term)) :-
		predicate_property(callable(_), built_in).

	goal_expansion(meta::ignore(Goal), (Goal->true;true)).

	goal_expansion(meta::include(Closure, List, Included), ExpandedGoal) :-
		decompose_closure(Closure, 1, Functor, Arity, Args, GArgs),
		aux_predicate_functor(include, 3, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/3) ->
			replace_functor([include_(List, Args, Included)], include_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, GArgs, [Head], Goal),
			Clauses0 = [
				include_([], _, []),
				(include_([Head| Tail], GArgs, Result) :-
					(	Goal ->
						Result = [Head| Rest]
					;	Result = Rest
					),
					include_(Tail, GArgs, Rest))
				],
			replace_functor([include_(List, Args, Included)| Clauses0], include_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/3))
		).

	goal_expansion(meta::filter(Closure, List, Included), ExpandedGoal) :-
		goal_expansion(meta::include(Closure, List, Included), ExpandedGoal).

	goal_expansion(meta::exclude(Closure, List, Excluded), ExpandedGoal) :-
		decompose_closure(Closure, 1, Functor, Arity, Args, GArgs),
		aux_predicate_functor(exclude, 3, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/3) ->
			replace_functor([exclude_(List, Args, Excluded)], exclude_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, GArgs, [Head], Goal),
			Clauses0 = [
				exclude_([], _, []),
				(exclude_([Head| Tail], GArgs, Result) :-
					(	Goal ->
						Result = Rest
					;	Result = [Head| Rest]
					),
					exclude_(Tail, GArgs, Rest))
				],
			replace_functor([exclude_(List, Args, Excluded)| Clauses0], exclude_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/3))
		).

	goal_expansion(meta::partition(Closure, List, Included, Excluded), ExpandedGoal) :-
		decompose_closure(Closure, 1, Functor, Arity, Args, GArgs),
		aux_predicate_functor(partition, 4, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/4) ->
			replace_functor([partition_(List, Args, Included, Excluded)], partition_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, GArgs, [Head], Goal),
			Clauses0 = [
				partition_([], _, [], []),
				(partition_([Head| Tail], GArgs, RIncluded, RExcluded) :-
					(   Goal ->
						RIncluded = [Head| RestIncluded],
						RExcluded = RestExcluded
					;	RIncluded = RestIncluded,
						RExcluded = [Head| RestExcluded]
					),
					partition_(Tail, GArgs, RestIncluded, RestExcluded))
				],
			replace_functor([partition_(List, Args, Included, Excluded)| Clauses0], partition_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/4))
		).

	goal_expansion(meta::partition(Closure, List, Value, Less, Equal, Greater), ExpandedGoal) :-
		decompose_closure(Closure, 3, Functor, Arity, Args, GArgs),
		aux_predicate_functor(partition, 6, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/6) ->
			replace_functor([partition_(List, Value, Args, Less, Equal, Greater)], partition_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, GArgs, [Order, X, Y], Goal),
			Clauses0 = [
				partition_([], _, _, [], [], []),
				(partition_([X| Xs], Y, GArgs, RLess, REqual, RGreater) :-
					Goal,
					partition_(Order, X, Xs, Y, GArgs, RLess, REqual, RGreater)),		
				(partition_(<, X, Xs, Y, GArgs, [X| RLess], REqual, RGreater) :-
					partition_(Xs, Y, GArgs, RLess, REqual, RGreater)),
				(partition_(=, X, Xs, Y, GArgs, RLess, [X| REqual], RGreater) :-
					partition_(Xs, Y, GArgs, RLess, REqual, RGreater)),
				(partition_(>, X, Xs, Y, GArgs, RLess, REqual, [X| RGreater]) :-
					partition_(Xs, Y, GArgs, RLess, REqual, RGreater))
				],
			replace_functor([partition_(List, Value, Args, Less, Equal, Greater)| Clauses0], partition_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/6))
		).

	goal_expansion(meta::map(Closure, List), ExpandedGoal) :-
		decompose_closure(Closure, 1, Functor, Arity, Args, GArgs),
		aux_predicate_functor(map, 2, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/2) ->
			replace_functor([map_(List, Args)], map_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, GArgs, [Head], Goal),
			Clauses0 = [
					map_([], _),
					(map_([Head| Tail], GArgs) :-
						Goal, map_(Tail, GArgs))
				],
			replace_functor([map_(List, Args)| Clauses0], map_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/2))
		).

	goal_expansion(meta::succeeds(Closure, List), ExpandedGoal) :-
		goal_expansion(meta::map(Closure, List), ExpandedGoal).

	goal_expansion(meta::map(Closure, List1, List2), ExpandedGoal) :-
		decompose_closure(Closure, 2, Functor, Arity, Args, GArgs),
		aux_predicate_functor(map, 3, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/3) ->
			replace_functor([map_(List1, Args, List2)], map_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, GArgs, [Head1, Head2], Goal),
			Clauses0 = [
					map_([], _, []),
					(map_([Head1| Tail1], GArgs, [Head2| Tail2]) :-
						Goal, map_(Tail1, GArgs, Tail2))
				],
			replace_functor([map_(List1, Args, List2)| Clauses0], map_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/3))
		).

	goal_expansion(meta::map(Closure, List1, List2, List3), ExpandedGoal) :-
		decompose_closure(Closure, 3, Functor, Arity, Args, GArgs),
		aux_predicate_functor(map, 4, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/4) ->
			replace_functor([map_(List1, Args, List2, List3)], map_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, GArgs, [Head1, Head2, Head3], Goal),
			Clauses0 = [
					map_([], _, [], []),
					(map_([Head1| Tail1], GArgs, [Head2| Tail2], [Head3| Tail3]) :-
						Goal, map_(Tail1, GArgs, Tail2, Tail3))
				],
			replace_functor([map_(List1, Args, List2, List3)| Clauses0], map_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/4))
		).

	goal_expansion(meta::map(Closure, List1, List2, List3, List4), ExpandedGoal) :-
		decompose_closure(Closure, 4, Functor, Arity, Args, GArgs),
		aux_predicate_functor(map, 5, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/5) ->
			replace_functor([map_(List1, Args, List2, List3, List4)], map_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, GArgs, [Head1, Head2, Head3, Head4], Goal),
			Clauses0 = [
					map_([], _, [], [], []),
					(map_([Head1| Tail1], GArgs, [Head2| Tail2], [Head3| Tail3], [Head4| Tail4]) :-
						Goal, map_(Tail1, GArgs, Tail2, Tail3, Tail4))
				],
			replace_functor([map_(List1, Args, List2, List3, List4)| Clauses0], map_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/5))
		).

	goal_expansion(meta::map(Closure, List1, List2, List3, List4, List5), ExpandedGoal) :-
		decompose_closure(Closure, 5, Functor, Arity, Args, GArgs),
		aux_predicate_functor(map, 6, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/6) ->
			replace_functor([map_(List1, Args, List2, List3, List4, List5)], map_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, GArgs, [Head1, Head2, Head3, Head4, Head5], Goal),
			Clauses0 = [
					map_([], _, [], [], [], []),
					(map_([Head1| Tail1], GArgs, [Head2| Tail2], [Head3| Tail3], [Head4| Tail4], [Head5| Tail5]) :-
						Goal, map_(Tail1, GArgs, Tail2, Tail3, Tail4, Tail5))
				],
			replace_functor([map_(List1, Args, List2, List3, List4, List5)| Clauses0], map_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/6))
		).

	goal_expansion(meta::map(Closure, List1, List2, List3, List4, List5, List6), ExpandedGoal) :-
		decompose_closure(Closure, 6, Functor, Arity, Args, GArgs),
		aux_predicate_functor(map, 7, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/7) ->
			replace_functor([map_(List1, Args, List2, List3, List4, List5, List6)], map_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, GArgs, [Head1, Head2, Head3, Head4, Head5, Head6], Goal),
			Clauses0 = [
					map_([], _, [], [], [], [], []),
					(map_([Head1| Tail1], GArgs, [Head2| Tail2], [Head3| Tail3], [Head4| Tail4], [Head5| Tail5], [Head6| Tail6]) :-
						Goal, map_(Tail1, GArgs, Tail2, Tail3, Tail4, Tail5, Tail6))
				],
			replace_functor([map_(List1, Args, List2, List3, List4, List5, List6)| Clauses0], map_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/7))
		).

	goal_expansion(meta::map(Closure, List1, List2, List3, List4, List5, List6, List7), ExpandedGoal) :-
		decompose_closure(Closure, 7, Functor, Arity, Args, GArgs),
		aux_predicate_functor(map, 8, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/8) ->
			replace_functor([map_(List1, Args, List2, List3, List4, List5, List6, List7)], map_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, GArgs, [Head1, Head2, Head3, Head4, Head5, Head6, Head7], Goal),
			Clauses0 = [
					map_([], _, [], [], [], [], [], []),
					(map_([Head1| Tail1], GArgs, [Head2| Tail2], [Head3| Tail3], [Head4| Tail4], [Head5| Tail5], [Head6| Tail6], [Head7| Tail7]) :-
						Goal, map_(Tail1, GArgs, Tail2, Tail3, Tail4, Tail5, Tail6, Tail7))
				],
			replace_functor([map_(List1, Args, List2, List3, List4, List5, List6, List7)| Clauses0], map_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/8))
		).

	goal_expansion(meta::fold_left(Closure, AccE, ListE, ResultE), ExpandedGoal) :-
		decompose_closure(Closure, 3, Functor, Arity, Args, GArgs),
		aux_predicate_functor(fold_left, 4, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/4) ->
			replace_functor([fold_left_(ListE, Args, AccE, ResultE)], fold_left_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, GArgs, [Acc, Head, Acc2], Goal),
			Clauses0 = [
					fold_left_([], _, Result, Result),
					(fold_left_([Head| Tail], GArgs, Acc, Result) :-
						Goal, fold_left_(Tail, GArgs, Acc2, Result))
				],
			replace_functor([fold_left_(ListE, Args, AccE, ResultE)| Clauses0], fold_left_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/4))
		).

	goal_expansion(meta::foldl(Closure, AccE, ListE, ResultE), ExpandedGoal) :-
		goal_expansion(meta::fold_left(Closure, AccE, ListE, ResultE), ExpandedGoal).

	goal_expansion(meta::fold_right(Closure, AccE, ListE, ResultE), ExpandedGoal) :-
		decompose_closure(Closure, 3, Functor, Arity, Args, GArgs),
		aux_predicate_functor(fold_right, 4, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/4) ->
			replace_functor([fold_right_(ListE, Args, AccE, ResultE)], fold_right_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, GArgs, [Head, Acc2, Result], Goal),
			Clauses0 = [
					fold_right_([], _, Result, Result),
					(fold_right_([Head| Tail], GArgs, Acc, Result) :-
						fold_right_(Tail, GArgs, Acc, Acc2), Goal)
				],
			replace_functor([fold_right_(ListE, Args, AccE, ResultE)| Clauses0], fold_right_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/4))
		).

	goal_expansion(meta::foldr(Closure, AccE, ListE, ResultE), ExpandedGoal) :-
		goal_expansion(meta::fold_right(Closure, AccE, ListE, ResultE), ExpandedGoal).

	goal_expansion(meta::scan_left(Closure, AccE, ListE, ResultsE), ExpandedGoal) :-
		decompose_closure(Closure, 3, Functor, Arity, Args, GArgs),
		aux_predicate_functor(scan_left, 4, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/4) ->
			replace_functor([scan_left_(ListE, Args, AccE, ResultsE)], scan_left_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, GArgs, [Acc, Head, Acc2], Goal),
			Clauses0 = [
					scan_left_([], _, Result, [Result]),
					(scan_left_([Head| Tail], GArgs, Acc, [Acc| Results]) :-
						Goal, scan_left_(Tail, GArgs, Acc2, Results))
				],
			replace_functor([scan_left_(ListE, Args, AccE, ResultsE)| Clauses0], scan_left_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/4))
		).

	goal_expansion(meta::scanl(Closure, AccE, ListE, ResultsE), ExpandedGoal) :-
		goal_expansion(meta::scan_left(Closure, AccE, ListE, ResultsE), ExpandedGoal).

	goal_expansion(meta::scan_right(Closure, AccE, ListE, ResultsE), ExpandedGoal) :-
		decompose_closure(Closure, 3, Functor, Arity, Args, GArgs),
		aux_predicate_functor(scan_right, 4, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/4) ->
			replace_functor([scan_right_(ListE, Args, AccE, ResultsE)], scan_right_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, GArgs, [Head, Acc2, Result], Goal),
			Clauses0 = [
					scan_right_([], _, Result, [Result]),
					(scan_right_([Head| Tail], GArgs, Acc, [Result, Acc2| Results]) :-
						scan_right_(Tail, GArgs, Acc, [Acc2| Results]), Goal)
				],
			replace_functor([scan_right_(ListE, Args, AccE, ResultsE)| Clauses0], scan_right_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/4))
		).

	goal_expansion(meta::scanr(Closure, AccE, ListE, ResultsE), ExpandedGoal) :-
		goal_expansion(meta::scan_right(Closure, AccE, ListE, ResultsE), ExpandedGoal).

	goal_expansion(meta::map_reduce(Map, Reduce, AccE, ListE, ResultE), ExpandedGoal) :-
		decompose_closure(Map, 2, MapFunctor, _, MapArgs, GMapArgs),
		decompose_closure(Reduce, 3, ReduceFunctor, _, ReduceArgs, GReduceArgs),
		atom_concat(MapFunctor, '+', Functor0),
		atom_concat(Functor0, ReduceFunctor, Functor),
		aux_predicate_functor(map_reduce, 5, Functor, 3, AuxFunctor),
		(	generated_predicate(AuxFunctor/5) ->
			replace_functor([map_reduce_(ListE, MapArgs, ReduceArgs, AccE, ResultE)], map_reduce_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(MapFunctor, GMapArgs, [Head, Head2], MapGoal),
			extend_closure(ReduceFunctor, GReduceArgs, [Acc, Head2, Acc2], ReduceGoal),
			Clauses0 = [
					map_reduce_([], _, _, Result, Result),
					(map_reduce_([Head| Tail], GMapArgs, GReduceArgs, Acc, Result) :-
						MapGoal,
						ReduceGoal,
						map_reduce_(Tail, GMapArgs, GReduceArgs, Acc2, Result))
				],
			replace_functor([map_reduce_(ListE, MapArgs, ReduceArgs, AccE, ResultE)| Clauses0], map_reduce_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/5))
		).

	decompose_closure({Free}/Parameters>>Goal, _, Functor, Arity, FreeList, GFreeList) :-
		!,
		callable(Goal),
		gensym('_lambda_', Functor),
		conjunction_to_list(Free, FreeList, Arity),
		length(GFreeList, Arity),
		append(FreeList, Parameters, Args),
		Head =.. [Functor| Args],
		logtalk::compile_clauses([(Head :- Goal)]).
	decompose_closure({Free}/(Object::Closure), MetaArity, Functor, Arity, FreeList, GFreeList) :-
		!,
		callable(Closure),
		gensym('_lambda_', Functor),
		conjunction_to_list(Free, FreeList, Arity),
		length(GFreeList, Arity),
		length(Parameters, MetaArity),
		append(FreeList, Parameters, Args),
		Head =.. [Functor| Args],
		Closure =.. [ClosureFunctor| ClosureArgs],
		append(ClosureArgs, Parameters, GoalArgs),
		Goal =.. [ClosureFunctor| GoalArgs],
		logtalk::compile_clauses([(Head :- Object::Goal)]).
	decompose_closure({Free}/{Closure}, MetaArity, Functor, Arity, FreeList, GFreeList) :-
		!,
		callable(Closure),
		gensym('_lambda_', Functor),
		conjunction_to_list(Free, FreeList, Arity),
		length(GFreeList, Arity),
		length(Parameters, MetaArity),
		append(FreeList, Parameters, Args),
		Head =.. [Functor| Args],
		Closure =.. [ClosureFunctor| ClosureArgs],
		append(ClosureArgs, Parameters, GoalArgs),
		Goal =.. [ClosureFunctor| GoalArgs],
		logtalk::compile_clauses([(Head :- {Goal})]).
	decompose_closure({Free}/Closure, MetaArity, Functor, Arity, FreeList, GFreeList) :-
		!,
		callable(Closure),
		gensym('_lambda_', Functor),
		conjunction_to_list(Free, FreeList, Arity),
		length(GFreeList, Arity),
		length(Parameters, MetaArity),
		append(FreeList, Parameters, Args),
		Head =.. [Functor| Args],
		Closure =.. [ClosureFunctor| ClosureArgs],
		append(ClosureArgs, Parameters, GoalArgs),
		Goal =.. [ClosureFunctor| GoalArgs],
		logtalk::compile_clauses([(Head :- Goal)]).
	decompose_closure(Parameters>>Goal, _, Functor, 0, [], []) :-
		!,
		callable(Goal),
		gensym('_lambda_', Functor),
		Head =.. [Functor| Parameters],
		logtalk::compile_clauses([(Head :- Goal)]).
	decompose_closure(Object::Closure, MetaArity, Object::Functor, Arity, Args, GArgs) :-
		!,
		nonvar(Closure),
		decompose_closure(Closure, MetaArity, Functor, Arity, Args, GArgs).
	decompose_closure({Closure}, MetaArity, {Functor}, Arity, Args, GArgs) :-
		!,
		nonvar(Closure),
		decompose_closure(Closure, MetaArity, Functor, Arity, Args, GArgs).
	decompose_closure(Closure, _, Functor, Arity, Args, GArgs) :-
		callable(Closure),
		Closure =.. [Functor| Args],
		functor(Closure, Functor, Arity),
		functor(GClosure, Functor, Arity),
		GClosure =.. [Functor| GArgs].

	extend_closure(Object::Functor, ClosureArgs, ExtraArgs, Object::Goal) :-
		!,
		extend_closure(Functor, ClosureArgs, ExtraArgs, Goal).
	extend_closure({Functor}, ClosureArgs, ExtraArgs, {Goal}) :-
		!,
		extend_closure(Functor, ClosureArgs, ExtraArgs, Goal).
	extend_closure(Functor, ClosureArgs, ExtraArgs, Goal) :-
		append(ClosureArgs, ExtraArgs, Args),
		Goal =.. [Functor| Args].

	replace_functor([], _, _, []).
	replace_functor([(Head0:-Body0)| Clauses0], Functor, AuxFunctor, [(Head:-Body)| Clauses]) :-
		!,
		replace_functor_head(Head0, Functor, AuxFunctor, Head),
		replace_functor_body(Body0, Functor, AuxFunctor, Body),
		replace_functor(Clauses0, Functor, AuxFunctor, Clauses).
	replace_functor([Head0| Clauses0], Functor, AuxFunctor, [Head| Clauses]) :-
		replace_functor_head(Head0, Functor, AuxFunctor, Head),
		replace_functor(Clauses0, Functor, AuxFunctor, Clauses).

	replace_functor_head(Head0, Functor, AuxFunctor, Head) :-
		(	Head0 =.. [Functor| Args] ->
			Head =.. [AuxFunctor| Args]
		;	Head = Head0
		).

	replace_functor_body((Goal01->Goal02;Goal03), Functor, AuxFunctor, (Goal1->Goal2;Goal3)) :-
		!,
		replace_functor_body(Goal01, Functor, AuxFunctor, Goal1),
		replace_functor_body(Goal02, Functor, AuxFunctor, Goal2),
		replace_functor_body(Goal03, Functor, AuxFunctor, Goal3).
	replace_functor_body((Goal01,Goal02), Functor, AuxFunctor, (Goal1,Goal2)) :-
		!,
		replace_functor_body(Goal01, Functor, AuxFunctor, Goal1),
		replace_functor_body(Goal02, Functor, AuxFunctor, Goal2).
	replace_functor_body(Goal0, Functor, AuxFunctor, Goal) :-
		replace_functor_head(Goal0, Functor, AuxFunctor, Goal).

	conjunction_to_list(Conjunction, Terms, N) :-
		conjunction_to_list(Conjunction, Terms, 1, N).

	conjunction_to_list(Term, [Term], N, N) :-
		var(Term),
		!.
	conjunction_to_list((Term, Conjunction), [Term| Terms], N0, N) :-
		!,
		N1 is N0 + 1,
		conjunction_to_list(Conjunction, Terms, N1, N).
	conjunction_to_list(Term, [Term], N, N).

	aux_predicate_functor(MetaFunctor, MetaArity, Object::ClosureFunctor, ClosureArity, AuxFunctor) :-
		!,
		atom_concat('_aux_', MetaFunctor, AuxFunctor0),
		atom_concat(AuxFunctor0, '/', AuxFunctor1),
		number_codes(MetaArity, MetaArityCodes),
		atom_codes(MetaArityAtom, MetaArityCodes),
		atom_concat(AuxFunctor1, MetaArityAtom, AuxFunctor2),
		atom_concat(AuxFunctor2, '+', AuxFunctor3),
		(	atom(Object) ->
			atom_concat(AuxFunctor3, Object, AuxFunctor4),
			atom_concat(AuxFunctor4, '.', AuxFunctor7)
		;	functor(Object, ObjectFunctor, ObjectArity),
			atom_concat(AuxFunctor3, ObjectFunctor, AuxFunctor4),
			atom_concat(AuxFunctor4, '.', AuxFunctor5),
			number_codes(ObjectArity, ObjectArityCodes),
			atom_codes(ObjectArityAtom, ObjectArityCodes),
			atom_concat(AuxFunctor5, ObjectArityAtom, AuxFunctor6),
			atom_concat(AuxFunctor6, '.', AuxFunctor7)
		),
		atom_concat(AuxFunctor7, ClosureFunctor, AuxFunctor8),
		atom_concat(AuxFunctor8, '/', AuxFunctor9),
		number_codes(ClosureArity, ClosureArityCodes),
		atom_codes(ClosureArityAtom, ClosureArityCodes),
		atom_concat(AuxFunctor9, ClosureArityAtom, AuxFunctor).
	aux_predicate_functor(MetaFunctor, MetaArity, {ClosureFunctor}, ClosureArity, AuxFunctor) :-
		!,
		atom_concat('_aux_', MetaFunctor, AuxFunctor0),
		atom_concat(AuxFunctor0, '/', AuxFunctor1),
		number_codes(MetaArity, MetaArityCodes),
		atom_codes(MetaArityAtom, MetaArityCodes),
		atom_concat(AuxFunctor1, MetaArityAtom, AuxFunctor2),
		atom_concat(AuxFunctor2, '+{', AuxFunctor3),
		atom_concat(AuxFunctor3, ClosureFunctor, AuxFunctor4),
		atom_concat(AuxFunctor4, '/', AuxFunctor5),
		number_codes(ClosureArity, ClosureArityCodes),
		atom_codes(ClosureArityAtom, ClosureArityCodes),
		atom_concat(AuxFunctor5, ClosureArityAtom, AuxFunctor6),
		atom_concat(AuxFunctor6, '}', AuxFunctor).
	aux_predicate_functor(MetaFunctor, MetaArity, ClosureFunctor, ClosureArity, AuxFunctor) :-
		atom_concat('_aux_', MetaFunctor, AuxFunctor0),
		atom_concat(AuxFunctor0, '/', AuxFunctor1),
		number_codes(MetaArity, MetaArityCodes),
		atom_codes(MetaArityAtom, MetaArityCodes),
		atom_concat(AuxFunctor1, MetaArityAtom, AuxFunctor2),
		atom_concat(AuxFunctor2, '+', AuxFunctor3),
		atom_concat(AuxFunctor3, ClosureFunctor, AuxFunctor4),
		atom_concat(AuxFunctor4, '/', AuxFunctor5),
		number_codes(ClosureArity, ClosureArityCodes),
		atom_codes(ClosureArityAtom, ClosureArityCodes),
		atom_concat(AuxFunctor5, ClosureArityAtom, AuxFunctor).

:- end_object.
