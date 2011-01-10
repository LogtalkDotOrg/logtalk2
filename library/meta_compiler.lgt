
:- object(meta_compiler,
	implements(expanding)).

	:- info([
		version is 0.3,
		date is 2011/01/10,
		author is 'Paulo Moura',
		comment is 'Compiler for the "meta" object meta-predicates. Generates auxiliary predicates in order to avoid meta-call overheads.']).

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
		decompose_closure(Closure, Functor, Arity, Args),
		aux_predicate_functor(include, 3, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/2) ->
			replace_functor([include_(List, Included)], include_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, Args, [Head], Goal),
			Clauses0 = [
				include_([], []),
				(include_([Head| Tail], Result) :-
					(	Goal ->
						Result = [Head| Rest]
					;	Result = Rest
					),
					include_(Tail, Rest))
				],
			replace_functor([include_(List, Included)| Clauses0], include_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/2))
		).

	goal_expansion(meta::filter(Closure, List, Included), ExpandedGoal) :-
		goal_expansion(meta::include(Closure, List, Included), ExpandedGoal).

	goal_expansion(meta::exclude(Closure, List, Excluded), ExpandedGoal) :-
		decompose_closure(Closure, Functor, Arity, Args),
		aux_predicate_functor(exclude, 3, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/2) ->
			replace_functor([exclude_(List, Excluded)], exclude_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, Args, [Head], Goal),
			Clauses0 = [
				exclude_([], []),
				(exclude_([Head| Tail], Result) :-
					(	Goal ->
						Result = Rest
					;	Result = [Head| Rest]
					),
					exclude_(Tail, Rest))
				],
			replace_functor([exclude_(List, Excluded)| Clauses0], exclude_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/2))
		).

	goal_expansion(meta::partition(Closure, List, Included, Excluded), ExpandedGoal) :-
		decompose_closure(Closure, Functor, Arity, Args),
		aux_predicate_functor(partition, 4, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/3) ->
			replace_functor([partition_(List, Included, Excluded)], partition_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, Args, [Head], Goal),
			Clauses0 = [
				partition_([], [], []),
				(partition_([Head| Tail], RIncluded, RExcluded) :-
					(   Goal ->
						RIncluded = [Head| RestIncluded],
						RExcluded = RestExcluded
					;	RIncluded = RestIncluded,
						RExcluded = [Head| RestExcluded]
					),
					partition_(Tail, RestIncluded, RestExcluded))
				],
			replace_functor([partition_(List, Included, Excluded)| Clauses0], partition_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/3))
		).

	goal_expansion(meta::partition(Closure, List, Value, Less, Equal, Greater), ExpandedGoal) :-
		decompose_closure(Closure, Functor, Arity, Args),
		aux_predicate_functor(partition, 6, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/5) ->
			replace_functor([partition_(List, Value, Less, Equal, Greater)], partition_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, Args, [Order, X, Y], Goal),
			Clauses0 = [
				partition_([], _, [], [], []),
				(partition_([X| Xs], Y, RLess, REqual, RGreater) :-
					Goal,
					partition_(Order, X, Xs, Y, RLess, REqual, RGreater)),		
				(partition_(<, X, Xs, Y, [X| RLess], REqual, RGreater) :-
					partition_(Xs, Y, RLess, REqual, RGreater)),
				(partition_(=, X, Xs, Y, RLess, [X| REqual], RGreater) :-
					partition_(Xs, Y, RLess, REqual, RGreater)),
				(partition_(>, X, Xs, Y, RLess, REqual, [X| RGreater]) :-
					partition_(Xs, Y, RLess, REqual, RGreater))
				],
			replace_functor([partition_(List, Value, Less, Equal, Greater)| Clauses0], partition_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/5))
		).

	goal_expansion(meta::map(Closure, List), ExpandedGoal) :-
		decompose_closure(Closure, Functor, Arity, Args),
		aux_predicate_functor(map, 2, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/1) ->
			replace_functor([map_(List)], map_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, Args, [Head], Goal),
			Clauses0 = [
					map_([]),
					(map_([Head| Tail]) :-
						Goal, map_(Tail))
				],
			replace_functor([map_(List)| Clauses0], map_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/1))
		).

	goal_expansion(meta::succeeds(Closure, List), ExpandedGoal) :-
		goal_expansion(meta::map(Closure, List), ExpandedGoal).

	goal_expansion(meta::map(Closure, List1, List2), ExpandedGoal) :-
		decompose_closure(Closure, Functor, Arity, Args),
		aux_predicate_functor(map, 3, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/2) ->
			replace_functor([map_(List1, List2)], map_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, Args, [Head1, Head2], Goal),
			Clauses0 = [
					map_([], []),
					(map_([Head1| Tail1], [Head2| Tail2]) :-
						Goal, map_(Tail1, Tail2))
				],
			replace_functor([map_(List1, List2)| Clauses0], map_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/2))
		).

	goal_expansion(meta::map(Closure, List1, List2, List3), ExpandedGoal) :-
		decompose_closure(Closure, Functor, Arity, Args),
		aux_predicate_functor(map, 4, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/3) ->
			replace_functor([map_(List1, List2, List3)], map_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, Args, [Head1, Head2, Head3], Goal),
			Clauses0 = [
					map_([], [], []),
					(map_([Head1| Tail1], [Head2| Tail2], [Head3| Tail3]) :-
						Goal, map_(Tail1, Tail2, Tail3))
				],
			replace_functor([map_(List1, List2, List3)| Clauses0], map_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/3))
		).

	goal_expansion(meta::map(Closure, List1, List2, List3, List4), ExpandedGoal) :-
		decompose_closure(Closure, Functor, Arity, Args),
		aux_predicate_functor(map, 5, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/4) ->
			replace_functor([map_(List1, List2, List3, List4)], map_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, Args, [Head1, Head2, Head3, Head4], Goal),
			Clauses0 = [
					map_([], [], [], []),
					(map_([Head1| Tail1], [Head2| Tail2], [Head3| Tail3], [Head4| Tail4]) :-
						Goal, map_(Tail1, Tail2, Tail3, Tail4))
				],
			replace_functor([map_(List1, List2, List3, List4)| Clauses0], map_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/4))
		).

	goal_expansion(meta::map(Closure, List1, List2, List3, List4, List5), ExpandedGoal) :-
		decompose_closure(Closure, Functor, Arity, Args),
		aux_predicate_functor(map, 6, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/5) ->
			replace_functor([map_(List1, List2, List3, List4, List5)], map_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, Args, [Head1, Head2, Head3, Head4, Head5], Goal),
			Clauses0 = [
					map_([], [], [], [], []),
					(map_([Head1| Tail1], [Head2| Tail2], [Head3| Tail3], [Head4| Tail4], [Head5| Tail5]) :-
						Goal, map_(Tail1, Tail2, Tail3, Tail4, Tail5))
				],
			replace_functor([map_(List1, List2, List3, List4, List5)| Clauses0], map_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/5))
		).

	goal_expansion(meta::map(Closure, List1, List2, List3, List4, List5, List6), ExpandedGoal) :-
		decompose_closure(Closure, Functor, Arity, Args),
		aux_predicate_functor(map, 7, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/6) ->
			replace_functor([map_(List1, List2, List3, List4, List5, List6)], map_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, Args, [Head1, Head2, Head3, Head4, Head5, Head6], Goal),
			Clauses0 = [
					map_([], [], [], [], [], []),
					(map_([Head1| Tail1], [Head2| Tail2], [Head3| Tail3], [Head4| Tail4], [Head5| Tail5], [Head6| Tail6]) :-
						Goal, map_(Tail1, Tail2, Tail3, Tail4, Tail5, Tail6))
				],
			replace_functor([map_(List1, List2, List3, List4, List5, List6)| Clauses0], map_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/6))
		).

	goal_expansion(meta::map(Closure, List1, List2, List3, List4, List5, List6, List7), ExpandedGoal) :-
		decompose_closure(Closure, Functor, Arity, Args),
		aux_predicate_functor(map, 8, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/7) ->
			replace_functor([map_(List1, List2, List3, List4, List5, List6, List7)], map_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, Args, [Head1, Head2, Head3, Head4, Head5, Head6, Head7], Goal),
			Clauses0 = [
					map_([], [], [], [], [], [], []),
					(map_([Head1| Tail1], [Head2| Tail2], [Head3| Tail3], [Head4| Tail4], [Head5| Tail5], [Head6| Tail6], [Head7| Tail7]) :-
						Goal, map_(Tail1, Tail2, Tail3, Tail4, Tail5, Tail6, Tail7))
				],
			replace_functor([map_(List1, List2, List3, List4, List5, List6, List7)| Clauses0], map_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/7))
		).

	goal_expansion(meta::fold_left(Closure, AccE, ListE, ResultE), ExpandedGoal) :-
		decompose_closure(Closure, Functor, Arity, Args),
		aux_predicate_functor(fold_left, 4, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/3) ->
			replace_functor([fold_left_(ListE, AccE, ResultE)], fold_left_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, Args, [Acc, Head, Acc2], Goal),
			Clauses0 = [
					fold_left_([], Result, Result),
					(fold_left_([Head| Tail], Acc, Result) :-
						Goal, fold_left_(Tail, Acc2, Result))
				],
			replace_functor([fold_left_(ListE, AccE, ResultE)| Clauses0], fold_left_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/3))
		).

	goal_expansion(meta::foldl(Closure, AccE, ListE, ResultE), ExpandedGoal) :-
		goal_expansion(meta::fold_left(Closure, AccE, ListE, ResultE), ExpandedGoal).

	goal_expansion(meta::fold_right(Closure, AccE, ListE, ResultE), ExpandedGoal) :-
		decompose_closure(Closure, Functor, Arity, Args),
		aux_predicate_functor(fold_right, 4, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/3) ->
			replace_functor([fold_right_(ListE, AccE, ResultE)], fold_right_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, Args, [Head, Acc2, Result], Goal),
			Clauses0 = [
					fold_right_([], Result, Result),
					(fold_right_([Head| Tail], Acc, Result) :-
						fold_right_(Tail, Acc, Acc2), Goal)
				],
			replace_functor([fold_right_(ListE, AccE, ResultE)| Clauses0], fold_right_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/3))
		).

	goal_expansion(meta::foldr(Closure, AccE, ListE, ResultE), ExpandedGoal) :-
		goal_expansion(meta::fold_right(Closure, AccE, ListE, ResultE), ExpandedGoal).

	goal_expansion(meta::scan_left(Closure, AccE, ListE, ResultsE), ExpandedGoal) :-
		decompose_closure(Closure, Functor, Arity, Args),
		aux_predicate_functor(scan_left, 4, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/3) ->
			replace_functor([scan_left_(ListE, AccE, ResultsE)], scan_left_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, Args, [Acc, Head, Acc2], Goal),
			Clauses0 = [
					scan_left_([], Result, [Result]),
					(scan_left_([Head| Tail], Acc, [Acc| Results]) :-
						Goal, scan_left_(Tail, Acc2, Results))
				],
			replace_functor([scan_left_(ListE, AccE, ResultsE)| Clauses0], scan_left_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/3))
		).

	goal_expansion(meta::scanl(Closure, AccE, ListE, ResultsE), ExpandedGoal) :-
		goal_expansion(meta::scan_left(Closure, AccE, ListE, ResultsE), ExpandedGoal).

	goal_expansion(meta::scan_right(Closure, AccE, ListE, ResultsE), ExpandedGoal) :-
		decompose_closure(Closure, Functor, Arity, Args),
		aux_predicate_functor(scan_right, 4, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/3) ->
			replace_functor([scan_right_(ListE, AccE, ResultsE)], scan_right_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, Args, [Head, Acc2, Result], Goal),
			Clauses0 = [
					scan_right_([], Result, [Result]),
					(scan_right_([Head| Tail], Acc, [Result, Acc2| Results]) :-
						scan_right_(Tail, Acc, [Acc2| Results]), Goal)
				],
			replace_functor([scan_right_(ListE, AccE, ResultsE)| Clauses0], scan_right_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/3))
		).

	goal_expansion(meta::scanr(Closure, AccE, ListE, ResultsE), ExpandedGoal) :-
		goal_expansion(meta::scan_right(Closure, AccE, ListE, ResultsE), ExpandedGoal).

	goal_expansion(meta::map_reduce(Map, Reduce, AccE, ListE, ResultE), ExpandedGoal) :-
		decompose_closure(Map, MapFunctor, _, MapArgs),
		decompose_closure(Reduce, ReduceFunctor, _, ReduceArgs),
		atom_concat(MapFunctor, '+', Functor0),
		atom_concat(Functor0, ReduceFunctor, Functor),
		aux_predicate_functor(map_reduce, 5, Functor, 3, AuxFunctor),
		(	generated_predicate(AuxFunctor/3) ->
			replace_functor([map_reduce_(ListE, AccE, ResultE)], map_reduce_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(MapFunctor, MapArgs, [Head, Head2], MapGoal),
			extend_closure(ReduceFunctor, ReduceArgs, [Acc, Head2, Acc2], ReduceGoal),
			Clauses0 = [
					map_reduce_([], Result, Result),
					(map_reduce_([Head| Tail], Acc, Result) :-
						MapGoal,
						ReduceGoal,
						map_reduce_(Tail, Acc2, Result))
				],
			replace_functor([map_reduce_(ListE, AccE, ResultE)| Clauses0], map_reduce_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/3))
		).

	decompose_closure(Object::Closure, Object::Functor, Arity, Args) :-
		!,
		nonvar(Closure),
		decompose_closure(Closure, Functor, Arity, Args).
	decompose_closure({Closure}, {Functor}, Arity, Args) :-
		!,
		nonvar(Closure),
		decompose_closure(Closure, Functor, Arity, Args).
	decompose_closure(Closure, Functor, Arity, Args) :-
		callable(Closure),
		Closure =.. [Functor| Args],
		Functor \== (>>),
		Functor \== (/), 
		functor(Closure, Functor, Arity).

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

	append([], List, List).
	append([Head| Tail], List, [Head| Tail2]) :-
		append(Tail, List, Tail2).

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
