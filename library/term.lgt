
:- object(term,
	implements(termp)).

	:- info([
		version is 1.4,
		author is 'Paulo Moura',
		date is 2009/7/5,
		comment is 'Prolog term utility predicates.']).

	:- alias(termp, variables/2, vars/2).

	depth(Term, Depth) :-
		depth(Term, 0, 0, Depth).

	depth(Var, Acc, MaxSoFar, Depth) :-
		var(Var),
		!,
		(	Acc > MaxSoFar ->
		 	Depth = Acc
		;	Depth = MaxSoFar
		).
	depth(Atomic, Acc, MaxSoFar, Depth) :-
		atomic(Atomic),
		!,
		(	Acc > MaxSoFar ->
			Depth = Acc
		;	Depth = MaxSoFar
		).
	depth([Arg| Args], Acc, MaxSoFar, Depth) :-
		!,
		depth(Arg, Acc, MaxSoFar, ArgDepth),
		depth(Args, Acc, ArgDepth, Depth).
	depth(Term, Acc, MaxSoFar, Depth) :-
		Acc2 is Acc + 1,
		Term =.. [_| Args],
		depth(Args, Acc2, MaxSoFar, Depth).

	:- if(predicate_property(ground(_), built_in)).

		ground(Term) :-
			{ground(Term)}.

	:- else.

		ground(Term) :-
			nonvar(Term),
			functor(Term, _, Arity),
			ground(Arity, Term).

		ground(0, _) :-
			!.
		ground(N, Term) :-
			N > 0,
			arg(N, Term, Arg),
			ground(Arg),
			N2 is N - 1,
			ground(N2, Term).

	:- endif.

	occurs(Var, Term) :-
		(	var(Term) ->
			Var == Term
		;	functor(Term, _, Arity),
			occurs(Arity, Var, Term)
		).

	occurs(N, Var, Term) :-
		compound(Term),
		arg(N, Term, Arg),
		occurs(Var, Arg),
		!.
	occurs(N, Var, Term) :-
		N > 1,
		N2 is N - 1,
		occurs(N2, Var, Term).

	:- if(predicate_property(subsumes(_, _), built_in)).

		subsumes(General, Specific) :-
			{subsumes(General, Specific)}.

	:- else.

		subsumes(General, Specific) :-
			variables(Specific, Vars),
			subsumes(General, Specific, Vars).

		subsumes(General, Specific, Vars) :-
			var(General),
			!,
			(	var_member_chk(General, Vars) ->
				General == Specific
			;	General = Specific
			).

		subsumes(General, Specific, Vars) :-
			nonvar(Specific),
			functor(General, Functor, Arity),
			functor(Specific, Functor, Arity),
			subsumes(Arity, General, Specific, Vars).

		subsumes(0, _, _, _) :-
			!.
		subsumes(N, General, Specific, Vars) :-
			arg(N, General,  GenArg),
			arg(N, Specific, SpeArg),
			subsumes(GenArg, SpeArg, Vars),
			M is N-1, !,
			subsumes(M, General, Specific, Vars).

	:- endif.

	var_member_chk(Var, [Head| Tail]) :-
		(	Var == Head ->
			true
		;	var_member_chk(Var, Tail)
		).

	subterm(Term, Term).
	subterm(Sub, Term) :-
		nonvar(Term),
		functor(Term, _, N),
		subterm(N, Sub, Term).

	subterm(N, Sub, Term) :-
		compound(Term),
		arg(N, Term, Arg),
		subterm(Sub, Arg).
	subterm(N, Sub, Term) :-
		N > 1,
		M is N-1,
		subterm(M, Sub, Term).

	valid(_).

	check(_).

	variant(Term1, Term2) :-
		\+ \+ subsumes(Term1, Term2),
		\+ \+ subsumes(Term2, Term1).

	:- if((
		current_logtalk_flag(prolog_dialect, Prolog),
		(Prolog == swi; Prolog == yap),
		predicate_property(term_variables(_,_), built_in)
	)).

		vars(Term, Vars) :-			% deprecated
			{term_variables(Term, Vars)}.

		variables(Term, Vars) :-
			{term_variables(Term, Vars)}.

	:- else.

		vars(Term, Vars) :-			% deprecated
			variables(Term, Vars).

		variables(Term, Vars) :-
			vars(Term, [], List),
			reverse(List, [], Vars).

		vars(Term, Acc, Vars) :-
			(	var(Term) ->
				(	var_member_chk(Term, Acc) ->
					Vars = Acc
				;	Vars = [Term| Acc]
				)
			;	Term =.. [_| Args],
				vars_list(Args, Acc, Vars)
			).

		vars_list([], Vars, Vars).
		vars_list([Term| Terms], Acc, Vars) :-
			vars(Term, Acc, Acc2),
			vars_list(Terms, Acc2, Vars).

		reverse([], Reversed, Reversed).
		reverse([Head| Tail], List, Reversed) :-
			reverse(Tail, [Head| List], Reversed).

	:- endif.

	singletons(Term, Singletons) :-
		term_to_vars(Term, [], Vars),
		vars_to_singletons(Vars, [], [], Singletons).

	term_to_vars(Term, Acc, Vars) :-
		(	var(Term) ->
			Vars = [Term| Acc]
		;	Term =.. [_| Args],
			term_to_vars_list(Args, Acc, Vars)
		).

	term_to_vars_list([], Vars, Vars).
	term_to_vars_list([Term| Terms], Acc, Vars) :-
		term_to_vars(Term, Acc, Acc2),
		term_to_vars_list(Terms, Acc2, Vars).

	vars_to_singletons([], _, Singletons, Singletons).
	vars_to_singletons([Var| Vars], Repeated, Acc, Singletons) :-
		(	var_member_chk(Var, Repeated) ->
			Repeated2 = Repeated,
			Acc2 = Acc
		;	var_member_chk(Var, Vars) ->
			Repeated2 = [Var| Repeated],
			Acc2 = Acc
		;	Repeated = Repeated2,
			Acc2 = [Var| Acc]
		),
		vars_to_singletons(Vars, Repeated2, Acc2, Singletons).

:- end_object.
