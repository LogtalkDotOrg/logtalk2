
:- object(coinduction,
	implements(expanding)).

	:- info([
		version is 0.1,
		author is 'Ajay Bansal. Adapted to Logtalk by Paulo Moura.',
		date is 2010/07/26,
		comment is 'Supports coinductive code in Logtalk objects when used as a hook object.']).

	:- private(coinductive/4).
	:- dynamic(coinductive/4).

	term_expansion((:- coinductive(Spec)), Clauses) :-
		coinductive(Spec, Clauses).

	term_expansion((H:-B), (NH:-B)) :-
		coinductive(H, _F, _N, NH), !.

	term_expansion(H, NH) :-
		coinductive(H, _F, _N, NH), !.
		
	coinductive(F/N, Clauses) :-
		this(This),
		functor(S, F, N),
		atom_concat(coinductive_, F, NF),
		functor(NS, NF, N),
		match_args(N, S, NS),
		logtalk_load_context(entity_prefix, Prefix),
		atom_concat(Prefix, stack_, SPrefix),
		atom_concat(SPrefix, F, SFn),
		number_chars(N, NChars),
		atom_chars(NAtom, NChars),
		atom_concat(SFn, NAtom, SF),
		Clauses = [
			(:- initialization(initval(SF, []))),
			(S :- getval(SF,L), (This::in_stack(S,L); \+ This::in_stack(S,L), setval(SF,[S|L]), NS))],
		assertz(coinductive(S,F,N,NS)).

	match_args(0, _, _) :-
		!.
	match_args(I, S1, S2) :-
		arg(I, S1, A),
		arg(I, S2, A),
        I1 is I - 1,
		match_args(I1, S1, S2).

	:- public(in_stack/2).

	in_stack(G, [G| _]).
	in_stack(G, [_| T]) :-
		in_stack(G, T).

	:- if(current_logtalk_flag(prolog_dialect, b)).

		goal_expansion(initval(Name, Value), global_set(Name, Value)).
		goal_expansion(getval(Name, Value), global_heap_get(Name, Value)).
		goal_expansion(setval(Name, Value), global_heap_set(Name, Value)).

	:- elif(current_logtalk_flag(prolog_dialect, eclipse)).

		goal_expansion(initval(Name, Value), local(reference(Name, Value))).
		%goal_expansion(getval(Name, Value), getval(Name, Value)).
		%goal_expansion(setval(Name, Value), setval(Name, Value)).

	:- elif(current_logtalk_flag(prolog_dialect, gnu)).

		goal_expansion(initval(Name, Value), g_assign(Name, Value)).
		goal_expansion(getval(Name, Value), g_read(Name, Value)).
		goal_expansion(setval(Name, Value), g_assignb(Name, Value)).

	:- else.	% assume either SWI-Prolog or YAP

		goal_expansion(initval(Name, Value), nb_setval(Name, Value)).
		goal_expansion(getval(Name, Value), b_getval(Name, Value)).
		goal_expansion(setval(Name, Value), b_setval(Name, Value)).

	:- endif.

:- end_object.
