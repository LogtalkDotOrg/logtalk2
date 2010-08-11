
:- object(coinduction,
	implements(expanding)).

	:- info([
		version is 0.3,
		author is 'Ajay Bansal and Vitor Santos Costa. Adapted to Logtalk by Paulo Moura.',
		date is 2010/07/26,
		comment is 'Supports coinductive code in Logtalk objects when used as a hook object.']).

	:- private(coinductive/4).
	:- dynamic(coinductive/4).

	term_expansion((:- coinductive(Spec)), Clauses) :-
		coinductive(Spec, Clauses).

	term_expansion((:- coinductive1(Spec)), Clauses) :-
		coinductive1(Spec, Clauses).

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
			(:- initialization(init_stack(SF, []))),
			(S :- get_stack(SF,L), (This::in_stack(S,L); set_stack(SF,[S|L]), NS))],
		assertz(coinductive(S,F,N,NS)).

	coinductive1(F/N, Clauses) :-
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
			(:- initialization(init_stack(SF, []))),
			(S :- get_stack(SF,L), (This::in_stack(S,L) *-> true; set_stack(SF,[S|L]), NS))],
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

	:- if(current_logtalk_flag(prolog_dialect, eclipse)).

		goal_expansion(init_stack(Name, Value), local(reference(Name, Value))).
		goal_expansion(get_stack(Name, Value), getval(Name, Value)).
		goal_expansion(set_stack(Name, Value), setval(Name, Value)).

	:- elif((current_logtalk_flag(prolog_dialect, swi); current_logtalk_flag(prolog_dialect, yap))).

		goal_expansion(init_stack(Name, Value), nb_setval(Name, Value)).
		goal_expansion(get_stack(Name, Value), b_getval(Name, Value)).
		goal_expansion(set_stack(Name, Value), b_setval(Name, Value)).

	:- else.

		:- initialization((write('ERROR: coinduction not supported!'), nl)).

	:- endif.

:- end_object.
