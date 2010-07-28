
:- object(coinduction_with_constraints,
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
		atom_concat(stack_, F, SFn),
		number_chars(N, NChars),
		atom_chars(NAtom, NChars),
		atom_concat(SFn, NAtom, SF),
		initval(SF, []),
		Clauses = [(S :- getval(SF,L), (This::in_stack(S,L); \+ This::in_stack(S,L), setval(SF,[S|L]), NS))],
		assertz(coinductive(S,F,N,NS)).

	match_args(0, _, _) :-
		!.
	match_args(I, S1, S2) :-
		arg(I, S1, A),
		arg(I, S2, A),
        I1 is I - 1,
		match_args(I1, S1, S2).

	:- public(in_stack/2).

	:- if(current_logtalk_flag(prolog_dialect, eclipse)).

		in_stack(G, [G| _]).
		in_stack(G, [_| T]) :-
			in_stack(G, T).

		%goal_expansion(getval(Name, Value), getval(Name, Value)).
		%goal_expansion(setval(Name, Value), setval(Name, Value)).

		initval(Name, Value) :-
			{setval(Name, Value)}.

	:- else.	% assume either SWI-Prolog or YAP

		in_stack(G, [R| _]) :-
			copy_term_nat(G-R, GC-RC),
			GC = RC.
		in_stack(G, [_| T]) :-
			in_stack(G, T).

		goal_expansion(getval(Name, Value), b_getval(Name, Value)).
		goal_expansion(setval(Name, Value), b_setval(Name, Value)).

		initval(Name, Value) :-
			{nb_setval(Name, Value)}.

	:- endif.

:- end_object.
