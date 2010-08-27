
:- object(coinduction,
	implements(expanding)).

	:- info([
		version is 0.5,
		author is 'Ajay Bansal and Vitor Santos Costa. Adapted to Logtalk by Paulo Moura.',
		date is 2010/08/27,
		comment is 'Supports coinductive code in Logtalk objects when used as a hook object.']).

	:- private(coinductive/5).
	:- dynamic(coinductive/5).

	term_expansion((:- coinductive(Spec)), Clauses) :-
		coinductive(Spec, Clauses).

	term_expansion((H:-B), (NH:-NB)) :-
		this(This),
		coinductive(H, _F, _N, NH, SF), !,
		NB = (get_stack(SF,L), \+ This::in_stack(H,L), set_stack(SF,[H|L]), B).		% experimental-style coinduction
%		NB = (get_stack(SF,L), set_stack(SF,[H|L]), B).								% both old- and new-style coinduction
%		NB = (get_stack(SF,L), \+ This::in_stack(H,L), set_stack(SF,[H|L]), writeq(stack-H-L), nl, get_code(_), B).

	term_expansion(H, NH) :-
		coinductive(H, _F, _N, NH, _), !.

	term_expansion((:- end_object), [(:- end_object)]) :-
		retractall(coinductive(_, _, _, _, _)).

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
			(S :- (get_stack(SF,L), This::in_stack(S,L); NS))],				% old- and experimental-style coinduction
%			(S :- (get_stack(SF,L), This::in_stack(S,L) *-> true; NS))],	% new-style coinduction
		assertz(coinductive(S,F,N,NS,SF)).

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
