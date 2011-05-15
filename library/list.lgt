
:- object(list,
	implements(listp),
	extends(compound)).

	:- info([
		version is 2.2,
		author is 'Paulo Moura',
		date is 2011/05/14,
		comment is 'List predicates.']).

	:- public(as_difflist/2).
	:- mode(as_difflist(+list, -list), one).
	:- info(as_difflist/2,
		[comment is 'Converts a list to a difference list.',
		 argnames is ['List', 'Diffist']]).

	append([], []).
	append([List| Lists], Concatenation) :-
		append(List, Tail, Concatenation),
		append(Lists, Tail).

	append([], List, List).
	append([Head| Tail], List, [Head| Tail2]) :-
		append(Tail, List, Tail2).

	as_difflist([], Back-Back).
	as_difflist([Head| Tail], [Head| Tail2]-Back) :-
		as_difflist(Tail, Tail2-Back).

	delete([], _, []).
	delete([Head| Tail], Element, Remaining) :-
		(	Head == Element ->
			delete(Tail, Element, Remaining)
		;	Remaining = [Head| Tail2],
			delete(Tail, Element, Tail2)
		).

	delete_matches([], _, []).
	delete_matches([Head| Tail], Element, Remaining) :-
		(	\+ \+ Head = Element ->
			delete_matches(Tail, Element, Remaining)
		;	Remaining = [Head| Tail2],
			delete_matches(Tail, Element, Tail2)
		).

	empty(List) :-
		List == [].

	flatten(List, Flatted) :-
		flatten(List, [], Flatted).

	flatten(Var, Tail, [Var| Tail]) :-
		var(Var),
		!.
	flatten([], Flatted, Flatted) :-
		!.
	flatten([Head| Tail], List, Flatted) :-
		!,
		flatten(Tail, List, Aux),
		flatten(Head, Aux, Flatted).
	flatten(Head, Tail, [Head| Tail]).

	keysort(List, Sorted) :-
		{keysort(List, Sorted)}.		

	last([Head| Tail], Last) :-
		last(Tail, Head, Last).

	last([], Last, Last).
	last([Head| Tail], _, Last) :-
		last(Tail, Head, Last).

	length(List, Length) :-
		(	integer(Length) ->
			Length >= 0,
			make_list(Length, List)
		;	var(Length) ->
			length(List, 0, Length)
		).

	make_list(N, List):-
		(	N =:= 0 ->
			List = []
		;	M is N-1,
			List = [_| Tail],
			make_list(M, Tail)
		).

	length([], Length, Length).
	length([_| Tail], Acc, Length) :-
		Acc2 is Acc + 1,
		length(Tail, Acc2, Length).

	max([N| Ns], Max) :-
		max(Ns, N, Max).

	max([], Max, Max).
	max([N| Ns], Aux, Max) :-
		(	N @> Aux ->
			max(Ns, N, Max)
		;	max(Ns, Aux, Max)
		).

	member(Element, [Element| _]).
	member(Element, [_| List]) :-
		member(Element, List).

	memberchk(Element, [Element| _]) :-
		!.
	memberchk(Element, [_| List]) :-
		memberchk(Element, List).

	min([N| Ns], Min) :-
		min(Ns, N, Min).

	min([], Min, Min).
	min([N| Ns], Aux, Min) :-
		(	N @< Aux ->
			min(Ns, N, Min)
		;	min(Ns, Aux, Min)
		).

	:- if(predicate_property(msort(_, _), built_in)).

		msort(List, Sorted) :-
			{msort(List, Sorted)}.

	:- elif(current_logtalk_flag(prolog_dialect, gnu)).

		msort(List, Sorted) :-
			{sort0(List, Sorted)}.

	:- else.

		msort([], []) :- !.
		msort([X], [X]) :- !.
		msort([X, Y| Xs], Ys) :-
			split([X, Y| Xs], X1s, X2s),
			msort(X1s, Y1s),
			msort(X2s, Y2s),
			merge(Y1s, Y2s, Ys).

		merge([X| Xs], [Y| Ys], [X| Zs]) :-
			X @=< Y, !,
			merge(Xs, [Y| Ys], Zs).
		merge([X| Xs], [Y| Ys], [Y| Zs]) :-
			X @> Y, !,
			merge([X | Xs], Ys, Zs).
		merge([], Xs, Xs) :- !.
		merge(Xs, [], Xs).

	:- endif.

	:- meta_predicate(msort(3, *, *)).
	:- meta_predicate(msort_(*, 3, *)).
	:- meta_predicate(merge_keep_dups(*, *, 3, *)).

	msort(Compare, List, Sorted) :-
		msort_(List, Compare, Sorted).

	msort_([], _, []) :- !.
	msort_([X], _, [X]) :- !.
	msort_([X, Y| Xs], C, Ys) :-
		split([X, Y| Xs], X1s, X2s),
		msort_(X1s, C, Y1s),
		msort_(X2s, C, Y2s),
		merge_keep_dups(Y1s, Y2s, C, Ys).

	split([], [], []).
	split([X| Xs], [X| Ys], Zs) :-
		split(Xs, Zs, Ys).

	merge_keep_dups([], Xs, _, Xs) :- !.
	merge_keep_dups(Xs, [], _, Xs) :- !.
	merge_keep_dups([X| Xs], [Y| Ys], C, Zs) :-
		call(C, R, X, Y),
		merge_keep_dups(R, [X| Xs], [Y| Ys], C, Zs).

	merge_keep_dups(<, [X| Xs], [Y| Ys], C, [X| Zs]) :-
		merge_keep_dups(Xs, [Y| Ys], C, Zs).
	merge_keep_dups(=, [X| Xs], [Y| Ys], C, [X| Zs]) :-
		merge_keep_dups(Xs, [Y| Ys], C, Zs).
	merge_keep_dups(>, [X| Xs], [Y| Ys], C, [Y| Zs]) :-
		merge_keep_dups([X | Xs], Ys, C, Zs).

	new([]).

	nextto(X, Y, [X, Y| _]).
	nextto(X, Y, [_| Tail]) :-
		nextto(X, Y, Tail).

	nth0(Nth, List, Element) :-
		nth(Element, List, 0, Nth, _).

	nth0(Nth, List, Element, Tail) :-
		nth(Element, List, 0, Nth, Tail).

	nth1(Nth, List, Element) :-
		nth(Element, List, 1, Nth, _).

	nth1(Nth, List, Element, Tail) :-
		nth(Element, List, 1, Nth, Tail).

	nth(Element, List, Acc, Nth, Tail) :-
		(	integer(Nth),
			Nth >= Acc,
			nth_aux(NthElement, List, Acc, Nth, Tail) ->
			Element = NthElement
		;	var(Nth),
			nth_aux(Element, List, Acc, Nth, Tail)
		).

	nth_aux(Head, [Head| Tail], Position, Position, Tail).
	nth_aux(Head, [_| List], Count, Position, Tail) :-
		Count2 is Count + 1,
		nth_aux(Head, List, Count2, Position, Tail).

	partition([], _, [], [], []).
	partition([X| Xs], Y, Less, Equal, Greater) :-
		compare(Order, X, Y),
		partition(Order, X, Xs, Y, Less, Equal, Greater).
	
	partition(<, X, Xs, Y, [X| Less], Equal, Greater) :-
		partition(Xs, Y, Less, Equal, Greater).
	partition(=, X, Xs, Y, Less, [X| Equal], Greater) :-
		partition(Xs, Y, Less, Equal, Greater).
	partition(>, X, Xs, Y, Less, Equal, [X| Greater]) :-
		partition(Xs, Y, Less, Equal, Greater).

	permutation(List, Permutation) :-
		same_length(List, Permutation),
		permutation2(List, Permutation).

	permutation2([], []).
	permutation2(List, [Head| Tail]) :-
		select(Head, List, Remaining),
		permutation2(Remaining, Tail).

	prefix([], _).
	prefix([Element| Tail], [Element| Tail2]) :-
		prefix(Tail, Tail2).

	proper_prefix([], [_| _]).
	proper_prefix([Head| PrefixTail], [Head| ListTail]) :-
		proper_prefix(PrefixTail, ListTail).

	reverse(List, Reversed) :-
		reverse(List, [], Reversed, Reversed).

	reverse([], Reversed, Reversed, []).
	reverse([Head| Tail], List, Reversed, [_| Bound]) :-
		reverse(Tail, [Head| List], Reversed, Bound).

	same_length([], []).
	same_length([_| Tail1], [_| Tail2]) :-
		same_length(Tail1, Tail2).

	same_length(List1, List2, Length) :-
		(	integer(Length) ->
			same_length_length(Length, List1, List2)
		;	var(List1) ->
			same_length_list(List2, List1, 0, Length)
		;	same_length_list(List1, List2, 0, Length)
		).

	same_length_length(Length, List1, List2) :-
		(	Length =:= 0 ->
			List1 = [],
			List2 = []
		;	Length > 0,
			Length2 is Length - 1,
			List1 = [_| Tail1],
			List2 = [_| Tail2],
			same_length_length(Length2, Tail1, Tail2)
		).

	same_length_list([], [], Length, Length).
	same_length_list([_| Tail1], [_| Tail2], Acc, Length) :-
		Acc2 is Acc + 1,
		same_length_list(Tail1, Tail2, Acc2, Length).

	select(Head, [Head| Tail], Tail).
	select(Head, [Head2| Tail], [Head2| Tail2]) :-
		select(Head, Tail, Tail2).

	selectchk(Elem, List, Remaining) :-
		select(Elem, List, Rest) ->
		Remaining = Rest.

	select(Old, [Old| Tail], New, [New| Tail]).
	select(Old, [Head| OldTail], New, [Head| NewTail]) :-
		select(Old, OldTail, New, NewTail).

	selectchk(Old, [Old| Tail], New, [New| Tail]) :-
		!.
	selectchk(Old, [Head| OldTail], New, [Head| NewTail]) :-
		selectchk(Old, OldTail, New, NewTail).

	:- if(predicate_property(sort(_, _), built_in)).

		sort(List, Sorted) :-
			{sort(List, Sorted)}.		

	:- else.

		sort(List, Sorted) :-
			setof(Element, member(Element, List), Sorted).		

	:- endif.

	:- meta_predicate(sort(3, *, *)).
	:- meta_predicate(sort_(*, 3, *)).
	:- meta_predicate(merge_no_dups(*, *, 3, *)).

	sort(Compare, List, Sorted) :-
		sort_(List, Compare, Sorted).

	sort_([], _, []) :- !.
	sort_([X], _, [X]) :- !.
	sort_([X, Y| Xs], C, Ys) :-
		split([X, Y| Xs], X1s, X2s),
		sort_(X1s, C, Y1s),
		sort_(X2s, C, Y2s),
		merge_no_dups(Y1s, Y2s, C, Ys).

	merge_no_dups([], Xs, _, Xs) :- !.
	merge_no_dups(Xs, [], _, Xs) :- !.
	merge_no_dups([X| Xs], [Y| Ys], C, Zs) :-
		call(C, R, X, Y),
		merge_no_dups(R, [X| Xs], [Y| Ys], C, Zs).

	merge_no_dups(<, [X| Xs], [Y| Ys], C, [X| Zs]) :-
		merge_no_dups(Xs, [Y| Ys], C, Zs).
	merge_no_dups(=, [X| Xs], [_| Ys], C, [X| Zs]) :-
		merge_no_dups(Xs, Ys, C, Zs).
	merge_no_dups(>, [X| Xs], [Y| Ys], C, [Y| Zs]) :-
		merge_no_dups([X | Xs], Ys, C, Zs).

	sublist(List, List).
	sublist(Sublist, [Head| Tail]):-
		sublist(Tail, Head, Sublist).

	sublist(Sublist, _, Sublist).
	sublist([Head| Tail], _, Sublist):-
		sublist(Tail, Head, Sublist).
	sublist([Head| Tail], Element, [Element| Sublist]):-
		sublist(Tail, Head, Sublist).

	subsequence([], [], []).
	subsequence([Head| Tail], Subsequence, [Head| Remaining]) :-
		subsequence(Tail, Subsequence, Remaining).
	subsequence([Head| Tail], [Head| Subsequence], Remaining) :-
		subsequence(Tail, Subsequence, Remaining).

	subtract([], _, []).
	subtract([Head| Tail], List, Rest) :-
		(	memberchk(Head, List) ->
			subtract(Tail, List, Rest)
		;	Rest = [Head| Tail2],
			subtract(Tail, List, Tail2)
		).

	suffix(List, List).
	suffix(List, [_| Tail]) :-
		suffix(List, Tail).

	proper_suffix(Suffix, [_| Tail]) :-
		suffix(Suffix, Tail).

	valid(-) :-		% catch variables and lists with unbound tails
		!,
		fail.
	valid([]).
	valid([_| List]) :-
		valid(List).

	check(Term) :-
		this(This),
		sender(Sender),
		(	valid(Term) ->
			true
		;	var(Term) ->
			throw(error(instantiation_error, This::check(Term), Sender))
		;	throw(error(type_error(This, Term), This::check(Term), Sender))
		).

:- end_object.
