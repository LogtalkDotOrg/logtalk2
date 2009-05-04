
:- object(difflist,
	implements(listp),
	extends(compound)).

	:- info([
		version is 1.3,
		author is 'Paulo Moura',
		date is 2009/5/4,
		comment is 'Difference list predicates.']).

	:- public(as_list/2).
	:- mode(as_list(+list, -list), one).
	:- info(as_list/2,
		[comment is 'Converts a difference list to a normal list.',
		 argnames is ['Diffist', 'List']]).

	append(List1-Back1, Back1-Back2, List1-Back2) :-
		nonvar(List1),
		nonvar(Back1),
		!.
	append(Prefix, Suffix, List) :-
		length(List, Length),
		prefix(Prefix, List),
		length(Prefix, PLength),
		SLength is Length - PLength,
		length(Suffix, SLength),
		suffix(Suffix, List).

	as_list(List-Back, Out) :-
		(	List == Back ->
			Out = []
		;	Out = [Head| Tail],
			List = [Head| Rest],
			as_list(Rest-Back, Tail)
		).

	delete(List-Back, Element, Remaining) :-
		(	List == Back ->
			unify_with_occurs_check(Remaining, Back-Back)
		;	List \== Back,
			List = [Head| Tail],
			(	Head == Element ->
				delete(Tail-Back, Element, Remaining)
			;	Remaining = [Head| Tail2],
				delete(Tail-Back, Element, Tail2-Back)
			)
		).

	delete_matches(List-Back, Element, Remaining) :-
		(	List == Back ->
			unify_with_occurs_check(Remaining, Back-Back)
		;	List \== Back,
			List = [Head| Tail],
			(	\+ \+ Head = Element ->
				delete_matches(Tail-Back, Element, Remaining)
			;	Remaining = [Head| Tail2],
				delete_matches(Tail-Back, Element, Tail2-Back)
			)
		).

	empty(List-Back) :-
		List == Back.

	flatten(List-Back, Flatted-Back) :-
		flatten(List-Back, Back-Back, Flatted-Back).

	flatten(Var, Tail-Back, [Var| Tail]-Back) :-
		var(Var),
		!.
	flatten(List-Back, Flatted, Flatted) :-
		List == Back,
		!.
	flatten(List-Back, Acc, Flatted) :-
		!,
		List \== Back,
		List = [Head| Tail],
		flatten(Tail-Back, Acc, Acc2),
		flatten(Head, Acc2, Flatted).
	flatten(Head, Tail-Back, [Head| Tail]-Back).

	keysort(Difflist, Sorted) :-
		as_list(Difflist, List),
		{keysort(List, List2)},
		list::as_difflist(List2, Sorted).		

	last(List-Back, Last) :-
		List \== Back,
		List = [Head| Tail],
		last(Tail-Back, Head, Last).

	last(List, Last, Last) :-
		unify_with_occurs_check(List, Back-Back).
	last(List-Back, _, Last) :-
		List \== Back,
		List = [Head| Tail],
		last(Tail-Back, Head, Last).

	length(List, Length) :-
		(	integer(Length) ->
			Length >= 0,
			make_list(Length, List)
		;	length(List, 0, Length)
		).

	length(List, Length, Length) :-
		unify_with_occurs_check(List, Back-Back).
	length(List-Back, Acc, Length) :-
		List \== Back,
		List = [_| Tail],
		Acc2 is Acc + 1,
		length(Tail-Back, Acc2, Length).

	make_list(0, List):-
		!,
		unify_with_occurs_check(List, Back-Back).
	make_list(N, List-Back):-
		List \== Back,
		List = [_| Tail],
		M is N-1,
		make_list(M, Tail-Back).

	max(List-Back, Max) :-
		List \== Back,
		List = [Head| Tail],
		max(Tail-Back, Head, Max).

	max(List-Back, Max, Max) :-
		List == Back, !.
	max(List-Back, Aux, Max) :-
		List \== Back,
		List = [Head| Tail],
		(	Aux @< Head ->
			max(Tail-Back, Head, Max)
		;	max(Tail-Back, Aux, Max)
		).

	member(Element, List-Back) :-
		List \== Back,
		List = [Element|_].
	member(Element, List-Back) :-
		List \== Back,
		List = [_| Tail],
		member(Element, Tail-Back).

	memberchk(Element, List) :-
		once(member(Element, List)).

	nth0(Position, List, Element) :-
		nth(Element, List, 0, Position, _).

	nth0(Nth, List, Element, Tail) :-
		nth(Element, List, 0, Nth, Tail).

	nth1(Position, List, Element) :-
		nth(Element, List, 1, Position, _).

	nth1(Nth, List, Element, Tail) :-
		nth(Element, List, 1, Nth, Tail).

	nth(Element, List-Back, Position, Position, Tail-Back) :-
		List \== Back,
		List = [Element| Tail].
	nth(Element, List-Back, Count, Position, Tail-Back) :-
		List \== Back,
		List = [_| List2],
		Count2 is Count + 1,
		nth(Element, List2-Back, Count2, Position, Tail-Back).

	min(List-Back, Min) :-
		List \== Back,
		List = [Head| Tail],
		min(Tail-Back, Head, Min).

	min(List-Back, Min, Min) :-
		List == Back, !.
	min(List-Back, Aux, Min) :-
		List \== Back,
		List = [Head| Tail],
		(	Head @< Aux ->
			min(Tail-Back, Head, Min)
		;	min(Tail-Back, Aux, Min)
		).

	new(List) :-
		unify_with_occurs_check(List, Back-Back).

	partition(List, _, Less, Equal, Greater) :-
		unify_with_occurs_check(List, Back-Back),
		!,
		unify_with_occurs_check(Less, LBack-LBack),
		unify_with_occurs_check(Equal, EBack-EBack),
		unify_with_occurs_check(Greater, GBack-GBack).
	partition(List-Back, Y, Less, Equal, Greater) :-
		List \== Back,
		List =[X| Xs],
		compare(Order, X, Y),
		partition(Order, X, Xs-Back, Y, Less, Equal, Greater).
	
	partition(<, X, Xs-Back, Y, [X| Less]-LBack, Equal, Greater) :-
		partition(Xs-Back, Y, Less-LBack, Equal, Greater).
	partition(=, X, Xs-Back, Y, Less, [X| Equal]-EBack, Greater) :-
		partition(Xs-Back, Y, Less, Equal-EBack, Greater).
	partition(>, X, Xs-Back, Y, Less, Equal, [X| Greater]-GBack) :-
		partition(Xs-Back, Y, Less, Equal, Greater-GBack).

	permutation(List, Permutation) :-
		same_length(List, Permutation),
		permutation2(List, Permutation).

	permutation2(List1-Back1, List2-Back2) :-
		List1 == Back1,
		List2 == Back2.
	permutation2(List1-Back1, List2-Back2) :-
		List2 \== Back2,
		List2 = [Head2| Tail2],
		select(Head2, List1-Back1, Tail1-Back1),
		permutation2(Tail1-Back1, Tail2-Back2).

	prefix(List, _) :-
		unify_with_occurs_check(List, Back-Back).
	prefix(List-Back, List2-Back2) :-
		List \== Back,
		List = [Head| Tail],
		List2 \== Back2,
		List2 = [Head| Tail2],
		prefix(Tail-Back, Tail2-Back2).

	reverse(List-Back, Reversed-Back) :-
		same_length(List-Back, Reversed-Back),
		reverse(List-Back, Back-Back, Reversed-Back).

	reverse(List-Back, Reversed, Reversed) :-
		List == Back.
	reverse(List-Back, Acc-Back, Reversed) :-
		List \== Back,
		List = [Head| Tail],
		reverse(Tail-Back, [Head| Acc]-Back, Reversed).

	same_length(List1, List2) :-
		unify_with_occurs_check(List1, Back1-Back1),
		unify_with_occurs_check(List2, Back2-Back2).
	same_length(List1-Back1, List2-Back2) :-
		List1 \== Back1,
		List1 = [_| Tail1],
		List2 \== Back2,
		List2 = [_| Tail2],
		same_length(Tail1-Back1, Tail2-Back2).

	same_length(List1, List2, Length) :-
		(	integer(Length) ->
			same_length_length(Length, List1, List2)
		;	var(List1) ->
			same_length_list(List2, List1, 0, Length)
		;	same_length_list(List1, List2, 0, Length)
		).

	same_length_length(0, List1, List2) :-
		!,
		unify_with_occurs_check(List1, Back1-Back1),
		unify_with_occurs_check(List2, Back2-Back2).
	same_length_length(Length, List1-Back1, List2-Back2) :-
		Length > 0,
		Length2 is Length - 1,
		List1 \== Back1,
		List1 = [_| Tail1],
		List2 \== Back2,
		List2 = [_| Tail2],
		same_length_length(Length2, Tail1-Back1, Tail2-Back2).

	same_length_list(List1, List2, Length, Length) :-
		unify_with_occurs_check(List1, Back1-Back1),
		unify_with_occurs_check(List2, Back2-Back2).
	same_length_list(List1-Back1, List2-Back2, Acc, Length) :-
		List1 \== Back1,
		List1 = [_| Tail1],
		List2 \== Back2,
		List2 = [_| Tail2],
		Acc2 is Acc + 1,
		same_length_list(Tail1-Back1, Tail2-Back2, Acc2, Length).

	select(Head, List-Back, Tail-Back) :-
		List \== Back,
		List = [Head| Tail].
	select(Head, List-Back, List2-Back) :-
		List \== Back,
		List = [Other| Tail],
		List2 \== Back,
		List2 = [Other| Tail2],
		select(Head, Tail-Back, Tail2-Back).

	selectchk(Elem, List-Back, Remaining-Back) :-
		select(Elem, List-Back, Rest-Back) ->
		unify_with_occurs_check(Remaining, Rest).

	sort(Difflist, Sorted) :-
		as_list(Difflist, List),
		{sort(List, List2)},
		list::as_difflist(List2, Sorted).		

	sublist(Sublist, List) :-
		unify_with_occurs_check(Sublist, List).
	sublist(Sublist-Back, List-Back):-
		List \== Back,
		List = [Head| Tail],
		sublist(Tail-Back, Head, Sublist-Back).

	sublist(List, _, Sublist) :-
		unify_with_occurs_check(List, Sublist).
	sublist(List-Back, _, Sublist-Back):-
		List \== Back,
		List = [Head| Tail],
		sublist(Tail-Back, Head, Sublist-Back).
	sublist(List-Back, Element, [Element| Sublist]-Back):-
		List \== Back,
		List = [Head| Tail],
		sublist(Tail-Back, Head, Sublist-Back).

	subsequence(List-Back, List-Back, List-Back) :-
		List == Back.
	subsequence(List-Back, Subsequence-Back, [Head| Remaining]-Back) :-
		List \== Back,
		List = [Head| Tail],
		subsequence(Tail-Back, Subsequence-Back, Remaining-Back).
	subsequence(List-Back, [Head| Subsequence]-Back, Remaining-Back) :-
		List \== Back,
		List = [Head| Tail],
		subsequence(Tail-Back, Subsequence-Back, Remaining-Back).

	subtract(List-Back, _, Result) :-
		unify_with_occurs_check(Result, Back-Back),
		List == Back, !.
	subtract(List-Back, Ys, List2-Back) :-
		List \== Back,
		List = [Head| Tail],
		(	member(Head, Ys) ->
			subtract(Tail-Back, Ys, List2-Back)
		;	List2 = [Head| Tail2],
			subtract(Tail-Back, Ys, Tail2-Back)
		).

	suffix(Suffix, List) :-
		unify_with_occurs_check(Suffix, List).
	suffix(Suffix-Back, List-Back) :-
		List \== Back,
		List = [_| Tail],
		suffix(Suffix-Back, Tail-Back).

	valid(List) :-
		nonvar(List),
		valid2(List).

	valid2(List-Back) :-
		List == Back,
		!.
	valid2(List-Back) :-
		nonvar(List),
		List = [_| Tail],
		valid2(Tail-Back).

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
