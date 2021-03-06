
:- object(queue,
	implements(queuep),
	extends(compound)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Queue predicates implemented using difference lists.']).

	as_list(Queue-Back, List) :-
		(	Queue == Back ->
			List = []
		;	List = [Head| Tail],
			Queue = [Head| Rest],
			as_list(Rest-Back, Tail)
		).

	empty(Front-Back) :-
		Front == Back.

	head(Front-Back, Head) :-
		Front \== Back,
		Front = [Head| _].

	join(Element, Front-[Element| Back], Front-Back).

	join_all([], Queue, Queue).
	join_all([Head| Tail], Queue1, Queue3) :-
		join(Head, Queue1, Queue2),
		join_all(Tail, Queue2, Queue3).

	jump(Element, Front-Back, [Element| Front]-Back).

	jump_all([], Queue, Queue).
	jump_all([Head| Tail], Queue1, Queue3) :-
		jump(Head, Queue1, Queue2),
		jump_all(Tail, Queue2, Queue3).

	length(Front-Back, Length) :-
		length(Front, Back, 0, N),
		Length = N.

	length(Front, Back, N, N) :-
		Front == Back, !.
	length([_|Front], Back, K, N) :-
		L is K+1,
		length(Front, Back, L, N).

	new(Back-Back).

	serve(OldFront-Back, Head, NewFront-Back) :-
		OldFront \== Back,
		OldFront = [Head| NewFront].

	valid(Queue) :-
		nonvar(Queue),
		valid2(Queue).

	valid2(Queue-Back) :-
		Queue == Back,
		!.
	valid2(Queue-Back) :-
		nonvar(Queue),
		Queue = [_| Tail],
		valid2(Tail-Back).

:- end_object.
