
:- object(bridge,
	instantiates(heuristic_state_space)).


	initial_state(start, ([], right, [1,3,6,8,12])).


	goal_state(end, ([1,3,6,8,12], left, [])).


	next_state((Left1, left, Right1), (Left2, right, Right2), Slower) :-	% two person
		list::sublist([Person1, Person2], Left1),
		list::subtract(Left1, [Person1, Person2], Left2),
		set::insert_all([Person1, Person2], Right1, Right2),
		(Person1 > Person2 ->
			Slower = Person1
			;
			Slower = Person2).

	next_state((Left1, right, Right1), (Left2, left, Right2), Slower) :-	% two person
		list::sublist([Person1, Person2], Right1),
		list::subtract(Right1, [Person1, Person2], Right2),
		set::insert_all([Person1, Person2], Left1, Left2),
		(Person1 > Person2 ->
			Slower = Person1
			;
			Slower = Person2).

	next_state((Left1, left, Right1), (Left2, right, Right2), Person) :-	% one person
		set::select(Person, Left1, Left2),
		set::insert(Right1, Person, Right2).
		
	next_state((Left1, right, Right1), (Left2, left, Right2), Person) :-	% one person
		set::select(Person, Right1, Right2),
		set::insert(Left1, Person, Left2).


	heuristic((_, _, Right), Heuristic) :-
		numberlist::sum(Right, Heuristic).


	print_state((Left, Lamp, Right)) :-
		write_list(Left),
		(Lamp = left ->
			write(' lamp _|____________|_ ')
			;
			write(' _|____________|_ lamp ')),
		write_list(Right),
		nl.


	write_list([]).
	
	write_list([Head| Tail]) :-
		write(Head), write(' '),
		write_list(Tail).


:- end_object.
