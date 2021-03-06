
:- object(depth_first(Bound),
	instantiates(blind_search(Bound))).

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2008/6/9,
		comment is 'Depth first state space search strategy.',
		parnames is ['Bound']]).

	:- uses(list, [member/2, reverse/2]).

	search(Space, State, Bound, Solution) :-
		depth(Space, State, Bound, [], Path),
		reverse(Path, Solution).

	depth(Space, State, _, Path, [State| Path]) :-
		Space::goal_state(State).
	depth(Space, State, Bound, Path, Solution) :-
		Bound > 0,
		Space::next_state(State, Next),
		\+ Space::member_path(Next, [State| Path]),
		Bound2 is Bound - 1,
		depth(Space, Next, Bound2, [State| Path], Solution).

:- end_object.
