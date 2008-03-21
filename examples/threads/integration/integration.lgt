
:- protocol(integrate).

	:- info([
		version is 1.1,
		author is 'Paul Crocker',
		date is 2008/3/18,
		comment is 'Default protocol for Numerical Integration.']).

	:- public(integrate/6).
	:- mode(integrate(+object_identifier, +float, +float, +integer, +float, -float), one).
	:- info(integrate/6, [
		comment is 'Find the integral of a function of one variable in the interval [A, B] given a maximum aproximation error (epsilon).',
		argnames is ['Function', 'A', 'B', 'NP','Epsilon', 'Integral']]).

:- end_protocol.



:- object(quadrec(_Threads),
	implements(integrate),
	imports(areas)).

	:- threaded.

	:- info([
		version is 1.0,
		author is 'Paul Crocker',
		date is 2008/03/17,
		comment is 'Multi-threading implementation of Recursive Gaussian Quadrature Methods for Numerical Integration for functions of a single variable.',
		parameters is ['Threads'- 'Number of threads to use.']]).

	integrate(Function, Left, Right, NP, Epsilon, Integral) :-
		parameter(1, Threads),
		Threads > 0,
		(	NP =:= 0 ->
			Function::eval(Left,  Fleft),
			Function::eval(Right, Fright),
			:trapezium_area(Left, Right, Fleft, Fright, InitialArea),
			quadrature(Function, Threads, Left, Right, Fleft, Fright, InitialArea, Epsilon, Integral)
		;	NP > 0,
			:interval_area(Function, Left, Right, NP, NP, 0.0, InitialArea),
			quadrature(Function, Threads, Left, Right, InitialArea, NP, Epsilon, Integral)
		).

	% NP Point Quadrature 
	quadrature(Function, Threads, Left, Right, Area, NP, Epsilon, Integral) :-		
		Middle is 0.5*(Right+Left),
		:interval_area(Function, Left,   Middle, NP, NP, 0.0, Area1),
		:interval_area(Function, Middle, Right,  NP, NP, 0.0, Area2),	
		Error is abs(Area-Area1-Area2),
		(	Error > Epsilon -> 	
			(	Threads =:= 1 ->
				quadrature(Function, Threads, Left,   Middle, Area1, NP, Epsilon, I1),
				quadrature(Function, Threads, Middle, Right,  Area2, NP, Epsilon, I2)
			;	% Threads > 1,
				Threads2 is Threads//2,
				threaded((
					quadrature(Function, Threads2, Left,   Middle, Area1, NP, Epsilon, I1),
					quadrature(Function, Threads2, Middle, Right,  Area2, NP, Epsilon, I2)
				))
			),
			Integral is I1 + I2
		;	Integral is Area1 + Area2
		).

	% trapezium
	quadrature(Function, Threads, Left, Right, Fleft, Fright, Area, Epsilon, Integral) :-
		Middle is 0.5*(Right+Left),
		Function::eval(Middle, Fmiddle),
		:trapezium_area(Left,   Middle, Fleft,   Fmiddle, Area1),
		:trapezium_area(Middle, Right,  Fmiddle, Fright,  Area2),
		Error is abs(Area-Area1-Area2),
		(	Error > Epsilon -> 
			(	Threads =:= 1 ->
				quadrature(Function, Threads, Left, Middle,  Fleft, Fmiddle,  Area1, Epsilon, I1),
				quadrature(Function, Threads, Middle, Right, Fmiddle, Fright, Area2, Epsilon, I2)
			;	% Threads > 1,
				Threads2 is Threads//2,
				threaded(( 
					quadrature(Function, Threads2, Left,   Middle,  Fleft,   Fmiddle,  Area1, Epsilon, I1),
					quadrature(Function, Threads2, Middle, Right,   Fmiddle, Fright,   Area2, Epsilon, I2)
				))
			),
			Integral is I1 + I2
		;	Integral is Area1 + Area2
		).

:- end_object.



:- object(quadsplit(_Threads),
	implements(integrate),
	imports(areas)).

	:- threaded.

	:- info([
		version is 1.0,
		author is 'Paul Crocker',
		date is 2008/03/17,
		comment is 'Multi-threading implementation of Recursive Gaussian Quadrature Methods for Numerical Integration for functions of a single variable.',
		parameters is ['Threads'- 'Number of threads to use.']]).

	integrate(Function, Left, Right, NP, Epsilon, Integral) :-
		parameter(1, Threads),
		Threads > 0,
		Right > Left,
		NP >= 0, NP =< 4,
		split(Left, Right, Threads, Intervals),
		spawn(Intervals, Function, NP, Epsilon, Goals),
		collect(Goals, 0.0, Integral).

	% split an interval into a list of intervals.
	split(Inf, Sup, N, Intervals):-
		Width is (Sup - Inf) / N,
		split(1, N, Width, Sup, Inf, Intervals).

	split(N, N, _, Sup, Current, [Current-Sup]) :-
		!.
	split(I, N, Width, Sup, Current, [Current-Next| Intervals]):-
		I2 is I + 1,
		Next is Current + Width,
		split(I2, N, Width, Sup, Next, Intervals).

	% initiate the thread calls
	spawn([], _, _, _, []).
	spawn([Left-Right| Intervals], Function, NP, Epsilon, [start(Function,Left,Right,NP,Epsilon,Subarea)| Goals]) :-
		threaded_once(start(Function,Left,Right,NP,Epsilon,Subarea)),
		spawn(Intervals, Function, NP, Epsilon, Goals).

	% wait for the threads to finish and then we will collect the results summing as we go
	collect([], Integral, Integral).
	collect([start(Function,Left,Right,NP,Epsilon,Subarea)| Goals], Acc, Integral) :-
		threaded_exit(start(Function,Left,Right,NP,Epsilon,Subarea)),		
		Acc2 is Acc + Subarea,
		collect(Goals, Acc2, Integral).

	% predicate that the threads will start	
	start(Function, Left, Right, NP, Epsilon, Integral) :-
		(	NP =:= 0 -> 
			Function::eval(Left, Fleft),
			Function::eval(Right,Fright),
			:trapezium_area(Left, Right, Fleft, Fright, InitialArea),
			quadrature(Function, Left, Right, Fleft, Fright, InitialArea, Epsilon, Integral)
		;	% NP > 0,
			:interval_area(Function, Left, Right, NP, NP, 0.0, InitialArea),
			quadrature(Function, Left, Right, InitialArea, NP, Epsilon, Integral)
		).

	% NP Point Quadrature 
	quadrature(Function, Left, Right, Area, NP, Epsilon, Integral) :-
		Middle is 0.5*(Right+Left),
		:interval_area(Function, Left,   Middle, NP, NP, 0.0, Area1),
		:interval_area(Function, Middle, Right,  NP, NP, 0.0, Area2),	
		Error is abs(Area-Area1-Area2),
		(	Error > Epsilon -> 	
			quadrature(Function, Left, Middle,   Area1, NP, Epsilon, I1),
			quadrature(Function, Middle, Right,  Area2, NP, Epsilon, I2),
			Integral is I1 + I2
		;	Integral is Area1 + Area2
		).

	% trapezium
	quadrature(Function, Left, Right, Fleft, Fright, Area, Epsilon, Integral) :-
		Middle is 0.5*(Right+Left),
		Function::eval(Middle, Fmiddle),
		:trapezium_area(Left,   Middle, Fleft,   Fmiddle, Area1),
		:trapezium_area(Middle, Right,  Fmiddle, Fright,  Area2),
		Error is abs(Area-Area1-Area2),	
		(	Error > Epsilon -> 
			quadrature(Function, Left,   Middle, Fleft,   Fmiddle, Area1, Epsilon, I1),
			quadrature(Function, Middle, Right,  Fmiddle, Fright,  Area2, Epsilon, I2),
			Integral is I1 + I2
		;	Integral is Area1 + Area2
		).

:- end_object.
