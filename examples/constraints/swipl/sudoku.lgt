/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Sudoku CLP(FD) animation.

   Written Feb. 2008 by Markus Triska  (triska@gmx.at)
   Public domain code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- object(soduku).

	:- use_module(clpfd, [all_different/1, ins/2, labeling/2]).
	:- use_module(lists, [append/2, length/2]).

	:- uses(meta, [succeeds/2::maplist/2]).

	:- public([problem/2, show/2, sudoku/1]).

	/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	   Constraint posting
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	sudoku(Rows) :-
		length(Rows, 9), maplist(length_(9), Rows),
		append(Rows, Vs), Vs ins 1..9,
		map_all_different(Rows),
		transpose(Rows, Columns), map_all_different(Columns),
		Rows = [A,B,C,D,E,F,G,H,I],
		blocks(A, B, C), blocks(D, E, F), blocks(G, H, I).

	length_(L, Ls) :- length(Ls, L).

	transpose(Ms, Ts) :- Ms = [F|_], transpose(F, Ms, Ts).

	transpose([], _, []).
	transpose([_|Rs], Ms, [Ts|Tss]) :-
		lists_firsts_rests(Ms, Ts, Ms1),
		transpose(Rs, Ms1, Tss).

	lists_firsts_rests([], [], []).
	lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
		lists_firsts_rests(Rest, Fs, Oss).

	blocks([], [], []).
	blocks([A,B,C|Bs1], [D,E,F|Bs2], [G,H,I|Bs3]) :-
		all_different([A,B,C,D,E,F,G,H,I]),
		blocks(Bs1, Bs2, Bs3).

	/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	   Animation.
	
	   A frozen goal for each variable emits PostScript instructions to
	   draw a number. On backtracking, the field is cleared.
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	animate(Rows) :- animate(Rows, 1).

	animate([], _).
	animate([Row|Rows], N) :-
		animate(Row, 1, N),
		N1 is N + 1,
		animate(Rows, N1).

	animate([], _, _).
	animate([C|Cs], Col, Row) :-
		freeze(C, label(Col, Row, C)),
		Col1 is Col + 1,
		animate(Cs, Col1, Row).

	label(Col, Row, N) :- format("(~w) ~w ~w num\n", [N,Col,Row]).
	label(Col, Row, _) :- format("~w ~w clear\n", [Col,Row]), fail.

	/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	   PostScript definitions. Place a number N and clear a cell with:
	
	   (N) Col Row num
	   Col Row clear
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	postscript("systemdict /.setlanguagelevel known { 2 .setlanguagelevel} if \
	 /Palatino-Bold findfont 5 scalefont setfont \
	 320 9 div dup scale 0 setlinewidth -0.9 -0.9 translate \
	 /num { gsave 10 exch sub translate 0.5 0.25 translate 0.16 dup scale \
	     dup stringwidth pop -2 div 0 moveto show grestore } bind def \
	 /clear { gsave 10 exch sub translate 1 setgray 0.1 dup 0.8 dup rectfill \
	     grestore } bind def \
	 1 1 10 { gsave dup 1 moveto 10 lineto stroke grestore } for \
	 1 1 10 { gsave dup 1 exch moveto 10 exch lineto stroke grestore } for \
	 1 3 9 { 1 3 9 { 1 index gsave translate 0.05 setlinewidth
	     0 0 3 3 rectstroke grestore } for pop } for\n").

	/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	   Putting it all together: Set up communication with gs.
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	show(Options, Rows) :-
		open(pipe('gs -dNOPAUSE -g680x680 -r150 -q -'), write, Out,
			[buffer(false),alias(gs)]),
		tell(Out),
		postscript(Ps),
		format(Ps),
		sudoku(Rows),
		animate(Rows),
		append(Rows, Vs),
		labeling(Options, Vs),
		finish.
	show(_, _) :- finish, close(gs), fail.

	finish :-
		format("copypage\n"),
		% fill the buffer to make 'gs' process all generated output
		ignore((between(1,500,_),
			format("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"),
			fail)),
		flush_output.

	/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	   Sample problems.
	- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

	problem(1, P) :- % shokyuu
		P = [[1,_,_,8,_,4,_,_,_],
			 [_,2,_,_,_,_,4,5,6],
			 [_,_,3,2,_,5,_,_,_],
			 [_,_,_,4,_,_,8,_,5],
			 [7,8,9,_,5,_,_,_,_],
			 [_,_,_,_,_,6,2,_,3],
			 [8,_,1,_,_,_,7,_,_],
			 [_,_,_,1,2,3,_,8,_],
			 [2,_,5,_,_,_,_,_,9]].

	problem(2, P) :-  % shokyuu
		P = [[_,_,2,_,3,_,1,_,_],
			 [_,4,_,_,_,_,_,3,_],
			 [1,_,5,_,_,_,_,8,2],
			 [_,_,_,2,_,_,6,5,_],
			 [9,_,_,_,8,7,_,_,3],
			 [_,_,_,_,4,_,_,_,_],
			 [8,_,_,_,7,_,_,_,4],
			 [_,9,3,1,_,_,_,6,_],
			 [_,_,7,_,6,_,5,_,_]].

	problem(3, P) :-
		P = [[1,_,_,_,_,_,_,_,_],
			 [_,_,2,7,4,_,_,_,_],
			 [_,_,_,5,_,_,_,_,4],
			 [_,3,_,_,_,_,_,_,_],
			 [7,5,_,_,_,_,_,_,_],
			 [_,_,_,_,_,9,6,_,_],
			 [_,4,_,_,_,6,_,_,_],
			 [_,_,_,_,_,_,_,7,1],
			 [_,_,_,_,_,1,_,3,_]].

	map_all_different([]).
	map_all_different([Head| Tail]) :-
		all_different(Head),
		map_all_different(Tail).

	%?- show([ff], Rows).

	%?- problem(1, Rows), show([ff], Rows).

:- end_object.
