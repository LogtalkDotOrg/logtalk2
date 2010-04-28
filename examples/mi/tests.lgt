
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.1,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/04/26,
		comment is 'Unit tests for the "mi" example.']).

	:- uses(lgtunit, [op(700, xfx, '=~='), '=~='/2]).

	test(mi_1) :-
		space_time::rotate(1, 2, 3),
		space_time::translate(4),
		space_time::xyzt(X, Y, Z, T),
		T == 4, X == 1, Y == 2, Z == 3.

	test(mi_2) :-
		findall(Pred-Arity-Object-Functor, space_time::(current_predicate(Functor/Arity), functor(Pred, Functor, Arity), predicate_property(Pred, declared_in(Object))),Solutions),
		list::msort(Solutions,SolutionsSorted),
		SolutionsSorted = [ t(_)-1-time-t,
                         translate(_)-1-time-translate,
                         rotate(_,_,_)-3-space-rotate, 
                         xyz(_,_,_)-3-space-xyz, 
                         xyzt(_,_,_,_)-4-space_time-xyzt].

	test(mi_3) :-
		space_time(2,3,4,7)::distance(Distance),
		Distance =~= 5.385164807134504.

	test(mi_4) :-
		space_time(2,3,4,7)::time(Time),
		Time == 7.

	test(mi_5) :-
		findall(Pred-Arity-Object-Functor, space_time(2,3,4,7)::(current_predicate(Functor/Arity), functor(Pred, Functor, Arity), predicate_property(
Pred, declared_in(Object))), Solutions),
		list::msort(Solutions,SolutionsSorted),
		SolutionsSorted= [distance(_)-1-space(_,_,_)-distance, time(_)-1-time(_)-time].

:- end_object.
