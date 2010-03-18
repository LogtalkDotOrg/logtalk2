
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "prototypes" example.']).

	test(prototypes_1) :-
		findall(P, (alf::current_predicate(F/A), functor(P,F,A), alf::P), Solutions),
		Solutions == [
			chases('Lucky'), 
			favorite_food(cats), 
			motto('Are you going to finish that sandwich?'), 
			name('Gordon Shumway'), 
			planet('Melmac'), 
			stomachs(8)].

	test(prototypes_2) :-
		findall(Melmacian, rhonda::boyfriend(Melmacian), Solutions),
		Solutions == [alf].

:- end_object.
