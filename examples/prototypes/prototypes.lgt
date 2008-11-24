
% Alf believes he is the only survivor of its species; no point in
% defining a class if there is only going to be a single instance:

:- object(alf).

	:- public(
		[name/1, planet/1, stomachs/1, favorite_food/1, chases/1, motto/1]).

	name('Gordon Shumway').
	planet('Melmac').
	stomachs(8).
	favorite_food(cats).
	chases('Lucky').
	motto('Are you going to finish that sandwich?').

:- end_object.

% later on, Alf finds out that his best friend, Skip, and his
% girlfriend, Rhonda, also survived Melmac's explosion;
% as they are all Melmacians, they share most attributes:

:- object(skip,
	extends(alf)).

	name('Skip').
	chases(_) :-	% still longing for a nice cat
		fail.		% to eat since Melmac exploded

:- end_object.


:- object(rhonda,
	extends(alf)). 

	name('Rhonda').
	chases(_) :-	% still longing for a nice cat
		fail.		% to eat since Melmac exploded

:- end_object.
