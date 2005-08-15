
:- object(joePerson).

	:- public(getOlder/0).

	:- public(address/1).

	:- public(age/1).
	:- dynamic(age/1).

	:- public(name/1).

	:- public(phone/1).

	:- public(score/1).
	:- dynamic(score/1).

	:- public(setScore/1).

	getOlder :-
		retract(age(Old)),
		New is Old + 1,
		asserta(age(New)).

	address('8 Octave Street').

	age(30).

	name('John').

	phone(11-11-11-11).

	score(0).

	setScore(Score) :-
		::retractall(score(_)),
		::asserta(score(Score)).

:- end_object.


:- object(joeEmployee,
	extends(joePerson)).

	:- public(worksFor/1).

	:- public(salary/1).
	:- dynamic(salary/1).
	
	:- public(giveRaise/1).

	worksFor('ToonTown').

	salary(1500).

	giveRaise(Raise) :-
		retract(salary(Old)),
		New is Old + Raise,
		asserta(salary(New)).

:- end_object.


:- object(joeChessPlayer,
    extends(joePerson)).

    :- public(category/1).

    category('National Master').

:- end_object.


:- object(joeFilmEnthusiast,
	extends(joePerson)).

	:- public(favActor/1).
	:- public(favFilm/1).
	:- public(favDirector/1).

	favActor('Fred Filistone').

	favFilm('The Wizard of Oz').

	favDirector('Krzystof Kieslowski').

:- end_object.


:- object(joeSportsman,
	extends(joePerson)).

    :- public(sport/1).
	:- public(stamina/1).
	:- public(weight/1).

    sport(snowboard).

	stamina(30).

	weight(111).

:- end_object.
