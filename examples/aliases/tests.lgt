
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "aliases" example.']).

	test(aliases_1) :-
		findall(Predicate, square(_)::current_predicate(Predicate), PredicatesUnsorted),
		list::msort(PredicatesUnsorted, PredicatesSorted),
		PredicatesSorted = [area/1, height/1, side/1, width/1].

	test(aliases_2) :-
		square(2)::side(Side),
		Side == 2.

	test(aliases_3) :-
		findall(Property, square(2)::predicate_property(side(_), Property), PropertiesUnsorted),
		list::msort(PropertiesUnsorted, PropertiesSorted),
		PropertiesSorted = [public, static, alias_of(width(_)), declared_in(rectangle(_, _)), defined_in(rectangle(_, _))].

	test(aliases_4) :-
		findall(Property, square(2)::predicate_property(width(_), Property), PropertiesUnsorted),
		list::msort(PropertiesUnsorted, PropertiesSorted),
		PropertiesSorted = [public, static, declared_in(rectangle(_, _)), defined_in(rectangle(_, _))].

	test(aliases_5) :-
		findall(Predicate, circle(_)::current_predicate(Predicate), PredicatesUnsorted),
		list::msort(PredicatesUnsorted, PredicatesSorted),
		PredicatesSorted = [area/1, r/1, rx/1, ry/1].

	test(aliases_6) :-
		circle(3)::r(Radius),
		Radius == 3.

	test(aliases_7) :-
		findall(Property, circle(3)::predicate_property(r(_), Property), PropertiesUnsorted),
		list::msort(PropertiesUnsorted, PropertiesSorted),
		PropertiesSorted = [public, static, alias_of(rx(_)), declared_in(ellipse(_, _)), defined_in(ellipse(_, _))].

	test(aliases_8) :-
		findall(Property, circle(3)::predicate_property(rx(_), Property), PropertiesUnsorted),
		list::msort(PropertiesUnsorted, PropertiesSorted),
		PropertiesSorted = [public, static, declared_in(ellipse(_, _)), defined_in(ellipse(_, _))].

:- end_object.
