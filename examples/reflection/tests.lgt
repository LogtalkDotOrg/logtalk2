
:- object(tests,
	extends(lgtunit)).

	:- set_logtalk_flag(unknown, silent).

	:- info([
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "reflection" example.']).

	:- discontiguous(test/1).
	:- discontiguous(throws/2).

	test(reflection_1) :-
		class::instances(Instances), 
		class::metaclass,
		list::msort(Instances,InstancesSorted),
		InstancesSorted == [abstract_class,class,object].

	throws(reflection_2, error(existence_error(predicate_declaration,new(i)),ac::new(i),This)) :-
		this(This),
		abstract_class::new(ac),
		ac::abstract_class,
		ac::new(i).

	test(reflection_3) :-
		class::new(c),
		c::new(i), 
		c::instances(Instances),
		Instances == [i].

	test(reflection_4) :-
    	\+ i::current_predicate(_Predicate).

:- end_object.
