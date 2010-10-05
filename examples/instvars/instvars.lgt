
:- object(root).

	:- private(ivar_/1).
	:- dynamic(ivar_/1).
	:- mode(ivar_(?integer), zero_or_one).

	:- public(ivar/1).
	:- mode(ivar(?integer), zero_or_one).

	:- public(set_ivar/1).
	:- mode(set_ivar(+integer), one).

	ivar_(0).					% default value for ivar_/1, stored locally in the class

	ivar(Value) :-				% retrieve ivar_/1 value from "self", i.e. from
		::ivar_(Value).			% the instance that received the ivar/1 message

	set_ivar(Value) :-
		::retractall(ivar_(_)),		% retract old ivar_/1 from "self"
		::asserta(ivar_(Value)).	% assert the new value into "self"

:- end_object.


:- object(instance1,
	instantiates(root)).

:- end_object.


:- object(instance2,
	instantiates(root)).

:- end_object.


:- object(instance3,
	instantiates(root)).

:- end_object.
