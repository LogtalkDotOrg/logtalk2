
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.41.2
%  
%  Copyright (c) 1998-2010 Paulo Moura.        All Rights Reserved.
%  Logtalk is free software.  You can redistribute it and/or modify
%  it under the terms of the "Artistic License 2.0" as published by 
%  The Perl Foundation. Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- op(600, xfy, ::).	% message-sending operator


:- multifile(user:prolog_predicate_name/2).

user:prolog_predicate_name(Goal, Label) :-
	(	Goal = Module:THead ->
		Module == user
	;	Goal = THead
	),
	functor(THead, TFunctor, TArity),
	'$lgt_reverse_predicate_indicator'(TFunctor/TArity, Entity, _, Functor/Arity),
	(	atom(Entity) ->
		atomic_list_concat([Entity, '::', Functor, '/', Arity], Label)
	;	functor(Entity, EFunctor, EArity),
		atomic_list_concat([EFunctor, '/', EArity, '::', Functor, '/', Arity], Label)
	).
