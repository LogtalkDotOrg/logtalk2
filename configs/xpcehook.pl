%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.32.1
%
%  Copyright (c) 1998-2008 Paulo Moura. All Rights Reserved.
%  
%  Logtalk is free software. You can redistribute it and/or modify
%  it under the terms of the Artistic License 2.0 as published by 
%  the The Perl Foundation.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%  Artistic License 2.0 for more details. A copy of the license is 
%  provided in the "LICENSE.txt" file.
%
%
%  integration code for XPCE 6.6.21 and later versions supporting Logtalk 
%  message sending goals as call-backs goals by using the syntax:
% 
%    logtalk(Object, MessageFunctor, MessageArg1, MessageArg2, ...)
%
%  as an alternative to XPCE's message(...) call-backs
%
%  last updated: October 20, 2006
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- use_module(library(pce)).


:- pce_begin_class(logtalk, message).

	initialise(Msg, Obj:prolog, Functor:prolog, Args:unchecked ...) :->
		Pred =.. [Functor| Args],
		SuperMsg =.. [initialise, @prolog, call, prolog(Obj::Pred)],
		send_super(Msg, SuperMsg).

:- pce_end_class.
