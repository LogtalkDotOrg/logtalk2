%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.30.4
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
