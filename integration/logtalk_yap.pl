
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.31.4
%
%  Copyright (c) 1998-2008 Paulo Moura.  All Rights Reserved.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- initialization((
	reconsult('$LOGTALKUSER/configs/yap.config'),
	reconsult('$LOGTALKHOME/compiler/logtalk.pl'),
	reconsult('$LOGTALKUSER/libpaths/libpaths.pl'))).
