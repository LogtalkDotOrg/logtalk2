
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.31.3
%
%  Copyright (c) 1998-2008 Paulo Moura.  All Rights Reserved.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- unix(argv([_, LOGTALKUSER| _])), unix(cd(LOGTALKUSER)), compile('configs/quintus.config').
:- unix(argv([LOGTALKHOME| _])), unix(cd(LOGTALKHOME)), compile('compiler/logtalk.pl').
:- unix(argv([_, LOGTALKUSER| _])), unix(cd(LOGTALKUSER)), compile('libpaths/libpaths.pl').
