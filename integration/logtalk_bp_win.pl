
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.35.1
%  
%  Copyright (c) 1998-2009 Paulo Moura.        All Rights Reserved.
%  Logtalk is free software.  You can redistribute it and/or modify
%  it under the terms of the "Artistic License 2.0" as published by 
%  The Perl Foundation. Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- set_prolog_flag(redefined, off).
:- cl('$LOGTALKUSER/configs/b.pl').
%:- system('del %LOGTALKUSER%\\configs\\b.pl.out').
:- (expand_environment('$LOGTALKHOME/compiler/logtalk.pl.out', Expanded), exists(Expanded) -> load('$LOGTALKHOME/compiler/logtalk.pl'); cl('$LOGTALKHOME/compiler/logtalk.pl')).
:- cl('$LOGTALKUSER/libpaths/libpaths.pl').
%:- system('del %LOGTALKUSER%\\libpaths\\libpaths.pl.out').
