
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.33.2
%  
%  Copyright (c) 1998-2008 Paulo Moura.        All Rights Reserved.
%  Logtalk is free software.  You can redistribute it and/or modify
%  it under the terms of the "Artistic License 2.0" as published by 
%  The Perl Foundation. Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- set_prolog_flag(redefined, off).
:- cl('$LOGTALKUSER/configs/b.config').
%:- system('del %LOGTALKUSER%\\configs\\b.config.out').
:- (expand_environment('$LOGTALKHOME/compiler/logtalk.pl.out', Expanded), exists(Expanded) -> load('$LOGTALKHOME/compiler/logtalk.pl'); cl('$LOGTALKHOME/compiler/logtalk.pl')).
:- cl('$LOGTALKUSER/libpaths/libpaths.pl').
%:- system('del %LOGTALKUSER%\\libpaths\\libpaths.pl.out').
