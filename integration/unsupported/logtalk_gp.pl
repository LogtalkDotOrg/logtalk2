
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.40.0
%  
%  Copyright (c) 1998-2010 Paulo Moura.        All Rights Reserved.
%  Logtalk is free software.  You can redistribute it and/or modify
%  it under the terms of the "Artistic License 2.0" as published by 
%  The Perl Foundation. Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- initialization((
	set_prolog_flag(suspicious_warning, off),
	set_prolog_flag(multifile_warning, off),
	consult('$LOGTALKHOME/configs/gnu.pl'),
	consult('$LOGTALKHOME/integration/logtalk_comp_gp.pl'),
	consult('$LOGTALKUSER/libpaths/libpaths_no_multifile.pl'),
	set_prolog_flag(suspicious_warning, on),
	set_prolog_flag(multifile_warning, on)
)).
