
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.43.1
%  
%  Copyright (c) 1998-2011 Paulo Moura.        All Rights Reserved.
%  Logtalk is free software.  You can redistribute it and/or modify
%  it under the terms of the "Artistic License 2.0" as published by 
%  The Perl Foundation. Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- 	unix(args([LOGTALKHOME, LOGTALKUSER| _])),	% hack for workaround the lack of support for environment variables in file names
	atom_chars(LOGTALKHOME, LH),
	atom_chars(LOGTALKUSER, LU),
	atom_chars('/configs/quintus.pl', LC), append(LH, LC, L1), atom_chars(ConfigFile, L1), compile(ConfigFile),
	atom_chars('/libpaths/libpaths.pl', LP), append(LU, LP, L3), atom_chars(LibpathsFile, L3), compile(LibpathsFile),
	atom_chars('/compiler/logtalk.pl', LL), append(LH, LL, L2), atom_chars(CompilerFile, L2), compile(CompilerFile).
