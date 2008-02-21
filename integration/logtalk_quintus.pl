
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.31.5
%
%  Copyright (c) 1998-2008 Paulo Moura.  All Rights Reserved.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- 	unix(args([LOGTALKHOME, LOGTALKUSER| _])),	% hack for workaround the lack of support for environment variables in file names
	atom_chars(LOGTALKHOME, LH),
	atom_chars(LOGTALKUSER, LU),
	atom_chars('/configs/quintus.config', LC), append(LU, LC, L1), atom_chars(ConfigFile, L1), compile(ConfigFile),
	atom_chars('/compiler/logtalk.pl', LL), append(LH, LL, L2), atom_chars(CompilerFile, L2), compile(CompilerFile),
	atom_chars('/libpaths/libpaths.pl', LP), append(LU, LP, L3), atom_chars(LibpathsFile, L3), compile(LibpathsFile).
