
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.35.2
%  
%  Copyright (c) 1998-2009 Paulo Moura.        All Rights Reserved.
%  Logtalk is free software.  You can redistribute it and/or modify
%  it under the terms of the "Artistic License 2.0" as published by 
%  The Perl Foundation. Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- 	fcompile('$LOGTALKUSER/configs/qu.pl', [assemble_only(true)]),
	load('$LOGTALKUSER/configs/qu.qo'),
	(	access('settings.pl', 4, 0) ->
		fcompile('settings.pl', [assemble_only(true)]),
		load('settings.qo')
	;	true
	),
	os(system('ln -sf $LOGTALKHOME/compiler/logtalk.pl $LOGTALKUSER/.logtalk.pl')),
	fcompile('$LOGTALKUSER/.logtalk.pl', [assemble_only(true), object_file('$LOGTALKUSER/.logtalk.qo'), string_table(256)]),
	load('$LOGTALKUSER/.logtalk.qo'),
	fcompile('$LOGTALKUSER/libpaths/libpaths.pl', [assemble_only(true)]),
	load('$LOGTALKUSER/libpaths/libpaths.qo').
