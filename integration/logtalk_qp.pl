
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.41.1
%  
%  Copyright (c) 1998-2010 Paulo Moura.        All Rights Reserved.
%  Logtalk is free software.  You can redistribute it and/or modify
%  it under the terms of the "Artistic License 2.0" as published by 
%  The Perl Foundation. Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:-	(	stat('$LOGTALKHOME/configs/qp.pl', stat(TimeConfigSource, _)),
		stat('$LOGTALKUSER/.qp.qo', stat(TimeConfigObject, _)) ->
		(	TimeConfigObject < TimeConfigSource ->
			fcompile('$LOGTALKUSER/.qp.pl', [assemble_only(true), object_file('$LOGTALKUSER/.qp.qo')])
		;	true
		)
	;	os(system('ln -sf $LOGTALKHOME/configs/qp.pl $LOGTALKUSER/.qp.pl')),
		fcompile('$LOGTALKUSER/.qp.pl', [assemble_only(true), object_file('$LOGTALKUSER/.qp.qo')])
	),
	load('$LOGTALKUSER/.qp.qo'),
	(	stat('$LOGTALKHOME/compiler/logtalk.pl', stat(TimeCompilerSource, _)),
		stat('$LOGTALKUSER/.logtalk.qo', stat(TimeCompilerObject, _)) ->
		(	TimeCompilerObject < TimeCompilerSource ->
			fcompile('$LOGTALKUSER/.logtalk.pl', [assemble_only(true), object_file('$LOGTALKUSER/.logtalk.qo'), string_table(256)])
		;	true
		)
	;	os(system('ln -sf $LOGTALKHOME/compiler/logtalk.pl $LOGTALKUSER/.logtalk.pl')),
		fcompile('$LOGTALKUSER/.logtalk.pl', [assemble_only(true), object_file('$LOGTALKUSER/.logtalk.qo'), string_table(256)])
	),
	load('$LOGTALKUSER/.logtalk.qo'),

	(	stat('$LOGTALKHOME/libpaths/libpaths.pl', stat(TimePathSource, _)),
		stat('$LOGTALKUSER/libpaths/libpaths.qo', stat(TimePathsObject, _)) ->
		(	TimePathsObject < TimePathSource ->
			fcompile('$LOGTALKUSER/libpaths/libpaths.pl', [assemble_only(true)])
		;	true
		)
	;	fcompile('$LOGTALKUSER/libpaths/libpaths.pl', [assemble_only(true)])
	),
	load('$LOGTALKUSER/libpaths/libpaths.qo').
