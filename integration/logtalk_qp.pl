
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.32.2
%
%  Copyright (c) 1998-2008 Paulo Moura. All Rights Reserved.
%  
%  Logtalk is free software. You can redistribute it and/or modify
%  it under the terms of the Artistic License 2.0 as published by 
%  the The Perl Foundation.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%  Artistic License 2.0 for more details. A copy of the license is 
%  provided in the "LICENSE.txt" file.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- 	os(system('ln -sf $LOGTALKUSER/configs/qu.config $LOGTALKUSER/configs/qu.pl')),
	fcompile('$LOGTALKUSER/configs/qu.pl', [assemble_only(true)]),
	load('$LOGTALKUSER/configs/qu.qo'),
	os(system('ln -sf $LOGTALKHOME/compiler/logtalk.pl $LOGTALKUSER/.logtalk.pl')),
	fcompile('$LOGTALKUSER/.logtalk.pl', [assemble_only(true), object_file('$LOGTALKUSER/.logtalk.qo'), string_table(256)]),
	load('$LOGTALKUSER/.logtalk.qo'),
	fcompile('$LOGTALKUSER/libpaths/libpaths.pl', [assemble_only(true)]),
	load('$LOGTALKUSER/libpaths/libpaths.qo').
