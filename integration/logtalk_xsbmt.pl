
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.42.4
%  
%  Copyright (c) 1998-2011 Paulo Moura.        All Rights Reserved.
%  Logtalk is free software.  You can redistribute it and/or modify
%  it under the terms of the "Artistic License 2.0" as published by 
%  The Perl Foundation. Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- import expand_atom/2 from standard.

:- expand_atom('$LOGTALKHOME/configs/xsb.pl', Config), reconsult(Config).
:- expand_atom('$LOGTALKHOME/integration/logtalk_comp_xsbmt.pl', Compiler), reconsult(Compiler).
:- expand_atom('$LOGTALKUSER/libpaths/libpaths.pl', Libpaths), reconsult(Libpaths).
