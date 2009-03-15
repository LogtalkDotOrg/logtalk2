
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


%  This is a sample settings file for Logtalk that can be used to override
%  the default flag values in the back-end Prolog compiler config files.
%  Using setting files allows Logtalk to easily support project-specific
%  settings. Note that the settings here can always be overriden by using
%  the logtalk_compile/2 and logtalk_load/2 built-in predicates.
%
%  To use this feature, simply copy this file to the directory containing
%  your Logtalk source files and customize it by adding clauses for the
%  logtalk_flag/2 predicate (see the examples below).
%
%  Unfortunately, limitations of the back-end Prolog compilers may prevent
%  this feature to work. Consult the "configs/NOTES.txt" for details on
%  the supported back-end Prolog compiler and operating-systems.

%  To make Logtalk completely silent for batch processing uncomment the
%  following lines:
% logtalk_flag(startup_message, none).
% logtalk_flag(report, off).

%  To make Logtalk startup and compilation less verbose uncomment the
%  following lines:
% logtalk_flag(startup_message, flags(compact)).
% logtalk_flag(report, warnings).

%  To compile all your source files in debug mode uncomment the following
%  lines:
% logtalk_flag(debug, on).
% logtalk_flag(smart_compilation, off).
% logtalk_flag(reload, always).

%  To recude clutter in the directory containing your source files uncomment
%  the following lines:
% logtalk_flag(altdirs, on).

%  To collect all XML documenting files in the same place for generating 
%  (X)HTML or PDF documentation of your project uncomment the following
%  lines:
% logtalk_flag(altdirs, on).
% logtalk_flag(xmldocs, on).
% logtalk_flag(xmldir, '/home/user/my_project_docs/').
