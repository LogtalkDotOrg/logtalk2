
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.39.1
%  
%  Copyright (c) 1998-2010 Paulo Moura.        All Rights Reserved.
%  Logtalk is free software.  You can redistribute it and/or modify
%  it under the terms of the "Artistic License 2.0" as published by 
%  The Perl Foundation. Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%  This is a sample settings file for Logtalk that can be used to override
%  the default flag values in the back-end Prolog compiler config files.
%  Using settings files allows Logtalk to easily support per-project
%  settings. Note that the settings defined here can always be overridden
%  by using the logtalk_compile/2 and logtalk_load/2 built-in predicates
%  or by using the set_logtalk_flag/2 directive within the source files.
%
%  To use this feature, copy this file to the directory containing your
%  project files, customize it (see the examples below), and start Logtalk
%  from the project directory. Note that, for setting Logtalk flag values,
%  you must use the set_logtalk_flag/2 predicate (wrapped in a directive
%  initialization/1) as the scope of the set_logtalk_flag/2 directive is
%  always local to an entity or to a source file.
%
%  If you use more than one back-end Prolog compiler and want to use
%  different settings per compiler you will need to use the Logtalk 
%  conditional compilation directives and the "prolog_dialect" compiler
%  flag. See the User and Reference Manuals for details.
%
%  Logtalk compiles and loads settings files silently, ignoring any errors.
%  Be sure to debug and test your settings files as normal Logtalk source
%  files before using them (you may use the logtalk_compile/1-2 built-in
%  predicates to compile the settings files without loading them).
%
%  Logtalk looks for a settings file first in the startup directory, If not
%  found, Logtalk looks for a settings file in the Logtalk user directory.
%  If no settings file is found, Logtalk will use the default flag values
%  defined in the back-end Prolog compiler config file.
%
%  Limitations of the back-end Prolog compilers may prevent settings files
%  to work from directories other than the Logtalk user directory, specially
%  when running on non-POSIX operating systems such as Windows. Check the 
%  "configs/NOTES.txt" file for compatibility details.


%  To define a "library" path for your projects, edit and uncomment the
%  following lines (the library path must end with a slash character):

/*
:- multifile(logtalk_library_path/2).
:- dynamic(logtalk_library_path/2).

logtalk_library_path(my_project, '$HOME/my_project/').
logtalk_library_path(my_project_examples, my_project('examples/')).
*/


%  To make Logtalk completely silent for batch processing uncomment the
%  following lines:

/*
:- initialization((
	set_logtalk_flag(startup_message, none),
	set_logtalk_flag(report, off)
)).
*/


%  To make Logtalk startup and compilation less verbose uncomment the
%  following lines:

/*
:- initialization((
	set_logtalk_flag(startup_message, banner),
	set_logtalk_flag(report, warnings)
)).
*/


%  To compile all your source files for debugging uncomment the following
%  lines:

/*
:- initialization((
	set_logtalk_flag(debug, on),
	set_logtalk_flag(smart_compilation, off),
	set_logtalk_flag(reload, always),
	set_logtalk_flag(unknown, warning),
	set_logtalk_flag(misspelt, warning),
	set_logtalk_flag(singletons, warning),
	set_logtalk_flag(context_switching_calls, allow)
)).
*/


%  To reduce clutter in the directory containing your source files uncomment
%  the following lines:

/*
:- initialization((
	set_logtalk_flag(altdirs, on),
	set_logtalk_flag(clean, on)
)).
*/


%  To avoid recompilation of stable source files uncomment the following lines:

/*
:- initialization((
	set_logtalk_flag(smart_compilation, on),
	set_logtalk_flag(clean, off)
)).
*/


%  To collect all XML documenting files in the same place for generating 
%  (X)HTML or PDF documentation of your project edit and uncomment the
%  following lines:

/*
:- initialization((
	set_logtalk_flag(altdirs, on),
	set_logtalk_flag(xmldocs, on),
	set_logtalk_flag(xmldir, '$HOME/my_project_docs/')
)).
*/


%  To develop portable Logtalk applications uncomment the following lines:

/*
:- initialization((
	set_logtalk_flag(portability, warning),
	set_logtalk_flag(plredef, warning)
)).
*/


%  To maximize performance by turning off relevant optional features
%  uncomment the following lines:

/*
:- initialization((
	set_logtalk_flag(events, deny),
	set_logtalk_flag(complements, deny),
	set_logtalk_flag(dynamic_declarations, deny)
)).
*/


%  To prevent using the <</2 context-switching control construct to bypass
%  object encapsulation rules uncomment the following lines:

/*
:- initialization((
	set_logtalk_flag(context_switching_calls, deny)
)).
*/
