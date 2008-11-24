%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.34.0
%
%  Copyright (c) 1998-2008 Paulo Moura.        All Rights Reserved.
%  Logtalk is free software.  You can redistribute it and/or modify
%  it under the terms of the "Artistic License 2.0" as published by 
%  The Perl Foundation. Consult the "LICENSE.txt" file for details.
%
%
%  configuration file for MasterProLog 4.1
%
%  last updated: November 7, 2008
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- option('c+').



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  ISO Prolog Standard predicates that we must define because they are
%  not built-in
%
%  add a clause for lgt_iso_predicate/1 declaring each ISO predicate that
%  we must define; there must be at least one clause for this predicate
%  whose call should fail if we don't define any ISO predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


initialization(Goal) :-
	call(Goal).


% '$lgt_iso_predicate'(?callable).

'$lgt_iso_predicate'(atom_codes(_, _)).
'$lgt_iso_predicate'(current_predicate(_)).
'$lgt_iso_predicate'(number_codes(_, _)).
'$lgt_iso_predicate'(once(_)).
'$lgt_iso_predicate'(open(_,_,_)).
'$lgt_iso_predicate'(open(_, _, _, _)).
'$lgt_iso_predicate'(read_term(_, _, _)).
'$lgt_iso_predicate'(write_canonical(_, _)).
'$lgt_iso_predicate'(write_term(_, _, _)).


atom_codes(Atom, Codes) :-
	asciilist(Atom, Codes).


current_predicate(Functor/Arity) :-
	current_predicate(Functor, Term),
	functor(Term, Functor, Arity).


number_codes(Number, Codes) :-
	name(Number, Codes).


once(Goal) :-
	call(Goal),
	!.


open(File, read, Stream) :-
	fopen(Stream, File, r).

open(File, write, Stream) :-
	fopen(Stream, File, w).


open(File, Mode, Stream, []) :-
	open(File, read, Stream).


read_term(Stream, Term, [singletons([])]) :-
	!,
	read(Stream, Term).

read_term(Stream, Term, _) :-
	read(Stream, Term).


write_canonical(Stream, Term) :-
	writeq(Stream, Term).


write_term(Stream, Term, [quoted(true)]) :-
	!,
	writeq(Stream, Term).

write_term(Stream, Term, _) :-
	write(Stream, Term).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  predicate properties
%
%  this predicate must return at least static, dynamic, and built_in 
%  properties for an existing predicate
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_predicate_property'(+callable, ?predicate_property)

'$lgt_predicate_property'(Term, Property) :-
	predicate_type(Term, Property2),
	(Property2 = builtin -> Property = built_in; Property = Property2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  meta-predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% call_cleanup(+callable, +callble)

call_cleanup(_, _) :-
	throw(not_supported(call_cleanup/2)).


% forall(+callable, +callble) -- built-in

forall(Generate, Test) :-
	\+ (Generate, \+ Test).


% retractall(+callable) -- built-in


% call_with_args/2-9
%
% use these definitions only if your compiler does
% not provide call_with_args/2-9 as built-in predicates

call_with_args(F, A) :-
	call(F, A).

call_with_args(F, A1, A2) :-
	call(F, A1, A2).

call_with_args(F, A1, A2, A3) :-
	call(F, A1, A2, A3).

call_with_args(F, A1, A2, A3, A4) :-
	call(F, A1, A2, A3, A4).

call_with_args(F, A1, A2, A3, A4, A5) :-
	call(F, A1, A2, A3, A4, A5).

call_with_args(F, A1, A2, A3, A4, A5, A6) :-
	call(F, A1, A2, A3, A4, A5, A6).

call_with_args(F, A1, A2, A3, A4, A5, A6, A7) :-
	call(F, A1, A2, A3, A4, A5, A6, A7).

call_with_args(F, A1, A2, A3, A4, A5, A6, A7, A8) :-
	call(F, A1, A2, A3, A4, A5, A6, A7, A8).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Prolog built-in meta-predicates
%
%  (excluding ISO Prolog Standard meta-predicates)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_pl_meta_predicate'(?callable, ?atom).

'$lgt_pl_meta_predicate'(_, _) :-
	fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  file extension predicates
%
%  these extensions are used by Logtalk load/compile predicates
%
%  you may want to change the extension for Prolog files to match 
%  the one expected by your Prolog compiler
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_file_extension'(?atom, ?atom)

'$lgt_file_extension'(logtalk, '.lgt').
'$lgt_file_extension'(prolog, '.pl').
'$lgt_file_extension'(xml, '.xml').



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  default flag values
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_default_flag'(?atom, ?atom)
%
% default values for all flags

'$lgt_default_flag'(prolog, master).

'$lgt_default_flag'(xmldocs, on).
'$lgt_default_flag'(xslfile, 'lgtxml.xsl').
'$lgt_default_flag'(xmlspec, dtd).
'$lgt_default_flag'(xmlsref, local).

'$lgt_default_flag'(unknown, warning).
'$lgt_default_flag'(misspelt, warning).
'$lgt_default_flag'(singletons, warning).
'$lgt_default_flag'(lgtredef, warning).
'$lgt_default_flag'(plredef, silent).
'$lgt_default_flag'(portability, silent).

'$lgt_default_flag'(report, on).

'$lgt_default_flag'(smart_compilation, off).
'$lgt_default_flag'(reload, always).

'$lgt_default_flag'(startup_message, flags(verbose)).

'$lgt_default_flag'(underscore_variables, singletons).

'$lgt_default_flag'(code_prefix, '').

'$lgt_default_flag'(debug, off).
'$lgt_default_flag'(break_predicate, unsupported).

'$lgt_default_flag'(complements, off).
'$lgt_default_flag'(dynamic_declarations, off).
'$lgt_default_flag'(events, off).

'$lgt_default_flag'(altdirs, off).
'$lgt_default_flag'(tmpdir, 'lgt_tmp/').
'$lgt_default_flag'(xmldir, 'xml_docs/').

'$lgt_default_flag'(encoding_directive, unsupported).
'$lgt_default_flag'(multifile_directive, unsupported).
'$lgt_default_flag'(threads, unsupported).

'$lgt_default_flag'(context_switching_calls, allow).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  list predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


'$lgt_append'([], List, List).
'$lgt_append'([Head| Tail], List, [Head| Tail2]) :-
	'$lgt_append'(Tail, List, Tail2).


'$lgt_member'(Head, [Head| _]).
'$lgt_member'(Head, [_| Tail]) :-
	'$lgt_member'(Head, Tail).


'$lgt_member_var'(V, [H| _]) :-
	V == H.
'$lgt_member_var'(V, [_| T]) :-
	'$lgt_member_var'(V, T).


'$lgt_is_list'([]) :-
    !.
'$lgt_is_list'([_| Tail]) :-
    '$lgt_is_list'(Tail).


'$lgt_is_proper_list'(List) :-
    List == [], !.
'$lgt_is_proper_list'([_| Tail]) :-
    nonvar(Tail),
    '$lgt_is_proper_list'(Tail).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  file predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_file_exists'(+atom)
%
% checks if a file exist in the current directory

'$lgt_file_exists'(File) :-
	exists(File).


% '$lgt_directory_exists'(+atom)
%
% checks if a directory exists

'$lgt_directory_exists'(Directory) :-
	exists(Directory),
	file_info(Directory, type, dir).


% '$lgt_current_directory'(-atom)
%
% gets current working directory

'$lgt_current_directory'(Directory) :-
	getenv('PWD', Directory).


% '$lgt_change_directory'(+atom)
%
% changes current working directory

'$lgt_change_directory'(Directory) :-
	putenv('PWD', Directory).


% '$lgt_make_directory'(+atom)
%
% makes a new directory; succeeds if the directory already exists

'$lgt_make_directory'(_) :-
	throw(not_supported('$lgt_make_directory'/1)).


% '$lgt_load_prolog_code'(+atom, +atom, +list)
%
% compile and load a Prolog file, resulting from a
% Logtalk source file, given a list of options

'$lgt_load_prolog_code'(File, _, _) :-
	reconsult(File).


% '$lgt_compare_file_mtimes'(?atom, +atom, +atom)
%
% compare file modification times

'$lgt_compare_file_mtimes'(Result, File1, File2) :-
	file_info(File1, mtime, Time1),
	file_info(File2, mtime, Time2),
	compare(Result, Time1, Time2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  sorting predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_keysort'(+list, -list)

'$lgt_keysort'(List, Sorted) :-
	keysort(List, Sorted).


% '$lgt_sort'(+list, -list)

'$lgt_sort'(List, Sorted) :-
	sort(List, Sorted).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  time and date predicates
%
%  if your Prolog compiler does not provide access to the operating system 
%  time and date just write dummy definitions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_current_date'(?Year, ?Month, ?Day)

'$lgt_current_date'(Year, Month, Day) :-
	time_info([year,month,day],[Year,Month,Day]).


% '$lgt_current_time'(?Hours, ?Mins, ?Secs)

'$lgt_current_time'(Hours, Mins, Secs) :-
	time_info([hour,min,sec],[Hours,Mins,Secs]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  timing predicate
%
%  if your Prolog compiler does not provide access to a timing predicate 
%  just write dummy definition
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_cpu_time'(-Seconds)

'$lgt_cpu_time'(Seconds) :-
	cputime(Seconds).


master_init :-
	please(q, on), please(c, on), please(s, on), please(rf, off),
	compiler(c, on), compiler(w, off),
	op(600, xfy, '::'), op(600, fy, '::'), op(600, fx, '^^'),
	op(200, fy, '+'), op(200, fy, '?'), op(200, fy, '@'), op(200, fy, '-'),
	error_message(115 /* Possible erroneous use of nil */ , off).
        
?- master_init.   



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  comparison predicate
%
%  the usual compare/3 definition
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% compare(?atom, @term, @term) -- built-in



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  callable predicate
%
%  the usual callable/1 definition
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% callable(@term)

callable(Term) :-
	nonvar(Term),
	functor(Term, Functor, _),
	atom(Functor).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  read character predicate
%
%  read a single character echoing it and writing a newline after
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


'$lgt_read_single_char'(Char) :-
	get(Code), name(Char, [Code]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  pretty print a term by naming its free variables
%  (avoid instantiating variables in term by using double negation if necessary)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


'$lgt_pretty_print_vars'(Stream, Term) :-
    write(Stream, Term).


'$lgt_pretty_print_vars_quoted'(Stream, Term) :-
    writeq(Stream, Term).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  getting stream current line number
%  (needed for improved compiler error messages)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_stream_current_line_number'(@stream, -integer)

'$lgt_stream_current_line_number'(_, _) :-
	fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  customized version of the read_term/3 predicate for returning the line
%  where the term starts (needed for improved compiler error messages)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_read_term'(@stream, -term, +list, -integer)

'$lgt_read_term'(Stream, Term, Options, -1) :-
	read_term(Stream, Term, Options).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  handling of Prolog-proprietary directives on Logtalk source files
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% '$lgt_ignore_pl_directive'(@callable)

'$lgt_ignore_pl_directive'(_) :-
	fail.


% '$lgt_rewrite_and_copy_pl_directive'(@callable, -callable)

'$lgt_rewrite_and_copy_pl_directive'(_, _) :-
	fail.


% '$lgt_rewrite_and_recompile_pl_directive'(@callable, -callable)

'$lgt_rewrite_and_recompile_pl_directive'(_, _) :-
	fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Shortcut to the Logtalk built-in predicate logtalk_load/1
%
%  defined in the config files in order to be able to comment it out in case
%  of conflict with some Prolog native feature; it implies conformance with
%  the ISO Prolog standard regarding the definition of the {}/1 syntax
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

{File, Files} :-
	!,
	logtalk_load(File),
	{Files}.
{File} :-
	logtalk_load(File).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  end!
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%