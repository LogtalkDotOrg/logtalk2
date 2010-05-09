%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.39.3
%
%  Copyright (c) 1998-2010 Paulo Moura.        All Rights Reserved.
%  Logtalk is free software.  You can redistribute it and/or modify
%  it under the terms of the "Artistic License 2.0" as published by 
%  The Perl Foundation. Consult the "LICENSE.txt" file for details.
%
%
%  integration code for SWI Prolog 5.8.0 and later versions to compile and
%  load Logtalk files using SWI Prolog consult/1 and to support edit/1 and
%  make/0
%
%  last updated: April 2, 2010
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- dynamic(prolog_load_file/2).
:- multifile(prolog_load_file/2).

user:prolog_load_file(_:Spec, Options) :-
	\+ '$lgt_member'(must_be_module(true), Options),	% exclude calls to use_module/1-2
	\+ '$lgt_member'(if(not_loaded), Options),			% exclude calls to ensure_loaded/1
	(	atom(Spec) ->
		expand_file_name(Spec, [SpecExp]),
		absolute_file_name(SpecExp, [extensions([lgt]), access(read), file_errors(fail)], Path)
	;	Spec =.. [Library, File],
		atom(File),										% no paths instead of a file name for Logtalk
		'$lgt_expand_library_path'(Library, LibPath),
		atom_concat(LibPath, File, Spec2),
		expand_file_name(Spec2, [SpecExp]),
		absolute_file_name(SpecExp, [extensions([lgt]), access(read), file_errors(fail)], Path)
	),
	file_directory_name(Path, Dir),
	file_base_name(Path, BaseName),
	file_name_extension(Entity, _, BaseName),
	working_directory(Old, Dir),
	'$lgt_filter_compiler_options'(Options, Options2),
	setup_call_cleanup(true, logtalk_load(Entity, Options2), working_directory(_, Old)).


'$lgt_filter_compiler_options'([], []).

'$lgt_filter_compiler_options'([Option| Options], [Option| Options2]) :-
	functor(Option, Functor, 1),
	'$lgt_valid_flag'(Functor),
	!,
	'$lgt_filter_compiler_options'(Options, Options2).

'$lgt_filter_compiler_options'([_| Options], Options2) :-
	'$lgt_filter_compiler_options'(Options, Options2).



:- multifile(prolog_edit:locate/3).

prolog_edit:locate(Name, source_file(Source), [file(Source)]) :-
	atom(Name),
	source_file(Path),
	file_base_name(Path, File),
	file_name_extension(Name, 'pl', File),
	file_name_extension(Plain, _, Path),
	file_name_extension(Plain, 'lgt', Source),
	exists_file(Source),
	!.
