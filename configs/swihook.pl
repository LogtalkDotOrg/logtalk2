%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Logtalk - Open source object-oriented logic programming language
%  Release 2.31.3
%
%  integration code for SWI Prolog 3.3.x and later versions to compile and
%  load Logtalk files using SWI Prolog consult/1 and to support edit/1 and
%  make/0
%
%  last updated: August 24, 2006
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- dynamic(prolog_load_file/2).
:- multifile(prolog_load_file/2).

user:prolog_load_file(_:Spec, Options) :-
	(	atom(Spec) ->
		expand_file_name(Spec, [SpecExp]),
		absolute_file_name(SpecExp, [extensions([lgt]), access(read), file_errors(fail)], Path)
	;	Spec =.. [Library, File],
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
	call_cleanup(logtalk_load(Entity, Options2), working_directory(_, Old)).


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
