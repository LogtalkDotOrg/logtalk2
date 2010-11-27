
:- object(diagram).

	:- info([
		version is 0.1,
		author is 'Paulo Moura',
		date is 2010/11/18,
		comment is 'Generates entity diagram DOT files for source files and libraries.']).

	:- public(rlibrary/2).
	:- mode(rlibrary(+atom, +list), one).
	:- info(rlibrary/2, [
		comment is 'Creates a diagram for all entities in a library (recursive) using the specified options.',
		argnames is ['Library', 'Options']]).

	:- public(rlibrary/1).
	:- mode(rlibrary(+atom), one).
	:- info(rlibrary/1, [
		comment is 'Creates a diagram for all entities in a library (recursive) using default options.',
		argnames is ['Library']]).

	rlibrary(Library, UserOptions) :-
		merge_options(UserOptions, Options),
		logtalk::expand_library_path(Library, TopPath),
		atom_concat(Library, '.dot', DotFile),
		member(output_path(Directory), Options),
		os::working_directory(Current),
		os::change_directory(Directory),
		open(DotFile, write, Stream, [alias(dot_file)]),
		dot_header(Options),
		output_rlibrary(TopPath, Options),
		dot_footer(Options),
		close(Stream),
		os::change_directory(Current).

	rlibrary(Library) :-
		rlibrary(Library, []).

	output_rlibrary(TopPath, Options) :-
		write(dot_file, 'subgraph "cluster_'),
		write(dot_file, TopPath),
		write(dot_file, '" {\n'),
		write(dot_file, 'bgcolor=snow3\nlabel="'),
		write(dot_file, TopPath),
		write(dot_file, '"'),
		nl(dot_file),
		member(exclude_paths(ExcludePaths), Options),
		forall(
			sub_library(TopPath, ExcludePaths, RelativePath, Path),
			output_library(RelativePath, Path, Options)),
		write(dot_file, '}\n'),
		nl(dot_file).

	sub_library(TopPath, ExcludePaths, RelativePath, Path) :-
		logtalk_library_path(Library, _),
		logtalk::expand_library_path(Library, Path),
		atom_concat(TopPath, RelativePath, Path),
		\+ member(RelativePath, ExcludePaths).

	:- public(library/2).
	:- mode(library(+atom, +list), one).
	:- info(library/2, [
		comment is 'Creates a diagram for all entities in a library using default options.',
		argnames is ['Library', 'Options']]).

	:- public(library/1).
	:- mode(library(+atom), one).
	:- info(library/1, [
		comment is 'Creates a diagram for all entities in a library using default options.',
		argnames is ['Library']]).

	library(Library, UserOptions) :-
		merge_options(UserOptions, Options),
		logtalk::expand_library_path(Library, Path),
		atom_concat(Library, '.dot', DotFile),
		member(output_path(Directory), Options),
		os::working_directory(Current),
		os::change_directory(Directory),
		open(DotFile, write, Stream, [alias(dot_file)]),
		dot_header(Options),
		output_library(Path, Path, Options),
		dot_footer(Options),
		close(Stream),
		os::change_directory(Current).

	library(Library) :-
		library(Library, []).

	output_library(RelativePath, Path, Options) :-
		(	member(library_paths(true), Options) ->
			write(dot_file, 'subgraph "cluster_'),
			write(dot_file, RelativePath),
			write(dot_file, '" {\n'),
			write(dot_file, 'bgcolor=snow2\nlabel="'),
			write(dot_file, RelativePath),
			write(dot_file, '"'),
			nl(dot_file),
			output_library_files(Path, Options),
			write(dot_file, '}\n'),
			nl(dot_file)
		;	output_library_files(Path, Options)
		).

	output_library_files(Path, Options) :-
		member(exclude_files(ExcludeFiles), Options),
		logtalk::loaded_file(File, Path),
		\+ member(File, ExcludeFiles),
		output_file(File, Path, Options),
		fail.
	output_library_files(_, _).

	:- public(file/2).
	:- mode(file(+atom, +list), one).
	:- info(file/2, [
		comment is 'Creates a diagram for all entities in a loaded source file using the specified options. Supports library notation.',
		argnames is ['File', 'Options']]).

	:- public(file/1).
	:- mode(file(+atom), one).
	:- info(file/1, [
		comment is 'Creates a diagram for all entities in a loaded source file using default options. Supports library notation.',
		argnames is ['File']]).

	file(Spec, UserOptions) :-
		merge_options(UserOptions, Options),
		compound(Spec),
		Spec =.. [Library, Source],
		logtalk::expand_library_path(Library, Path),
		atom_concat(Source, '.lgt', File),
		atom_concat(Source, '.dot', DotFile),
		member(output_path(Directory), Options),
		os::working_directory(Current),
		os::change_directory(Directory),
		open(DotFile, write, Stream, [alias(dot_file)]),
		dot_header(Options),
		output_file(File, Path, Options),
		dot_footer(Options),
		close(Stream),
		os::change_directory(Current).

	file(Source, UserOptions) :-
		merge_options(UserOptions, Options),
		atom(Source),
		atom_concat(Source, '.lgt', File),
		atom_concat(Source, '.dot', DotFile),
		member(output_path(Directory), Options),
		os::working_directory(Current),
		os::change_directory(Directory),
		open(DotFile, write, Stream, [alias(dot_file)]),
		dot_header(Options),
		output_file(File, _, Options),
		dot_footer(Options),
		close(Stream),
		os::change_directory(Current).

	file(Source) :-
		file(Source, []).

	output_file(File, Path, Options) :-
		(	member(file_names(true), Options) ->
			write(dot_file, 'subgraph "cluster_'),
			write(dot_file, File),
			write(dot_file, '" {\n'),
			write(dot_file, 'bgcolor=snow'),
			write(dot_file, '\nlabel="'),
			write(dot_file, File),
			write(dot_file, '"'),
			nl(dot_file),
			process(File, Path, Options),
			write(dot_file, '}\n'),
			nl(dot_file)
		;	process(File, Path, Options)
		).

	dot_header(Options) :-
		write(dot_file, 'digraph G {'),
		write(dot_file, '\nrankdir=BT'),
		write(dot_file, '\nranksep=1.25'),
		write(dot_file, '\ncompound=true'),
		write(dot_file, '\nclusterrank=local'),
		write(dot_file, '\nlabeljust=l'),
		write(dot_file, '\nfontname="Courier"'),
		write(dot_file, '\nfontsize=10'),
		write(dot_file, '\nfontcolor=snow4'),
		write(dot_file, '\npencolor=snow4'),
		(	member(pages(multiple), Options),
			member(format(Format), Options) ->
			(	Format == a4 ->
				write(dot_file, '\npage="8.3,11.7"'),
				write(dot_file, '\npagedir=TL')
			;	Format == a3 ->
				write(dot_file, '\npage="11.7,16.5"'),
				write(dot_file, '\npagedir=TL')
			;	Format == letter ->
				write(dot_file, '\npage="8.5,11.0"'),
				write(dot_file, '\npagedir=TL')
			;	Format == ledger ->
				write(dot_file, '\npage="11.0,17.0"'),
				write(dot_file, '\npagedir=TL')
			;	true
			)
		;	true
		),
		write(dot_file, '\nnode [style=filled,fillcolor=white,fontname="Courier",fontsize=9]'),
		write(dot_file, '\nedge [fontname="Courier",fontsize=9]'),
		output_date(Options),
		nl(dot_file).

	output_date(Options) :-
		(	member(date(true), Options),
			catch(os::date_time(Year, Month, Day, Hours, Mins, Secs, _), _, fail) ->
			write(dot_file, '\nlabel="Generated on '),
			write(dot_file, Year), write(dot_file, '/'),
			write(dot_file, Month), write(dot_file, '/'),
			write(dot_file, Day),
			write(dot_file, ', '),
			write(dot_file, Hours), write(dot_file, ':'),
			write(dot_file, Mins), write(dot_file, ':'),
			write(dot_file, Secs),
			write(dot_file, '"')
		;	true
		).

	dot_footer(_) :-
		write(dot_file, '}'),
		nl(dot_file).

	process(File, Path, Options) :-
		protocol_property(Protocol, file(File, Path)),
		output_protocol(Protocol, Options),
		fail.
	process(File, Path, Options) :-
		object_property(Object, file(File, Path)),
		output_object(Object, Options),
		fail.
	process(File, Path, Options) :-
		category_property(Category, file(File, Path)),
		output_category(Category, Options),
		fail.
	process(_, _, _).

	output_protocol(Protocol, Options) :-
		print_name(protocol, Protocol, Name),
		(	member(interface(true), Options) ->
			protocol_property(Protocol, public(Predicates)),
			predicate_list_to_atom(Predicates, PredicateText)
		;	PredicateText = ''
		),
		box(Name, PredicateText, protocol),
		output_protocol_relations(Protocol, Options).

	output_object(Object, Options) :-
		print_name(object, Object, Name),
		(	member(interface(true), Options) ->
			object_property(Object, public(Predicates)),
			predicate_list_to_atom(Predicates, PredicateText)
		;	PredicateText = ''
		),
		(	\+ instantiates_class(Object, _),
			\+ specializes_class(Object, _) ->
			box(Name, PredicateText, prototype)
		;	box(Name, PredicateText, instance_or_class)	
		),
		output_object_relations(Object, Options).

	output_category(Category, Options) :-
		print_name(category, Category, Name),
		(	member(interface(true), Options) ->
			category_property(Category, public(Predicates)),
			predicate_list_to_atom(Predicates, PredicateText)
		;	PredicateText = ''
		),
		box(Name, PredicateText, category),
		output_category_relations(Category, Options).

	output_protocol_relations(Protocol, _) :-
		extends_protocol(Protocol, ExtendedProtocol),
		print_name(protocol, Protocol, ProtocolName),
		print_name(protocol, ExtendedProtocol, ExtendedProtocolName),
		arrow(ProtocolName, ExtendedProtocolName, extends),
		fail.
	output_protocol_relations(_, _).

	output_object_relations(Object, _) :-
		implements_protocol(Object, Protocol),
		print_name(object, Object, ObjectName),
		print_name(protocol, Protocol, ProtocolName),
		arrow(ObjectName, ProtocolName, implements),
		fail.
	output_object_relations(Instance, _) :-
		print_name(object, Instance, InstanceName),
		instantiates_class(Instance, Class),
		print_name(object, Class, ClassName),
		arrow(InstanceName, ClassName, instantiates),
		fail.
	output_object_relations(Class, _) :-
		print_name(object, Class, ClassName),
		specializes_class(Class, SuperClass),
		print_name(object, SuperClass, SuperClassName),
		arrow(ClassName, SuperClassName, specializes),
		fail.
	output_object_relations(Prototype, _) :-
		print_name(object, Prototype, PrototypeName),
		extends_object(Prototype, Parent),
		print_name(object, Parent, ParentName),
		arrow(PrototypeName, ParentName, extends),
		fail.
	output_object_relations(Object, _) :-
		print_name(object, Object, ObjectName),
		imports_category(Object, Category),
		print_name(category, Category, CategoryName),
		arrow(ObjectName, CategoryName, imports),
		fail.
	output_object_relations(_, _).

	output_category_relations(Category, _) :-
		extends_category(Category, ExtendedCategory),
		print_name(category, Category, CategoryName),
		print_name(category, ExtendedCategory, ExtendedCategoryName),
		arrow(CategoryName, ExtendedCategoryName, extends),
		fail.
	output_category_relations(Category, _) :-
		implements_protocol(Category, Protocol),
		print_name(category, Category, CategoryName),
		print_name(protocol, Protocol, ProtocolName),
		arrow(CategoryName, ProtocolName, implements),
		fail.
	output_category_relations(Category, _) :-
		complements_object(Category, Object),
		print_name(category, Category, CategoryName),
		print_name(object, Object, ObjectName),
		arrow(ObjectName, CategoryName, complements),
		fail.
	output_category_relations(_, _).

	box(Name, PredicateText, Entity) :-
		entity_shape(Entity, Shape),
		write(dot_file, '"'),
		write(dot_file, Name),
		write(dot_file, '" [shape='),
		write(dot_file, Shape),
		write(dot_file, ',label="'),
		write(dot_file, Name),
		write(dot_file, '\\n'),
		write(dot_file, PredicateText),
		write(dot_file, '"]'),
		nl(dot_file).

	entity_shape(prototype, box).
	entity_shape(instance_or_class, box).
	entity_shape(protocol, note).
	entity_shape(category, component).

	arrow(Start, End, Label) :-
		label_arrowhead(Label, ArrowHead),
		write(dot_file, '"'),
		write(dot_file, Start),
		write(dot_file, '" -> "'),
		write(dot_file, End),
		write(dot_file, '" [arrowhead='),
		write(dot_file, ArrowHead),
		write(dot_file, ',label="'),
		write(dot_file, Label),
		write(dot_file, '"]'),
		nl(dot_file).

	label_arrowhead(extends, vee).
	label_arrowhead(instantiates, normal).
	label_arrowhead(specializes, onormal).
	label_arrowhead(implements, dot).
	label_arrowhead(imports, box).
	label_arrowhead(complements, obox).

	predicate_list_to_atom(List, Atom) :-
		predicate_list_to_atom(List, '', Atom).

	predicate_list_to_atom([], Atom, Atom).
	predicate_list_to_atom([Functor/Arity| Predicates], Atom0, Atom) :-
		number_codes(Arity, ArityCodes),
		atom_codes(ArityAtom, ArityCodes),
		atom_concat(Atom0, '\\n', Atom1),
		atom_concat(Atom1, Functor, Atom2),
		atom_concat(Atom2, '/', Atom3),
		atom_concat(Atom3, ArityAtom, Atom4),
		predicate_list_to_atom(Predicates, Atom4, Atom).

	print_name(object, Object, ObjectName) :-
		(	atom(Object) ->
			ObjectName = Object
		;	object_property(Object, parameter_names(Names)),
			Object =.. [Functor| _],
			ObjectName =.. [Functor| Names]
		).
	print_name(protocol, Protocol, Protocol).
	print_name(category, Category, CategoryName) :-
		(	atom(Category) ->
			CategoryName = Category
		;	object_property(Category, parameter_names(Names)),
			Category =.. [Functor| _],
			CategoryName =.. [Functor| Names]
		).

	merge_options(UserOptions, Options) :-
		(	member(library_paths(LibraryPaths), UserOptions) ->
			true
		;	LibraryPaths = true					% print library paths
		),
		(	member(file_names(FileNames), UserOptions) ->
			true
		;	FileNames = true					% print file names
		),
		(	member(date(Date), UserOptions) ->
			true
		;	Date = true							% print current date
		),
		(	member(interface(Interface), UserOptions) ->
			true
		;	Interface = true					% print public interface
		),
		(	member(output_path(OutputPath), UserOptions) ->
			true
		;	os::working_directory(OutputPath)	% write diagram to the current directory
		),
		(	member(pages(Pages), UserOptions) ->
			true
		;	Pages = single						% generate a single page diagram
		),
		(	member(format(Format), UserOptions) ->
			true
		;	Format = a4							% paper format for multiple pages diagram
		),
		(	member(exclude_files(ExcludeFiles), UserOptions) ->
			true
		;	ExcludeFiles = []					% don't exclude any source files
		),
		(	member(exclude_paths(ExcludePaths), UserOptions) ->
			true
		;	ExcludePaths = []					% don't exclude any sub-directories
		),
		(	member(exclude_entities(ExcludeEntities), UserOptions) ->
			true
		;	ExcludeEntities = []				% don't exclude any entities
		),
		Options = [
			library_paths(LibraryPaths), file_names(FileNames), date(Date), interface(Interface),
			output_path(OutputPath), pages(Pages), format(Format),
			exclude_files(ExcludeFiles), exclude_paths(ExcludePaths), exclude_entities(ExcludeEntities)].

	:- public(default_options/1).
	:- mode(default_options(-list), one).
	:- info(default_options/1, [
		comment is 'Returns a list of the default options used when generating a diagram.',
		argnames is ['DefaultOptions']]).

	default_options(DefaultOptions) :-
		merge_options([], DefaultOptions).

	member(Option, [Option| _]) :-
		!.
	member(Option, [_| Options]) :-
		member(Option, Options).

:- end_object.
