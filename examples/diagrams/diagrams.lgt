
:- object(diagram).

	:- info([
		version is 0.1,
		author is 'Paulo Moura',
		date is 2010/11/18,
		comment is 'Generates entity diagram DOT files for source files and libraries.']).

	:- public(library/1).
	:- mode(library(+atom), one).
	:- info(file/1, [
		comment is 'Creates a diagram for all entities in a library.',
		argnames is ['Library']]).

	library(Library) :-
		logtalk::expand_library_path(Library, Path),
		atom_concat(Library, '.dot', DotFile),
		open(DotFile, write, Stream),
		write(Stream, 'digraph G {\nrankdir=BT'), nl(Stream),
		process(Stream, _, Path),
		write(Stream, '}'), nl(Stream),
		close(Stream).

	:- public(file/1).
	:- mode(file(+atom), one).
	:- info(file/1, [
		comment is 'Creates a diagram for all entities in a loaded source file.',
		argnames is ['File']]).

	file(Source) :-
		atom_concat(Source, '.lgt', File),
		atom_concat(Source, '.dot', DotFile),
		open(DotFile, write, Stream),
		write(Stream, 'digraph G {\nrankdir=BT'), nl(Stream),
		process(Stream, File, _),
		write(Stream, '}'), nl(Stream),
		close(Stream).

	process(Stream, File, Path) :-
		protocol_property(Protocol, file(File, Path)),
		output_protocol(Stream, Protocol),
		fail.
	process(Stream, File, Path) :-
		object_property(Object, file(File, Path)),
		output_object(Stream, Object),
		fail.
	process(Stream, File, Path) :-
		category_property(Category, file(File, Path)),
		output_category(Stream, Category),
		fail.
	process(_, _, _).

	output_protocol(Stream, Protocol) :-
		print_name(protocol, Protocol, Name),
		protocol_property(Protocol, public(Predicates)),
		predicate_list_to_atom(Predicates, PredicateText),
		box(Stream, Name, PredicateText, protocol),
		output_protocol_relations(Stream, Protocol).

	output_object(Stream, Object) :-
		print_name(object, Object, Name),
		object_property(Object, public(Predicates)),
		predicate_list_to_atom(Predicates, PredicateText),
		(	\+ instantiates_class(Object, _),
			\+ specializes_class(Object, _) ->
			box(Stream, Name, PredicateText, prototype)
		;	box(Stream, Name, PredicateText, instance_or_class)	
		),
		output_object_relations(Stream, Object).

	output_category(Stream, Category) :-
		print_name(category, Category, Name),
		category_property(Category, public(Predicates)),
		predicate_list_to_atom(Predicates, PredicateText),
		box(Stream, Name, PredicateText, category),
		output_category_relations(Stream, Category).

	output_protocol_relations(Stream, Protocol) :-
		extends_protocol(Protocol, ExtendedProtocol),
		print_name(protocol, Protocol, ProtocolName),
		print_name(protocol, ExtendedProtocol, ExtendedProtocolName),
		arrow(Stream, ProtocolName, ExtendedProtocolName, extends),
		fail.
	output_protocol_relations(_, _).

	output_object_relations(Stream, Object) :-
		implements_protocol(Object, Protocol),
		print_name(object, Object, ObjectName),
		print_name(protocol, Protocol, ProtocolName),
		arrow(Stream, ObjectName, ProtocolName, implements),
		fail.
	output_object_relations(Stream, Instance) :-
		print_name(object, Instance, InstanceName),
		instantiates_class(Instance, Class),
		print_name(object, Class, ClassName),
		arrow(Stream, InstanceName, ClassName, instantiates),
		fail.
	output_object_relations(Stream, Class) :-
		print_name(object, Class, ClassName),
		specializes_class(Class, SuperClass),
		print_name(object, SuperClass, SuperClassName),
		arrow(Stream, ClassName, SuperClassName, specializes),
		fail.
	output_object_relations(Stream, Prototype) :-
		print_name(object, Prototype, PrototypeName),
		extends_object(Prototype, Parent),
		print_name(object, Parent, ParentName),
		arrow(Stream, PrototypeName, ParentName, extends),
		fail.
	output_object_relations(Stream, Object) :-
		print_name(object, Object, ObjectName),
		imports_category(Object, Category),
		print_name(category, Category, CategoryName),
		arrow(Stream, ObjectName, CategoryName, imports),
		fail.
	output_object_relations(_, _).

	output_category_relations(Stream, Category) :-
		extends_category(Category, ExtendedCategory),
		print_name(category, Category, CategoryName),
		print_name(category, ExtendedCategory, ExtendedCategoryName),
		arrow(Stream, CategoryName, ExtendedCategoryName, extends),
		fail.
	output_category_relations(Stream, Category) :-
		implements_protocol(Category, Protocol),
		print_name(category, Category, CategoryName),
		print_name(protocol, Protocol, ProtocolName),
		arrow(Stream, CategoryName, ProtocolName, implements),
		fail.
	output_category_relations(Stream, Category) :-
		complements_object(Category, Object),
		print_name(category, Category, CategoryName),
		print_name(object, Object, ObjectName),
		arrow(Stream, CategoryName, ObjectName, complements),
		fail.
	output_category_relations(_, _).

	box(Stream, Name, PredicateText, Entity) :-
		entity_shape(Entity, Shape),
		write(Stream, '"'),
		write(Stream, Name),
		write(Stream, '" [shape='),
		write(Stream, Shape),
		write(Stream, ',label="'),
		write(Stream, Name),
		write(Stream, '\\n'),
		write(Stream, PredicateText),
		write(Stream, '"]'),
		nl(Stream).

	entity_shape(prototype, box).
	entity_shape(instance_or_class, tab).
	entity_shape(protocol, note).
	entity_shape(category, component).

	arrow(Stream, Start, End, Label) :-
		label_arrowhead(Label, ArrowHead),
		write(Stream, '"'),
		write(Stream, Start),
		write(Stream, '" -> "'),
		write(Stream, End),
		write(Stream, '" [arrowhead='),
		write(Stream, ArrowHead),
		write(Stream, ',label="'),
		write(Stream, Label),
		write(Stream, '"]'),
		nl(Stream).

	label_arrowhead(extends, normal).
	label_arrowhead(instantiates, normal).
	label_arrowhead(specializes, normal).
	label_arrowhead(implements, normal).
	label_arrowhead(imports, normal).
	label_arrowhead(complements, normal).

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

:- end_object.
