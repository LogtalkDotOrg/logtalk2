!TCL=6209, Version 1
!TITLE=Logtalk templates
!SORT=Y

!TEXT=Prototype
:- object(Object).

	:- info([
		version is 1.0,
		author is 'Author',
		date is Year/Month/Day,
		comment is 'Description']).

	

:- end_object.
	
!		


!TEXT=Prototype with parent
:- object(Prototype,
	extends(Parent)).

	:- info([
		version is 1.0,
		author is 'Author',
		date is Year/Month/Day,
		comment is 'Description']).

	

:- end_object.
	  
!

!TEXT=Prototype with protocol
:- object(Prototype,
	implements(Protocol)).

	:- info([
		version is 1.0,
		author is 'Author',
		date is Year/Month/Day,
		comment is 'Description']).

	

:- end_object.

!


!TEXT=Prototype with category
:- object(Prototype,
	imports(Category)).

	:- info([
		version is 1.0,
		author is 'Author',
		date is Year/Month/Day,
		comment is 'Description']).

	

:- end_object.
	  
!


!TEXT=Prototype with all
:- object(Prototype,
	implements(Protocol),
	imports(Category),
	extends(Parent)).

	:- info([
		version is 1.0,
		author is 'Author',
		date is Year/Month/Day,
		comment is 'Description']).

	

:- end_object.

!


!TEXT=Class
:- object(Class,
	specializes(Superclass)).

	:- info([
		version is 1.0,
		author is 'Author',
		date is Year/Month/Day,
		comment is 'Description']).

	

:- end_object.

!


!TEXT=Class with protocol
:- object(Class,
	implements(Protocol),
	specializes(Superclass)).

	:- info([
		version is 1.0,
		author is 'Author',
		date is Year/Month/Day,
		comment is 'Description']).

	

:- end_object.


!


!TEXT=Class with category
:- object(Class,
	imports(Category),
	specializes(Superclass)).

	:- info([
		version is 1.0,
		author is 'Author',
		date is Year/Month/Day,
		comment is 'Description']).

	

:- end_object.

!


!TEXT=Class with metaclass
:- object(Class,
	instantiates(Metaclass),
	specializes(Superclass)).

	:- info([
		version is 1.0,
		author is 'Author',
		date is Year/Month/Day,
		comment is 'Description']).

	

:- end_object.

!


!TEXT=Class with all
:- object(Class,
	implements(Protocol),
	imports(Category),
	instantiates(Metaclass),
	specializes(Superclass)).

	:- info([
		version is 1.0,
		author is 'Author',
		date is Year/Month/Day,
		comment is '${6:Description']).

	

:- end_object.

!


!TEXT=Instance
:- object(Instance,
	instantiates(Class)).

	:- info([
		version is 1.0,
		author is 'Author',
		date is Year/Month/Day,
		comment is 'Description']).

	

:- end_object.

!


!TEXT=Instance with protocol
:- object(Instance,
	implements(Protocol),
	instantiates(Class)).

	:- info([
		version is 1.0,
		author is 'Author',
		date is Year/Month/Day,
		comment is 'Description']).

	

:- end_object.

!


!TEXT=Instance with category
:- object(Instance,
	imports(Category),
	instantiates(Class)).

	:- info([
		version is 1.0,
		author is 'Author',
		date is Year/Month/Day,
		comment is 'Description']).

	

:- end_object.

!


!TEXT=Instance with all
:- object(Instance,
	implements(Protocol),
	imports(Category),
	instantiates(Class)).

	:- info([
		version is 1.0,
		author is 'Author',
		date is Year/Month/Day,
		comment is 'Description']).

	

:- end_object.

!


!TEXT=Protocol
:- protocol(Protocol).

	:- info([
		version is 1.0,
		author is 'Author',
		date is Year/Month/Day,
		comment is 'Description']).

	

:- end_protocol.

!


!TEXT=Protocol (extended)
:- protocol(Extended,
	extends(Minimal)).

	:- info([
		version is 1.0,
		author is 'Author',
		date is Year/Month/Day,
		comment is 'Description']).

	

:- end_protocol.

!


!TEXT=Category
:- category(Category).

	:- info([
		version is 1.0,
		author is 'Author',
		date is Year/Month/Day,
		comment is 'Description']).

	

:- end_category.

!


!TEXT=Category with protocol
:- category(Category,
	implements(Protocol)).

	:- info([
		version is 1.0,
		author is 'Author',
		date is Year/Month/Day,
		comment is 'Description']).

	

:- end_category.

!
