
:- use_module(library(system)).


:- object(system,
	implements(systemp)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2004/5/10,
		comment is 'Operating system interface.']).


	make_directory(Directory) :-
		{make_directory(Directory)}.


	delete_directory(Directory) :-
		{delete_file(Directory)}.


	change_directory(Directory) :-
		{working_directory(_, Directory)}.


	working_directory(Directory) :-
		{working_directory(Directory, Directory)}.


	directory_exists(Directory) :-
		{fail}.


	directory_files(Directory, Files) :-
		{directory_files(Directory, Files)}.


	file_exists(File) :-
		{file_exists(File)}.


	file_property(File, Property) :-
		{file_property(File, Property)}.


	delete_file(File) :-
		{delete_file(File)}.


	rename_file(Old, New) :-
		{rename_file(Old, New)}.


	getenv(Variable, Value) :-
		{environ(Variable, Value)}.


	setenv(Variable, Value) :-
		{fail}.


	date_time(Year, Month, Day, Hour, Min, Sec) :-
		{datime(datime(Year, Month, Day, Hour, Min, Sec))}.


	cpu_time(Time) :-
		{statistics(runtime, [Miliseconds| _]), Time is Miliseconds/1000}.


	host_name(Name) :-
		{host_name(Name)}.


:- end_object.
