
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
		{delete_directory(Directory)}.


	change_directory(Directory) :-
		{cd(Directory)}.


	working_directory(Directory) :-
		{working_directory(Directory, Directory)}.


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
		{getenvstr(Variable, Codes), atom_codes(Value, Codes)}.


	setenv(Variable, Value) :-
		{atom_codes(Value, Codes), setenvstr(Variable, Codes)}.


	date_time(Year, Month, Day, Hour, Min, Sec) :-
		{datime(datime(Year, Month, Day, Hour, Min, Sec))}.


	host(Name) :-
		{current_host(Name)}.


:- end_object.
