
:- use_module(library(system)).


:- object(system,
	implements(systemp)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2004/5/10,
		comment is 'Operating system interface for SICStus Prolog.']).


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


	file_modtime(File, Time) :-
		{file_property(File, mod_time(Size))}.


	file_modtime(File, Year, Month, Day, Hours, Mins, Secs) :-
		{file_property(File, mod_time(Size)), datime(Year, Month, Day, Hours, Mins, Secs)}.


	file_size(File, Size) :-
		{file_property(File, size(Size))}.


	delete_file(File) :-
		{delete_file(File)}.


	rename_file(Old, New) :-
		{rename_file(Old, New)}.


	getenv(Variable, Value) :-
		{environ(Variable, Value)}.


	setenv(Variable, Value) :-
		{fail}.


	date_time(Year, Month, Day, Hours, Mins, Secs) :-
		{datime(datime(Year, Month, Day, Hours, Mins, Secs))}.


	cpu_time(Time) :-
		{statistics(runtime, [Miliseconds| _]), Time is Miliseconds/1000}.


	host_name(Name) :-
		{host_name(Name)}.


:- end_object.
