
:- use_module(library(system)).


:- object(system,
	implements(systemp)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2004/5/10,
		comment is 'Operating system interface for Amzi! Prolog.']).


	make_directory(Directory) :-
		{mkdir(Directory)}.


	delete_directory(Directory) :-
		{rmdir(Directory, 0)}.


	change_directory(Directory) :-
		{chdir(Directory)}.


	working_directory(Directory) :-
		{curdir(Directory, Directory)}.


	directory_exists(Directory) :-
		{fail}.


	directory_files(Directory, Files) :-
		{directory_files(Directory, Files)}.


	file_exists(File) :-
		{file_exists(File)}.


	file_property(File, Property) :-
		{file_property(File, Property)}.


	delete_file(File) :-
		{delfile(File, 0)}.


	rename_file(Old, New) :-
		{rename(Old, New)}.


	getenv(Variable, Value) :-
		{get_env_var(Variable, Value)}.


	date_time(Year, Month, Day, Hours, Mins, Secs) :-
		{date(Year, Month, Day), time(Hours, Mins, Secs)}.


	cpu_time(Time) :-
		{Time is cputime}.


	host_name(Name) :-
		{current_host(Name)}.


:- end_object.
