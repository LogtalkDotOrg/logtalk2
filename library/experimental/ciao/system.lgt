
:- use_module(library(system)).


:- object(system,
	implements(systemp)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2004/5/10,
		comment is 'Operating system interface for CIAO.']).


	make_directory(Directory) :-
		{make_directory(Directory)}.


	delete_directory(Directory) :-
		{delete_directory(Directory)}.


	change_directory(Directory) :-
		{cd(Directory)}.


	working_directory(Directory) :-
		{working_directory(Directory, Directory)}.


	directory_exists(Directory) :-
		{fail}.


	directory_files(Directory, Files) :-
		{directory_files(Directory, Files)}.


	absolute_file_name(File) :-
		{fail}.


	absolute_file_name(File, Full) :-
		{fail}.


	file_exists(File) :-
		{file_exists(File)}.


	file_modtime(File, Time) :-
		{modif_time(File, Time)}.


	file_modtime(File, Year, Month, Day, Hours, Mins, Secs) :-
		{modif_time(File, Time), datime(Time, Year, Month, Day, Hours, Mins, Secs, _, _)}.


	file_size(File, Size) :-
		{file_property(File, size(Size))}.


	file_type(File, Type) :-
		{file_property(File, type(Type))}.


	file_permission(File, Permission) :-
		{file_exists(File, Permission)}.
 

	delete_file(File) :-
		{delete_file(File)}.


	rename_file(Old, New) :-
		{rename_file(Old, New)}.


	getenv(Variable, Value) :-
		{getenvstr(Variable, Codes), atom_codes(Value, Codes)}.


	setenv(Variable, Value) :-
		{atom_codes(Value, Codes), setenvstr(Variable, Codes)}.


	date_time(Year, Month, Day, Hours, Mins, Secs) :-
		{datime(datime(Year, Month, Day, Hours, Mins, Secs))}.


	cpu_time(Time) :-
		{statistics(runtime, [Miliseconds| _]), Time is Miliseconds/1000}.


	host_name(Name) :-
		{current_host(Name)}.


:- end_object.
