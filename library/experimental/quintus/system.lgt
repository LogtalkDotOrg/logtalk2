
:- [library(files)].
:- [library(directory)].
:- [library(date)].


:- object(system,
	implements(systemp)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2004/5/10,
		comment is 'Operating system interface for Quintus Prolog.']).


	make_directory(Directory) :-
		{atom_concat('mkdir ', Directory, Command), unix(Command)}.


	delete_directory(Directory) :-
		{atom_concat('rmdir ', Directory, Command), unix(Command)}.


	change_directory(Directory) :-
		{unix(cd(Directory))}.


	working_directory(Directory) :-
		{fail}.


	directory_exists(Directory) :-
		{fail}.


	directory_files(Directory, Files) :-
		{file_members_of_directory(Directory, '*', Files)}.


	absolute_file_name(File) :-
		{absolute_file_name(File, File)}.


	absolute_file_name(File, Full) :-
		{absolute_file_name(File, Full)}.


	decompose_file_name(File, Directory, Base, Extension) :-
		{fail}.


	file_exists(File) :-
		{file_exists(File)}.


	file_modtime(File, Time) :-
		{fail}.


	file_modtime(File, Year, Month, Day, Hours, Mins, Secs) :-
		{file_property(File, modify_time, date(Year, Month, Day, Hours, Mins, Secs))}.


	file_size(File, Size) :-
		{file_property(File, size, Size)}.


	file_type(File, Type) :-
		{file_property(File, type, Type)}.


	file_permission(File, Permission) :-
		{file_exists(File, Permission)}.
 

	delete_file(File) :-
		{delete_file(File)}.


	rename_file(Old, New) :-
		{rename_file(Old, New)}.


	getenv(Variable, Value) :-
		{fail}.


	setenv(Variable, Value) :-
		{fail}.


	date_time(Year, Month, Day, Hours, Mins, Secs) :-
		{date(date(Day, Month, Year)), time(time(Hours, Mins, Secs))}.


	convert_time(Time, Year, Month, Day, Hours, Mins, Secs) :-
		{fail}.


	cpu_time(Time) :-
		{statistics(runtime, [Miliseconds| _]), Time is Miliseconds/1000}.


	host_name(Name) :-
		{fail}.


:- end_object.
