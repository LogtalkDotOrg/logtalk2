
:- object(system,
	implements(systemp)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2004/5/10,
		comment is 'Operating system interface for GNU Prolog.']).


	make_directory(Directory) :-
		{make_directory(Directory)}.


	delete_directory(Directory) :-
		{delete_directory(Directory)}.


	change_directory(Directory) :-
		{change_directory(Directory)}.


	working_directory(Directory) :-
		{working_directory(Directory)}.


	directory_exists(Directory) :-
		{file_exists(Directory), file_property(File, type(directory))}.


	directory_files(Directory, Files) :-
		{directory_files(Directory, Files)}.


	absolute_file_name(File) :-
		{absolute_file_name(File, File)}.


	absolute_file_name(File, Full) :-
		{absolute_file_name(File, Full)}.


	file_exists(File) :-
		{file_exists(File)}.


	file_modtime(File, Time) :-
		{fail}.


	file_modtime(File, Year, Month, Day, Hours, Mins, Secs) :-
		{file_property(File, last_modification(dt(Year, Month, Day, Hours, Mins, Secs)))}.


	file_size(File, Size) :-
		{file_property(File, size(Size))}.


	file_type(File, Type) :-
		{file_property(File, type(Type))}.


	file_permission(File, Permission) :-
		{file_permission(File, Permission)}.
 

	delete_file(File) :-
		{delete_file(File)}.


	rename_file(Old, New) :-
		{rename_file(Old, New)}.


	getenv(Variable, Value) :-
		{environ(Variable, Value)}.


	setenv(Variable, Value) :-
		{fail}.


	date_time(Year, Month, Day, Hours, Mins, Secs) :-
		{date_time(dt(Year, Month, Day, Hours, Mins, Secs))}.


	cpu_time(Time) :-
		{cpu_time(Miliseconds), Time is Miliseconds/1000}.


	host_name(Name) :-
		{host_name(Name)}.


:- end_object.
