
:- object(system,
	implements(systemp)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2004/5/10,
		comment is 'Operating system interface for SWI-Prolog.']).


	make_directory(Directory) :-
		{make_directory(Directory)}.


	delete_directory(Directory) :-
		{delete_directory(Directory)}.


	change_directory(Directory) :-
		{working_directory(_, Directory)}.


	working_directory(Directory) :-
		{working_directory(Directory, Directory)}.


	directory_exists(Directory) :-
		{exists_directory(Directory)}.


	directory_files(Directory, Files) :-
		{fail}.


	absolute_file_name(File) :-
		{is_absolute_file_name(File)}.


	absolute_file_name(File, Full) :-
		{absolute_file_name(File, Full)}.


	file_exists(File) :-
		{exists_file(File)}.


	file_modtime(File, Time) :-
		{time_file(File, Time)}.


	file_modtime(File, Year, Month, Day, Hours, Mins, Secs) :-
		{time_file(File, Time), convert_time(Time, Year, Month, Day, Hours, Mins, Secs)}.


	file_size(File, Size) :-
		{size_file(File, Size)}.


	file_type(File, Type) :-
		{fail}.


	delete_file(File) :-
		{delete_file(File)}.


	rename_file(Old, New) :-
		{rename_file(Old, New)}.


	getenv(Variable, Value) :-
		{getenv(Variable, Value)}.


	setenv(Variable, Value) :-
		{setenv(Variable, Value)}.


	date_time(Year, Month, Day, Hours, Mins, Secs) :-
		{get_time(Time), convert_time(Time, Year, Month, Day, Hours, Mins, Secs, _)}.


	cpu_time(Time) :-
		{Time is cputime}.


	host_name(Name) :-
		{fail}.


:- end_object.
