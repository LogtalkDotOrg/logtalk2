
:- object(system,
	implements(systemp)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2004/5/10,
		comment is 'Operating system interface for ALS Prolog.']).


	make_directory(Directory) :-
		{fail}.


	delete_directory(Directory) :-
		{fail}.


	change_directory(Directory) :-
		{change_cwd(Directory)}.


	working_directory(Directory) :-
		{get_cwd(Directory)}.


	directory_exists(Directory) :-
		{fail}.


	directory_files(Directory, Files) :-
		{files(Directory, '*', Files)}.


	absolute_file_name(File) :-
		{is_absolute_path(File)}.


	absolute_file_name(File, Full) :-
		{fail}.


	decompose_file_name(File, Directory, Base, Extension) :-
		{path_directory_tail(File, Directory, Name),
		 file_extension(Name, Base, Extension)}.


	file_exists(File) :-
		{exists_file(File)}.


	file_modtime(File, Time) :-
		{file_status(File, Status),
		 dmember(mod_time=Time, Status)}.


	file_modtime(File, Year, Month, Day, Hours, Mins, Secs) :-
		{fail}.


	file_size(File, Size) :-
		{file_status(File, Status),
		 dmember(size=Size, Status)}.


	file_type(File, Type) :-
		{file_status(File, Status),
		 dmember(type=Type, Status)}.
 

	file_permission(File, Permission) :-
		{file_status(File, Status),
		 dmember(permissions=Permissions, Status),
		 member(Permission, Permissions)}.
 

	delete_file(File) :-
		{remove_file(File)}.


	rename_file(Old, New) :-
		{fail}.


	symbolic_link(File, Target) :-
		{fail}.


	getenv(Variable, Value) :-
		{getenv(Variable, Value)}.


	setenv(Variable, Value) :-
		{fail}.


	date_time(Year, Month, Day, Hours, Mins, Secs) :-
		{datetime(Year/Month/Day, Hours:Mins:Secs)}.


	convert_time(Time, Year, Month, Day, Hours, Mins, Secs) :-
		{fail}.


	cpu_time(Time) :-
		{Time is cputime}.


	host_name(Name) :-
		{fail}.


:- end_object.
