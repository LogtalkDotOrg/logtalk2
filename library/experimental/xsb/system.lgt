
:- import path_sysop/2, path_sysop/3 from file_io.
:- import datime/1 from standard.


:- object(system,
	implements(systemp)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2004/5/10,
		comment is 'Operating system interface for XSB.']).


	make_directory(Directory) :-
		{path_sysop(mkdir, Directory)}.


	delete_directory(Directory) :-
		{path_sysop(rmdir, Directory)}.


	change_directory(Directory) :-
		{path_sysop(chdir, Directory)}.


	working_directory(Directory) :-
		{path_sysop(cwd, Directory)}.


	directory_exists(Directory) :-
		{path_sysop(isdir, Directory)
		 path_sysop(exists, Directory)}.


	directory_files(Directory, Files) :-
		{directory(Directory, Files)}.


	absolute_file_name(File) :-
		{path_sysop(isabsolute, File)}.


	absolute_file_name(File, Full) :-
		{path_sysop(expand, File, Full)}.


	decompose_file_name(File, Directory, Base, Extension) :-
		{path_sysop(dirname, File, Directory),
		 path_sysop(basename, File, Base),
		 path_sysop(extension, File, Extension)}.


	file_exists(File) :-
		{path_sysop(isplain, File)
		 path_sysop(exists, File)}.


	file_modtime(File, Time) :-
		{path_sysop(modtime, File, [High, Low]),
		 Time is Low + High * 2 ** 24}.


	file_modtime(File, Year, Month, Day, Hours, Mins, Secs) :-
		{fail}.


	file_size(File, Size) :-
		{path_sysop(size, File, Size)}.


	file_permission(File, read) :-
		{path_sysop(readable, File)}.

	file_permission(File, write) :-
		{path_sysop(writable, File)}.

	file_permission(File, execute) :-
		{path_sysop(executable, File)}.
 

	delete_file(File) :-
		{path_sysop(rm, Directory)}.


	rename_file(Old, New) :-
		{path_sysop(rename, Old, New)}.


	symbolic_link(File, Target) :-
		{fail}.


	getenv(Variable, Value) :-
		{fail}.


	setenv(Variable, Value) :-
		{fail}.


	date_time(Year, Month, Day, Hours, Mins, Secs) :-
		{datime(datime(Year, Month, Day, Hours, Mins, Secs))}.


	convert_time(Time, Year, Month, Day, Hours, Mins, Secs) :-
		{fail}.


	cpu_time(Time) :-
		{cputime(Time)}.


	host_name(Name) :-
		{fail}.


:- end_object.
