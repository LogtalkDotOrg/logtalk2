
:- object(system,
	implements(systemp)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2004/5/10,
		comment is 'Operating system interface for Bin Prolog.']).


	make_directory(Directory) :-
		{atom_concat('mkdir ', Directory, Command), unix(Command)}.


	delete_directory(Directory) :-
		{atom_concat('rmdir ', Directory, Command), unix(Command)}.


	change_directory(Directory) :-
		{cd(Directory)}.


	working_directory(Directory) :-
		{pwd(Chars), atom_chars(Directory, Chars)}.


	directory_exists(Directory) :-
		{fail}.


	directory_files(Directory, Files) :-
		{fail}.


	absolute_file_name(File) :-
		{fail}.


	absolute_file_name(File, Full) :-
		{fail}.


	decompose_file_name(File, Directory, Base, Extension) :-
		{fail}.


	file_exists(File) :-
		{exists_file(File)}.


	file_modtime(File, Time) :-
		{fail}.


	file_modtime(File, Year, Month, Day, Hours, Mins, Secs) :-
		{fail}.


	file_size(File, Size) :-
		{file_size(File, Size)}.


	file_type(File, Type) :-
		{fail}.


	file_permission(File, read) :-
		{unix_access(File, 4)}.

	file_permission(File, write) :-
		{unix_access(File, 2)}.

	file_permission(File, execute) :-
		{unix_access(File, 1)}.
 

	delete_file(File) :-
		{atom_concat('rm ', File, Command), unix(Command)}.


	rename_file(Old, New) :-
		{atom_concat('mv ', Old, Temp), atom_concat(' ', New, Command), unix(Command)}.


	symbolic_link(File, Target) :-
		{fail}.


	getenv(Variable, Value) :-
		{unix_getenv(Variable, Value)}.


	setenv(Variable, Value) :-
		{fail}.


	date_time(Year, Month, Day, Hours, Mins, Secs) :-
		{fail}.


	convert_time(Time, Year, Month, Day, Hours, Mins, Secs) :-
		{fail}.


	cpu_time(Time) :-
		{ctime(Miliseconds), Time is Miliseconds/1000}.


	host_name(Name) :-
		{hostname(Name)}.


:- end_object.
