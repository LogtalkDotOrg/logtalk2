
:- object(system,
	implements(systemp)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2004/5/10,
		comment is 'Operating system interface for XSB.']).


	make_directory(Directory) :-
		{atom_concat('mkdir ', Directory, Command), shell(Command)}.


	delete_directory(Directory) :-
		{atom_concat('rmdir ', Directory, Command), shell(Command)}.


	change_directory(Directory) :-
		{cd(Directory)}.


	working_directory(Directory) :-
		{fail}.


	directory_exists(Directory) :-
		{fail}.


	directory_files(Directory, Files) :-
		{fail}.


	file_exists(File) :-
		{file_exists(File)}.


	file_modtime(File, Time) :-
		{fail}.


	file_modtime(File, Year, Month, Day, Hours, Mins, Secs) :-
		{fail}.


	file_size(File, Size) :-
		{fail}.


	delete_file(File) :-
		{atom_concat('rm ', File, Command), shell(Command)}.


	rename_file(Old, New) :-
		{atom_concat('mv ', Old, Temp), atom_concat(' ', New, Command), shell(Command)}.


	getenv(Variable, Value) :-
		{fail}.


	setenv(Variable, Value) :-
		{fail}.


	date_time(Year, Month, Day, Hours, Mins, Secs) :-
		{datime(datime(Year, Month, Day, Hours, Mins, Secs))}.


	cpu_time(Time) :-
		{cputime(Time)}.


	host_name(Name) :-
		{fail}.


:- end_object.
