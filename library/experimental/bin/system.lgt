
:- object(system,
	implements(systemp)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2004/5/10,
		comment is 'Operating system interface.']).


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


	file_exists(File) :-
		{exists_file(File)}.


	file_property(File, Property) :-
		{fail}.


	delete_file(File) :-
		{atom_concat('rm ', File, Command), unix(Command)}.


	rename_file(Old, New) :-
		{atom_concat('mv ', Old, Temp), atom_concat(' ', New, Command), unix(Command)}.


	getenv(Variable, Value) :-
		{unix_getenv(Variable, Value)}.


	setenv(Variable, Value) :-
		{fail}.


	date_time(Year, Month, Day, Hour, Min, Sec) :-
		{fail}.


	cpu_time(Time) :-
		{ctime(Miliseconds), Time is Miliseconds/1000}.


	host_name(Name) :-
		{hostname(Name)}.


:- end_object.
