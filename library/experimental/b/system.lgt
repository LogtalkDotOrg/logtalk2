
:- object(system,
	implements(systemp)).


	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2004/5/10,
		comment is 'Operating system interface.']).


	make_directory(Directory) :-
		{atom_concat('mkdir ', Directory, Command), system(Command)}.


	delete_directory(Directory) :-
		{atom_concat('rmdir ', Directory, Command), system(Command)}.


	change_directory(Directory) :-
		{chdir(Directory)}.


	working_directory(Directory) :-
		{fail}.


	directory_exists(Directory) :-
		{fail}.


	directory_files(Directory, Files) :-
		{fail}.


	file_exists(File) :-
		{exists(File)}.


	file_property(File, Property) :-
		{fail}.


	delete_file(File) :-
		{atom_concat('rm ', File, Command), system(Command)}.


	rename_file(Old, New) :-
		{atom_concat('mv ', Old, Temp), atom_concat(' ', New, Command), system(Command)}.


	getenv(Variable, Value) :-
		{environ(Variable, Value)}.


	setenv(Variable, Value) :-
		{fail}.


	date_time(Year, Month, Day, Hour, Min, Sec) :-
		{date(Year, Month, Day), time(Hours, Mins, Secs)}.


	cpu_time(Time) :-
		{cputime(Miliseconds), Time is Miliseconds/1000}.


	host_name(Name) :-
		{fail}.


:- end_object.
