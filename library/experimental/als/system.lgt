
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


	file_exists(File) :-
		{exists_file(File)}.


	file_property(File, Property) :-
		{fail}.


	delete_file(File) :-
		{remove_file(File)}.


	rename_file(Old, New) :-
		{fail}.


	getenv(Variable, Value) :-
		{getenv(Variable, Value)}.


	setenv(Variable, Value) :-
		{fail}.


	date_time(Year, Month, Day, Hours, Mins, Secs) :-
		{date(Year/Month/Day), time(Hours:Mins:Secs)}.


	cpu_time(Time) :-
		{Time is cputime}.


	host_name(Name) :-
		{fail}.


:- end_object.
