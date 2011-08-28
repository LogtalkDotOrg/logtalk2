
:- protocol(osp).

	:- info([
		version is 1.7,
		author is 'Paulo Moura',
		date is 2011/02/04,
		comment is 'Portable operating-system access protocol.']).

	:- public(shell/2).
	:- mode(shell(+atom, -integer), one).
	:- info(shell/2, [
		comment is 'Runs an operating-system shell command. Unifies Status with the exit status of Command.',
		argnames is ['Command', 'Status']]).

	:- public(shell/1).
	:- mode(shell(+atom), zero_or_one).
	:- info(shell/1, [
		comment is 'Runs an operating-system shell command.',
		argnames is ['Command']]).

	:- public(expand_path/2).
	:- mode(expand_path(+atom, -atom), one).
	:- info(expand_path/2, [
		comment is 'Expands a file path to a full file path.',
		argnames is ['Path', 'ExpandedPath']]).

	:- public(make_directory/1).
	:- mode(make_directory(+atom), one).
	:- info(make_directory/1, [
		comment is 'Makes a new directory. Succeeds if the direcory already exists.',
		argnames is ['Directory']]).

	:- public(delete_directory/1).
	:- mode(delete_directory(+atom), one).
	:- info(delete_directory/1, [
		comment is 'Deletes an empty directory.',
		argnames is ['Directory']]).

	:- public(change_directory/1).
	:- mode(change_directory(+atom), one).
	:- info(change_directory/1, [
		comment is 'Changes current working directory.',
		argnames is ['Directory']]).

	:- public(working_directory/1).
	:- mode(working_directory(?atom), zero_or_one).
	:- info(working_directory/1, [
		comment is 'Current working directory.',
		argnames is ['Directory']]).

	:- public(directory_exists/1).
	:- mode(directory_exists(+atom), zero_or_one).
	:- info(directory_exists/1, [
		comment is 'True if the specified directory exists (irrespective of directory permissions).',
		argnames is ['Directory']]).

	:- public(file_exists/1).
	:- mode(file_exists(+atom), zero_or_one).
	:- info(file_exists/1, [
		comment is 'True if the specified file exists (irrespective of type and file permissions).',
		argnames is ['File']]).
 
	:- public(file_modification_time/2).
	:- mode(file_modification_time(+atom, -integer), zero_or_one).
	:- info(file_modification_time/2, [
		comment is 'File modification time.',
		argnames is ['File', 'Time']]).

	:- public(file_size/2).
	:- mode(file_size(+atom, -integer), zero_or_one).
	:- info(file_size/2, [
		comment is 'File size (in bytes).',
		argnames is ['File', 'Size']]).

	:- public(file_permission/2).
	:- mode(file_permission(+atom, ?atom), zero_or_one).
	:- info(file_permission/2, [
		comment is 'True if the specified file has a base name.',
		argnames is ['File', 'Permission']]).

	:- public(rename_file/2).
	:- mode(rename_file(+atom, +atom), zero_or_one).
	:- info(rename_file/2, [
		comment is 'Renames a file (or a directory).',
		argnames is ['Old', 'New']]).

	:- public(delete_file/1).
	:- mode(delete_file(+atom), one).
	:- info(delete_file/1, [
		comment is 'Deletes a file.',
		argnames is ['File']]).

	:- public(environment_variable/2).
	:- mode(environment_variable(+atom, ?atom), zero_or_one).
	:- info(environment_variable/2, [
		comment is 'Argument is a currently defined environment variable. Fails if the variable does not exists.',
		argnames is ['Variable', 'Value']]).

	:- public(time_stamp/1).
	:- mode(time_stamp(-number), one).
	:- info(time_stamp/1, [
		comment is 'Returns a system-dependent time stamp (which can be used for sorting).',
		argnames is ['Time']]).

	:- public(date_time/7).
	:- mode(date_time(-integer, -integer, -integer, -integer, -integer, -integer, -integer), one).
	:- info(date_time/7, [
		comment is 'Returns a system-dependent time stamp (which can be used for sorting).',
		argnames is ['Year', 'Month', 'Day', 'Hours', 'Mins', 'Secs', 'Milisecs']]).

	:- public(cpu_time/1).
	:- mode(cpu_time(-number), one).
	:- info(cpu_time/1, [
		comment is 'System cpu time in seconds.',
		argnames is ['Time']]).

	:- public(wall_time/1).
	:- mode(wall_time(-number), one).
	:- info(wall_time/1, [
		comment is 'Wall time in seconds.',
		argnames is ['Time']]).

	:- public(operating_system_type/1).
	:- mode(operating_system_type(?atom), one).
	:- info(operating_system_type/1, [
		comment is 'Operating system type. Possible values are "unix", "windows", and "unknown".',
		argnames is ['Type']]).

	:- public(command_line_arguments/1).
	:- mode(command_line_arguments(-list(atom)), one).
	:- info(command_line_arguments/1, [
		comment is 'Returns a list with the command line arguments that occur after "--".',
		argnames is ['Arguments']]).

:- end_protocol.
