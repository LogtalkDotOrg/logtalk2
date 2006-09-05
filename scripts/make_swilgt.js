// =================================================================
// Logtalk - Object oriented extension to Prolog
// Release 2.28.0
//
// Copyright (c) 1998-2006 Paulo Moura.  All Rights Reserved.
// =================================================================

if (WScript.Arguments.Unnamed.Length > 0) {
	usage_help();
	WScript.Quit(0);
}

WScript.Echo('');
WScript.Echo('Creating a shortcut named "Logtalk - SWI-Prolog" for running Logtalk');
WScript.Echo('with SWI-Prolog...');
WScript.Echo('');

var WshShell = new ActiveXObject("WScript.Shell");

var prolog_path = WshShell.RegRead("HKLM\\Software\\SWI\\Prolog\\home") + "\\bin\\plwin.exe";

var FSObject = new ActiveXObject("Scripting.FileSystemObject");

if (!FSObject.FileExists(prolog_path)) {
	WScript.Echo("Error! Cannot find plwin.exe at the expected place!");
	WScript.Echo("Please, edit the script and update the location of the plwin.exe executable.");
	WScript.Quit(1);
}

var WshSystemEnv = WshShell.Environment("SYSTEM");
var WshUserEnv = WshShell.Environment("USER");
var logtalk_home;

if (WshSystemEnv.Item("LOGTALKHOME"))
	logtalk_home = WshSystemEnv.Item("LOGTALKHOME");
else if (WshUserEnv.Item("LOGTALKHOME"))
	logtalk_home = WshUserEnv.Item("LOGTALKHOME")
else {
	WScript.Echo("Error! The environment variable LOGTALKHOME must be defined first!");
	usage_help();
	WScript.Quit(1);
}

if (!FSObject.FolderExists(logtalk_home)) {
	WScript.Echo("The environment variable LOGTALKHOME points to a non-existing directory!");
	WScript.Echo("Its current value is: %LOGTALKHOME%");
	WScript.Echo("The variable must be set to your Logtalk installation directory!");
	WScript.Echo("");
	usage_help();
	WScript.Quit(1);
}

logtalk_home = logtalk_home.replace(/\\/g, "\\\\");

if (!FSObject.FolderExists(logtalk_home + "\\bin"))
	FSObject.CreateFolder(logtalk_home + "\\bin");

var f1 = FSObject.CreateTextFile(logtalk_home + "\\bin\\logtalk_comp_swi.pl", true);
var f2 = FSObject.OpenTextFile(logtalk_home + "\\compiler\\logtalk.pl", 1);
var line;

f1.WriteLine(":- system_module.");
while (!f2.AtEndOfStream) {
	line = f2.ReadLine();
	f1.WriteLine(line);
}

f1.Close();
f2.Close();

f = FSObject.CreateTextFile(logtalk_home + "\\bin\\logtalk_swi.pl", true);

f.WriteLine(":- consult('$LOGTALKUSER/configs/swi.config').");
f.WriteLine(":- consult('$LOGTALKHOME/bin/logtalk_comp_swi.pl').");
f.WriteLine(":- consult('$LOGTALKUSER/libpaths/libpaths.pl').");
f.WriteLine(":- consult('$LOGTALKUSER/configs/swihook.pl').");
f.Close();

var ProgramsPath = WshShell.SpecialFolders("AllUsersPrograms");

if (!FSObject.FolderExists(ProgramsPath + "\\Logtalk")) 
	FSObject.CreateFolder(ProgramsPath + "\\Logtalk");

var link = WshShell.CreateShortcut(ProgramsPath + "\\Logtalk\\Logtalk - SWI-Prolog.lnk");
link.Arguments = '-f "%LOGTALKHOME%\\bin\\logtalk_swi.pl"';
link.Description = 'Runs Logtalk with SWI-Prolog';
link.IconLocation = 'app.exe,1';
link.TargetPath = prolog_path;
link.WindowStyle = 1;
link.WorkingDirectory = logtalk_home;
link.Save();

WScript.Echo('Done. The "Logtalk - SWI-Prolog" shortcut was been added to the');
WScript.Echo('Start Menu Programs. Make sure that the environment variables');
WScript.Echo('LOGTALKHOME and LOGTALKUSER are defined for all users wishing');
WScript.Echo('to use the shortcut.');
WScript.Echo('');
WScript.Echo('Users must run the batch script "cplgtdirs" before using the');
WScript.Echo('"Logtalk -  SWI-Prolog" shortcut.');
WScript.Echo('');

WScript.Quit(0);

function usage_help() {
	WScript.Echo('');
	WScript.Echo('This script creates a shortcut named "Logtalk - SWI-Prolog" for');
	WScript.Echo('running Logtalk with SWI-Prolog. The script must be run by a user');
	WScript.Echo('with administrative rights. The LOGTALKHOME environment variable');
	WScript.Echo('must be defined before running this script.');
	WScript.Echo('');
	WScript.Echo('Usage:');
	WScript.Echo('  ' + WScript.ScriptName + ' help');
	WScript.Echo('  ' + WScript.ScriptName);
	WScript.Echo('');
}
