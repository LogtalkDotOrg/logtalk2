// =================================================================
// Logtalk - Object oriented extension to Prolog
// Release 2.19.2
//
// Copyright (c) 1998-2004 Paulo Moura.  All Rights Reserved.
// =================================================================

var WshShell = new ActiveXObject("WScript.Shell");

var WshProcessEnv = WshShell.Environment("PROCESS");
var WshSystemEnv = WshShell.Environment("SYSTEM");
var WshUserEnv = WshShell.Environment("USER");
var logtalk_home;

if (WshProcessEnv.Item("LOGTALKHOME"))
	logtalk_home = WshProcessEnv.Item("LOGTALKHOME");
else if (WshSystemEnv.Item("LOGTALKHOME"))
	logtalk_home = WshSystemEnv.Item("LOGTALKHOME");
else if (WshUserEnv.Item("LOGTALKHOME"))
	logtalk_home = WshUserEnv.Item("LOGTALKHOME")
else {
	WScript.Echo("Error! The environment variable LOGTALKHOME must be defined first!");
	usage_help();
	WScript.Quit(1);
}

logtalk_home = logtalk_home.replace(/\\/g, "\\\\");

if (WScript.Arguments.Unnamed.Length > 0) {
	usage_help();
	WScript.Quit(0);
}

WScript.Echo("");
WScript.Echo("Creating a shortcut named Sicstuslgt for running Logtalk with SICStus Prolog...");

var fso = new ActiveXObject("Scripting.FileSystemObject");

if (!fso.FolderExists(logtalk_home + "\\bin")) 
	fso.CreateFolder(logtalk_home + "\\bin");

var f = fso.CreateTextFile(logtalk_home + "\\bin\\logtalksicstus.pl", true);

f.WriteLine(":- consult('" + logtalk_home + "\\\\configs\\\\sicstus.config').");
f.WriteLine(":- consult('" + logtalk_home + "\\\\compiler\\\\logtalk.pl').");
f.Close();

var ProgramsPath = WshShell.SpecialFolders("AllUsersPrograms");
var link = WshShell.CreateShortcut(ProgramsPath + "\\Sicstuslgt.lnk");
link.Arguments = "-l %LOGTALKHOME%\\bin\\logtalksicstus.pl";
link.Description = "Runs Logtalk with SICStus Prolog";
link.IconLocation = "app.exe,1";
link.TargetPath = WshShell.RegRead("HKEY_LOCAL_MACHINE\\Software\\SICS\\SICStus3.11_win32\\SP_PATH") + "\\bin\\spwin.exe";
link.WindowStyle = 1;
link.WorkingDirectory = logtalk_home;
link.Save();

WScript.Echo("Done. The Sicstuslgt shortcut was been added to the Start Menu Programs.");
WScript.Echo("Make sure that the LOGTALKHOME environment variable is defined for all");
WScript.Echo("users wishing to use the shortcut."
WScript.Echo("");

WScript.Quit(0);

function usage_help() {
	WScript.Echo("");
	WScript.Echo("This script creates a shortcut named Sicstuslgtfor running Logtalk with");
	WScript.Echo("SICStus Prolog. The script must be run by an user with administrative rights.");
	WScript.Echo("The LOGTALKHOME environment variable must be defined before running this");
	WScript.Echo("script.");
	WScript.Echo("");
	WScript.Echo("Usage:");
	WScript.Echo("  " + WScript.ScriptName + " help");
	WScript.Echo("  " + WScript.ScriptName);
	WScript.Echo("");
}
