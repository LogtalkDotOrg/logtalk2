// =================================================================
// Logtalk - Object oriented extension to Prolog
// Release 2.22.0
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

if (WScript.Arguments.Unnamed.Length > 0)
	usage_help();

if !(WshUserEnv.Item("LOGTALKUSER")) {
	var logtalk_user = WshShell.SpecialFolders("MyDocuments") + "\\logtalk";
	WScript.Echo("After the script completion, you must set the user environment variable");
	WScript.Echo("LOGTALKUSER pointing to MyDocuments\\logtalk");
	WScript.Echo("");
}
else {
	var logtalk_user = WshUserEnv.Item("LOGTALKUSER");
}

var fso = new ActiveXObject("Scripting.FileSystemObject");

if (fso.FolderExists(logtalk_user)) {
	WScript.Echo("Error! Logtalk directory already exists! Please rename it or delete it.");
	WScript.Echo("");
	usage_help();
}

WScript.Echo("Creating directory " + logtalk_user + "...");
fso.CreateFolder(logtalk_user);

WScript.Echo("Copying Logtalk files and directories...");
WScript.Echo("");
fso.CopyFolder(logtalk_home + "\\configs", logtalk_user + "\\configs");
fso.CopyFolder(logtalk_home + "\\examples", logtalk_user + "\\examples");
fso.CopyFolder(logtalk_home + "\\libpaths", logtalk_user + "\\libpaths");
fso.CopyFolder(logtalk_home + "\\library", logtalk_user + "\\library");
fso.CopyFolder(logtalk_home + "\\xml", logtalk_user + "\\xml");

WScript.Echo("Finished copying Logtalk files directories.");
WScript.Echo("");
WScript.Echo("You may need to edit the My Documents\\logtalk\\libpaths\\libpaths.pl");
WScript.Echo("file to match your Prolog compiler and operating-system requirements");
WScript.Echo("or to add your own library paths.");
WScript.Echo("");

WScript.Quit(0);

function usage_help() {
	WScript.Echo("");
	WScript.Echo("This script copies the Logtalk user-modifiable files and directories");
	WScript.Echo("to the user home directory (My Documents\\logtalk).");
	WScript.Echo("");
	WScript.Echo("Usage:");
	WScript.Echo("  " + WScript.ScriptName + " help");
	WScript.Echo("  " + WScript.ScriptName);
	WScript.Echo("");
	WScript.Quit(1);
}
