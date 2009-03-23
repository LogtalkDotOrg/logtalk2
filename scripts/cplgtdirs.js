// ================================================================
// Logtalk - Open source object-oriented logic programming language
// Release 2.36.0
//
// Copyright (c) 1998-2009 Paulo Moura. All Rights Reserved.
// 
// Logtalk is free software. You can redistribute it and/or modify
// it under the terms of the Artistic License 2.0 as published by 
// the The Perl Foundation.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// Artistic License 2.0 for more details. A copy of the license is 
// provided in the "LICENSE.txt" file.
// ================================================================

if (ScriptEngineMajorVersion() < 5 || ScriptEngineMajorVersion() == 5 && ScriptEngineMinorVersion() < 6) {
	WScript.Echo('Error! WSH 5.6 or later version needed for running this script.');
	WScript.Quit(1);
}

var WshShell = new ActiveXObject("WScript.Shell");

var WshSystemEnv = WshShell.Environment("SYSTEM");
var WshUserEnv = WshShell.Environment("USER");
var logtalk_home;
var logtalk_user;

if (WshSystemEnv.Item("LOGTALKHOME"))
	logtalk_home = WshSystemEnv.Item("LOGTALKHOME");
else if (WshUserEnv.Item("LOGTALKHOME"))
	logtalk_home = WshUserEnv.Item("LOGTALKHOME")
else {
	WScript.Echo("The environment variable LOGTALKHOME must be defined first!");
	usage_help();
	WScript.Quit(1);
}

var FSObject = new ActiveXObject("Scripting.FileSystemObject");

if (!FSObject.FolderExists(logtalk_home)) {
	WScript.Echo("The environment variable LOGTALKHOME points to a non-existing directory!");
	WScript.Echo("Its current value is: %LOGTALKHOME%");
	WScript.Echo("The variable must be set to your Logtalk installation directory!");
	WScript.Echo("");
	usage_help();
	WScript.Quit(1);
}

if (WScript.Arguments.Unnamed.Length > 0)
	usage_help();

if (WshUserEnv.Item("LOGTALKUSER"))
	logtalk_user = WshUserEnv.Item("LOGTALKUSER");
else {
	logtalk_user = WshShell.SpecialFolders("MyDocuments") + "\\logtalk";
	WshUserEnv.Item("LOGTALKUSER") = WshShell.SpecialFolders("MyDocuments") + "\\logtalk";
	WScript.Echo("Defined user environment variable LOGTALKUSER.");
	WScript.Echo("");
}

if (FSObject.FolderExists(logtalk_user)) {
	var today = new Date();
	var year  = today.getFullYear();
	var month = today.getMonth() + 1;
	var day = today.getDate();
	if (day < 10)
        day = "0" + day;
	var hours = today.getHours();
	if (hours < 10)
        hours = "0" + hours;
	var mins = today.getMinutes();
	if (mins < 10)
        mins = "0" + mins;
	var secs = today.getSeconds();
	date = year + "-" + month + "-" + day + "-" + hours + mins + secs;
	FSObject.MoveFolder(logtalk_user, logtalk_user + "-backup-" + date);
	WScript.Echo("Created a backup of the existing \%LOGTALKUSER\% directory.");
	WScript.Echo("");
	WScript.Echo("Creating a new LOGTALKUSER directory:");
	WScript.Echo("");
	WScript.Echo("  " + logtalk_user);
	WScript.Echo("");
	FSObject.CreateFolder(logtalk_user);
	if (FSObject.FileExists(logtalk_user + "-backup-" + date + "\\settings.lgt")) {
		FSObject.CopyFile(logtalk_user + "-backup-" + date + "\\settings.lgt", logtalk_user + "\\settings.lgt");
		WScript.Echo("Copied your old \"settings.lgt\" file to the new \%LOGTALKUSER\% directory.");
		WScript.Echo("The file \"settings-pristine.lgt\" file contains a pristine copy of the ");
		WScript.Echo("\"settings.lgt\" file distributed with the currently installed Logtalk");
		WScript.Echo("version. Review this file for possible settings files update instructions.");
	}
	WScript.Echo("");
} else {
	WScript.Echo("Creating a new LOGTALKUSER directory:");
	WScript.Echo("");
	WScript.Echo("  " + logtalk_user);
	WScript.Echo("");
	FSObject.CreateFolder(logtalk_user);
}

WScript.Echo("Copying Logtalk files and directories...");
FSObject.CopyFolder(logtalk_home + "\\contributions", logtalk_user + "\\contributions");
FSObject.CopyFolder(logtalk_home + "\\examples", logtalk_user + "\\examples");
FSObject.CopyFolder(logtalk_home + "\\libpaths", logtalk_user + "\\libpaths");
FSObject.CopyFile(logtalk_user + "\\libpaths\\libpaths.pl", logtalk_user + "\\libpaths\\libpaths_no_env_var.pl");
FSObject.CopyFolder(logtalk_home + "\\library", logtalk_user + "\\library");
FSObject.CopyFolder(logtalk_home + "\\xml", logtalk_user + "\\xml");
if (FSObject.FileExists(logtalk_user + "\\settings.lgt"))
	FSObject.CopyFile(logtalk_home + "\\settings.lgt", logtalk_user + "\\settings-pristine.lgt");
else
	FSObject.CopyFile(logtalk_home + "\\settings.lgt", logtalk_user + "\\settings.lgt");
FSObject.CopyFile(logtalk_home + "\\VERSION.txt", logtalk_user + "\\VERSION.txt");

FSObject.DeleteFile(logtalk_user + "\\xml\\lgt2*.*");
FSObject.DeleteFile(logtalk_user + "\\xml\\logtalk.dtd");
FSObject.DeleteFile(logtalk_user + "\\xml\\logtalk.xsd");

var link = WshShell.CreateShortcut(logtalk_user + "\\configs.lnk");
link.Description = "Shortcut to Logtalk config files";
link.TargetPath = logtalk_home + "\\configs";
link.Save();

link = WshShell.CreateShortcut(logtalk_user + "\\manuals.lnk");
link.Description = "Shortcut to Logtalk documentation";
link.TargetPath = logtalk_home + "\\manuals";
link.Save();

link = WshShell.CreateShortcut(logtalk_user + "\\wenv.lnk");
link.Description = "Shortcut to Logtalk text editing support";
link.TargetPath = logtalk_home + "\\wenv";
link.Save();

link = WshShell.CreateShortcut(logtalk_user + "\\xml\\lgt2html.js.lnk");
link.Description = "Shortcut to lgt2html script";
link.TargetPath = logtalk_home + "\\xml\\lgt2html.js";
link.Save();

link = WshShell.CreateShortcut(logtalk_user + "\\xml\\lgt2pdf.js.lnk");
link.Description = "Shortcut to lgt2pdf script";
link.TargetPath = logtalk_home + "\\xml\\lgt2pdf.js";
link.Save();

link = WshShell.CreateShortcut(logtalk_user + "\\xml\\lgt2xml.js.lnk");
link.Description = "Shortcut to lgt2xml script";
link.TargetPath = logtalk_home + "\\xml\\lgt2xml.js";
link.Save();

link = WshShell.CreateShortcut(logtalk_user + "\\xml\\lgt2txt.js.lnk");
link.Description = "Shortcut to lgt2txt script";
link.TargetPath = logtalk_home + "\\xml\\lgt2txt.js";
link.Save();

link = WshShell.CreateShortcut(logtalk_user + "\\xml\\logtalk.dtd.lnk");
link.Description = "Shortcut to Logtalk DTD";
link.TargetPath = logtalk_home + "\\xml\\logtalk.dtd";
link.Save();

link = WshShell.CreateShortcut(logtalk_user + "\\xml\\logtalk.rng.lnk");
link.Description = "Shortcut to Logtalk RELAX NG Schema";
link.TargetPath = logtalk_home + "\\xml\\logtalk.rng";
link.Save();

link = WshShell.CreateShortcut(logtalk_user + "\\xml\\logtalk.xsd.lnk");
link.Description = "Shortcut to Logtalk XML Schema";
link.TargetPath = logtalk_home + "\\xml\\logtalk.xsd";
link.Save();

WScript.Echo("Finished copying Logtalk files and directories.");
WScript.Echo("");
WScript.Echo("You may need to edit the contents of the file:");
WScript.Echo("");
WScript.Echo("  " + logtalk_user + "\\libpaths\\libpaths.pl");
WScript.Echo("");
WScript.Echo("to match your Prolog compiler and operating-system requirements or to");
WScript.Echo("add your own library paths.");
WScript.Echo("");
WScript.Echo("You may want to customize the default Logtalk compiler flags by editing");
WScript.Echo("the \"settings.lgt\" file found in the directory \%LOGTALKUSER\%.");
WScript.Echo("");
WScript.Echo("See the \%LOGTALKUSER\%\\CUSTOMIZE.txt file for details.");
WScript.Echo("");

WScript.Quit(0);

function usage_help() {
	WScript.Echo("");
	WScript.Echo("This script copies the Logtalk user-modifiable files and directories");
	WScript.Echo("to the user home directory. The location can be set by the environment");
	WScript.Echo("variable \%LOGTALKUSER\% (defaults to MyDocuments\\logtalk when the");
	WScript.Echo("variable is not defined)");
	WScript.Echo("");
	WScript.Echo("Usage:");
	WScript.Echo("  " + WScript.ScriptName + " help");
	WScript.Echo("  " + WScript.ScriptName);
	WScript.Echo("");
	WScript.Quit(1);
}
