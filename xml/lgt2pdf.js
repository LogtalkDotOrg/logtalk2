// =================================================================
// Logtalk - Object oriented extension to Prolog
// Release 2.19.2
//
// Copyright (c) 1998-2004 Paulo Moura.  All Rights Reserved.
// =================================================================

var WshShell = new ActiveXObject("WScript.Shell");

var WshSysEnv = WshShell.Environment("SYSTEM");
var WshUserEnv = WshShell.Environment("USER");

if (!WshSysEnv.Item("LOGTALKHOME") && !WshUserEnv.Item("LOGTALKHOME")) {
	WScript.Echo("The environment variable LOGTALKHOME must be defined first!");
	WScript.Quit(1);
}

var a4_xsl = WshShell.ExpandEnvironmentStrings("%LOGTALKHOME%") + "\\xml\\lgtpdfa4.xsl";
var us_xsl = WshShell.ExpandEnvironmentStrings("%LOGTALKHOME%") + "\\xml\\lgtpdfus.xsl";
var xsl;

var format = "a4";
// var format = "us";

var directory = WshShell.CurrentDirectory;

var processor = "fop";
// var processor = "xep";

var arguments = WScript.Arguments.Unnamed;

if (arguments.Exists("help"))
	usage_help();

arguments = WScript.Arguments.Named;

if (arguments.Exists("f"))
	format = arguments.Item("f");

if (arguments.Exists("d"))
	directory = arguments.Item("d");

if (arguments.Exists("p"))
	processor = arguments.Item("p");

if (format = "a4")
	xsl = a4_xsl;
else if (format = "us")
	xsl = us_xsl;
else {
	WScript.Echo("unsupported paper format:" + format);
	WScript.Echo("");
	WScript.Quit(1);
}

if (processor != "fop" && processor != "xep") {
	WScript.Echo("unsupported XSL-FO processor:" + processor);
	WScript.Echo("");
	WScript.Quit(1);
}

var fso = new ActiveXObject("Scripting.FileSystemObject");

fso.CopyFile(WshShell.ExpandEnvironmentStrings("%LOGTALKHOME%") + "\\xml\\logtalk.dtd", WshShell.CurrentDirectory);
fso.CopyFile(WshShell.ExpandEnvironmentStrings("%LOGTALKHOME%") + "\\xml\\logtalk.xsd", WshShell.CurrentDirectory);

WScript.Echo("");
WScript.Echo("converting XML files to PDF...");

var files = WshShell.CurrentDirectory.Files;
var file;

for (file in files)
	if (file.Extension = ".xml") {
		WScript.Echo("converting " + file.Name);
		WshShell.Run(processor + " -q -xml " + file + " -xsl " + xsl + " -pdf " + directory + "\\" + file.FileName + ".pdf", true);
	}

WScript.Echo("");
WScript.Echo("conversion done");
WScript.Echo("");

fso.DeleteFile("logtalk.dtd");
fso.DeleteFile("logtalk.xsd");

WScript.Quit(0);

function usage_help() {
	WScript.Echo("");
	WScript.Echo("This script converts all Logtalk XML documenting files in the");
	WScript.Echo("current directory to PDF files");
	WScript.Echo("");
	WScript.Echo("Usage: " + WScript.Name + "[help] [/f:format] [/o:directory] [/p:processor]");
	WScript.Echo("");
	WScript.Echo("Optional arguments:");
	WScript.Echo("  f - paper format (either a4 or us; default is " + format + ")");
	WScript.Echo("  o - output directory for the PDF files (default is " + directory + ")");
	WScript.Echo("  p - XSL-FO processor (either fop or xep; default is " + processor + ")");
	WScript.Echo("  help - print usage help");
	WScript.Echo("");
	WScript.Quit(1);
}
