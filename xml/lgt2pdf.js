// ================================================================
// Logtalk - Open source object-oriented logic programming language
// Release 2.30.6
//
// Copyright (c) 1998-2007 Paulo Moura.  All Rights Reserved.
// ================================================================

if (ScriptEngineMajorVersion() < 5 || ScriptEngineMajorVersion() == 5 && ScriptEngineMinorVersion() < 6) {
	WScript.Echo('Error! WSH 5.6 or later version needed for running this script.');
	WScript.Quit(1);
}

var WshShell = new ActiveXObject("WScript.Shell");

var format = "a4";
// var format = "us";

var directory = WshShell.CurrentDirectory;

var processor = "fop";
// var processor = "xep";
// var processor = "xinc";

if (WScript.Arguments.Unnamed.Length > 0) {
	usage_help();
	WScript.Quit(0);
}

var WshSystemEnv = WshShell.Environment("SYSTEM");
var WshUserEnv = WshShell.Environment("USER");

var logtalk_home;
var logtalk_user;

if (WshSystemEnv.Item("LOGTALKHOME"))
	logtalk_home = WshSystemEnv.Item("LOGTALKHOME");
else if (WshUserEnv.Item("LOGTALKHOME"))
	logtalk_home = WshUserEnv.Item("LOGTALKHOME")
else {
	WScript.Echo("Error! The environment variable LOGTALKHOME must be defined first!");
	usage_help();
	WScript.Quit(1);
}

if (WshSystemEnv.Item("LOGTALKUSER"))
	logtalk_user = WshSystemEnv.Item("LOGTALKUSER");
else if (WshUserEnv.Item("LOGTALKUSER"))
	logtalk_user = WshUserEnv.Item("LOGTALKUSER")
else {
	WScript.Echo("Error! The environment variable LOGTALKUSER must be defined first!");
	usage_help();
	WScript.Quit(1);
}

var a4_xsl = logtalk_user + "\\xml\\lgtpdfa4.xsl";
var us_xsl = logtalk_user + "\\xml\\lgtpdfus.xsl";
var xsl;

var f_arg = "";
var d_arg = "";
var p_arg = "";

if (WScript.Arguments.Named.Exists("f"))
	f_arg = WScript.Arguments.Named.Item("f");

if (WScript.Arguments.Named.Exists("d"))
	d_arg = WScript.Arguments.Named.Item("d");

if (WScript.Arguments.Named.Exists("p"))
	p_arg = WScript.Arguments.Named.Item("p");

if (f_arg != "" && f_arg != "a4" && f_arg != "us") {
	WScript.Echo("Error! Unsupported paper format:" + f_arg);
	WScript.Echo("");
	usage_help();
} else if (f_arg != "")
	format = f_arg;

var FSObject = new ActiveXObject("Scripting.FileSystemObject");

if (d_arg != "" && !FSObject.FolderExists(d_arg)) {
	WScript.Echo("Error! directory does not exists: " + d_arg);
	WScript.Echo("");
	usage_help();
} else if (d_arg != "")
	directory = d_arg;

if (p_arg != "" && p_arg != "fop" && p_arg != "xep" && p_arg != "xinc") {
	WScript.Echo("Error! Unsupported XSL-FO processor:" + p_arg);
	WScript.Echo("");
	usage_help();
} else if (p_arg != "")
	processor = p_arg;

if (format == "a4")
	xsl = a4_xsl;
else
	xsl = us_xsl;

if (!FSObject.FileExists(WshShell.CurrentDirectory + "\\logtalk.dtd")) {
	FSObject.CopyFile(logtalk_home + "\\xml\\logtalk.dtd", WshShell.CurrentDirectory + "\\logtalk.dtd");
}

if (!FSObject.FileExists(WshShell.CurrentDirectory + "\\custom.ent")) {
	FSObject.CopyFile(logtalk_home + "\\xml\\custom.ent", WshShell.CurrentDirectory + "\\custom.ent");
}

if (!FSObject.FileExists(WshShell.CurrentDirectory + "\\logtalk.xsd")) {
	FSObject.CopyFile(logtalk_home + "\\xml\\logtalk.xsd", WshShell.CurrentDirectory + "\\logtalk.xsd");
}

WScript.Echo("");
WScript.Echo("converting XML files to PDF...");

var files = new Enumerator(FSObject.GetFolder(WshShell.CurrentDirectory).Files);

for (files.moveFirst(); !files.atEnd(); files.moveNext()) {
	var file = files.item().name;
	if (FSObject.GetExtensionName(file) == "xml") {
		WScript.Echo("  converting " + file);
		var pdf_file = directory + "\\" + FSObject.GetBaseName(file) + ".pdf";
		switch (processor) {
			case "fop" :
				WshShell.Run("fop -q -xml \"" + file + "\" -xsl \"" + xsl + "\" -pdf \"" + pdf_file + "\"", true);
				break;
			case "xep" :
				WshShell.Run("xep -q -xml \"" + file + "\" -xsl \"" + xsl + "\" -pdf \"" + pdf_file + "\"", true);
				break;
			case "xinc" :
				WshShell.Run("xinc -xml \"" + file + "\" -xsl \"" + xsl + "\" -pdf \"" + pdf_file + "\"", true);
				break;
		}
	}
}

WScript.Echo("conversion done");
WScript.Echo("");

WScript.Quit(0);

function usage_help() {
	WScript.Echo("");
	WScript.Echo("This script converts all Logtalk XML documenting files in the");
	WScript.Echo("current directory to PDF files");
	WScript.Echo("");
	WScript.Echo("Usage:");
	WScript.Echo("  " + WScript.ScriptName + " [/f:format] [/d:directory] [/p:processor]");
	WScript.Echo("  " + WScript.ScriptName + " help");
	WScript.Echo("");
	WScript.Echo("Optional arguments:");
	WScript.Echo("  f - paper format (either a4 or us; default is " + format + ")");
	WScript.Echo("  d - output directory for the PDF files (default is " + directory + ")");
	WScript.Echo("  p - XSL-FO processor (either fop, xep, or xinc; default is " + processor + ")");
	WScript.Echo("");
	WScript.Quit(1);
}
