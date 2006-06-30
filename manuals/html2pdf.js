// =================================================================
// Logtalk - Object oriented extension to Prolog
// Release 2.28.0
//
// Copyright (c) 1998-2006 Paulo Moura.  All Rights Reserved.
// =================================================================

if (ScriptEngineMajorVersion() < 5 || ScriptEngineMajorVersion() == 5 && ScriptEngineMinorVersion() < 6) {
	WScript.Echo('Error! WSH 5.6 or later version needed for running this script.');
	WScript.Quit(1);
}

var WshShell = new ActiveXObject("WScript.Shell");

var css2xslfo = C:\CSSToXSLFO\css2xslfo1_3_2.jar;

var directory = WshShell.CurrentDirectory;

var processor = "fop";
// var processor = "xep";
// var processor = "xinc";

if (WScript.Arguments.Unnamed.Length > 0) {
	usage_help();
	WScript.Quit(0);
}

var d_arg = "";
var p_arg = "";

if (WScript.Arguments.Named.Exists("d"))
	d_arg = WScript.Arguments.Named.Item("d");

if (WScript.Arguments.Named.Exists("p"))
	p_arg = WScript.Arguments.Named.Item("p");

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

WScript.Echo("");
WScript.Echo("converting HTML files to PDF...");

var files = new Enumerator(FSObject.GetFolder(WshShell.CurrentDirectory).Files);

for (files.moveFirst(); !files.atEnd(); files.moveNext()) {
	var file = files.item().name;
	if (FSObject.GetExtensionName(file) == "html") {
		WScript.Echo("  converting " + file);
		var fo_file = directory + "\\" + FSObject.GetBaseName(file) + ".fo";
		WshShell.Run("java -jar " + css2xslfo + " \"" + file + "\" -fo \"" + fo_file + "\"", true);
		var pdf_file = directory + "\\" + FSObject.GetBaseName(file) + ".pdf";
		WshShell.Run(processor + " -fo \"" + fo_file + "\" -pdf \"" + pdf_file + "\"", true);
		FSObject.DeleteFile(fo_file);
	}
}

WScript.Echo("conversion done");
WScript.Echo("");

WScript.Quit(0);

function usage_help() {
	WScript.Echo("");
	WScript.Echo("This script converts all Logtalk HTML documenting files in the");
	WScript.Echo("current directory to PDF files");
	WScript.Echo("");
	WScript.Echo("Usage:");
	WScript.Echo("  " + WScript.ScriptName + " [/d:directory] [/p:processor]");
	WScript.Echo("  " + WScript.ScriptName + " help");
	WScript.Echo("");
	WScript.Echo("Optional arguments:");
	WScript.Echo("  d - output directory for the PDF files (default is " + directory + ")");
	WScript.Echo("  p - XSL-FO processor (either fop, xep, or xinc; default is " + processor + ")");
	WScript.Echo("");
	WScript.Quit(1);
}
