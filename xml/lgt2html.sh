#!/bin/bash

html_xslt="$LOGTALKHOME/xml/lgthtml.xsl"
xhtml_xslt="$LOGTALKHOME/xml/lgtxhtml.xsl"

format=xhtml

directory="."

title="Entity documentation index"

index_file="$directory/index.html"

processor=xsltproc
# processor=xalan
# processor=sabcmd

usage_help()
{
	echo 
	echo This script converts all XML files in the current directory to XHTML or HTML files
	echo
	echo "Usage: $0 -f format -o directory -i index -t title -p processor"
	echo
	echo "Optional arguments:"
	echo "  -f output file format (either xhtml or html; default is $format)"
	echo "  -o output directory for the generated files (default is $directory)"
	echo "  -i name of the index file (default is $index_file)"
	echo "  -t title to be used on the index file (default is $title)"
	echo "  -p XSLT processor (xsltproc, xalan, or sabcmd; default is $processor)"
	echo "  -h help"
	echo
	exit 1
}

xhtml_index_file()
{
	echo "" > $index_file

	echo "<?xml version=\"1.0\"?>" >> $index_file
	echo "<?xml-stylesheet href=\"logtalk.css\" type=\"text/css\"?>" >> $index_file
	echo "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" >> $index_file
	echo "<html lang=\"en\" xml:lang=\"en\" xmlns=\"http://www.w3.org/1999/xhtml\">" >> $index_file
	echo "<head>" >> $index_file
	echo "    <meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\"/>" >> $index_file
	echo "    <title>"$title"</title>" >> $index_file
	echo "    <link rel=\"stylesheet\" href=\"logtalk.css\" type=\"text/css\"/>" >> $index_file
	echo "</head>" >> $index_file
	echo "<body>" >> $index_file
	echo "<h1>"$title"</h1>" >> $index_file
	echo "<ul>" >> $index_file

	for file in *.xml; do
		name="`expr "$file" : '\(.*\)\.[^./]*$' \| "$file"`"
		echo "  indexing" $name.html
		echo "    <li><a href=\""$name.html"\">"$name"</a></li>" >> $index_file
	done

	echo "</ul>" >> $index_file

	date="`eval date`"

	echo "<p>Generated on "$date"</p>" >> $index_file
	echo "</body>" >> $index_file
	echo "</html>" >> $index_file
}

html_index_file()
{
	echo "" > $index_file

	echo "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">" >> $index_file
	echo "<html>" >> $index_file
	echo "<head>" >> $index_file
	echo "    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">" >> $index_file
	echo "    <title>"$title"</title>" >> $index_file
	echo "    <link rel=\"stylesheet\" href=\"logtalk.css\" type=\"text/css\">" >> $index_file
	echo "</head>" >> $index_file
	echo "<body>" >> $index_file
	echo "<h1>"$title"</h1>" >> $index_file
	echo "<ul>" >> $index_file

	for file in *.xml; do
		name="`expr "$file" : '\(.*\)\.[^./]*$' \| "$file"`"
		echo "  indexing" $name.html
		echo "    <li><a href=\""$name.html"\">"$name"</a></li>" >> $index_file
	done

	echo "</ul>" >> $index_file

	date="`eval date`"

	echo "<p>Generated on "$date"</p>" >> $index_file
	echo "</body>" >> $index_file
	echo "</html>" >> $index_file
}

if ! [ $LOGTALKHOME ]
then
	echo "The environment variable LOGTALKHOME must be defined first!"
else

	while getopts "f:o:i:t:p:h" Option
	do
		case $Option in
			f) format="$OPTARG";;
			o) directory="$OPTARG";;
			i) index_file="$OPTARG";;
			t) title="$OPTARG";;
			p) processor="$OPTARG";;
			h) usage_help;;
			*) usage_help;;
		esac
	done

	if [ "$format" = "xhtml" ]
	then
		xslt=$xhtml_xslt
	elif [ "$format" = "html" ]
	then
		xslt=$html_xslt
	else
		echo unsupported output format: $format
		usage_help
		exit 1
	fi

	cp $LOGTALKHOME/xml/logtalk.dtd .
	cp $LOGTALKHOME/xml/logtalk.xsd .
	cp $LOGTALKHOME/xml/logtalk.css $directory

	echo
	echo converting XML files...

	for file in *.xml; do
		echo "  converting" $file
		name="`expr "$file" : '\(.*\)\.[^./]*$' \| "$file"`"
		case "$processor" in
			xsltproc)	eval xsltproc -o $directory/$name.html $xslt $file;;
			xalan)		eval xalan -o $directory/$name.html $file $xslt;;
			sabcmd)		eval sabcmd $xslt $file $directory/$name.html;;
		esac
	done

	echo conversion done
	echo
	echo generating index file...

	index_file=$directory/$index_file

	case "$format" in
		xhtml)	xhtml_index_file;;
		html)	html_index_file;;
	esac

	echo index file generated
	echo

	rm logtalk.dtd
	rm logtalk.xsd

fi
