#!/bin/sh

if ! [ $LOGTALKHOME ]
then
	echo "The environment variable LOGTALKHOME must be defined first!"
else

	XSLT="$LOGTALKHOME/xml/lgthtml.xsl"

	if [ -z "$1" ]; then
		title="Entity documentation index"
	else
		title="$1"
	fi

	if [ -z "$2" ]; then
		output_dir="."
	else
		output_dir="$2"
	fi

	if [ -z "$3" ]; then
		index_file="$output_dir/index.html"
	else
		index_file="$output_dir/$3"
	fi

	echo
	echo This script converts all .xml files in the current directory to .html
	echo files applying the XSLT transformation defined in the file:
	echo "    "$XSLT
	echo using the libxslt XSLT processor 1.1.8 or later version. An index file, 
	echo containing links to all generated .html documenting files, is automatically 
	echo generated. 
	echo 
	echo This script accepts three optional parameters: the title to be used in
	echo the index file, the output directory for the HTML files, and the name of
	echo the index file.

	cp $LOGTALKHOME/xml/logtalk.dtd .
	cp $LOGTALKHOME/xml/logtalk.xsd .
	cp $LOGTALKHOME/xml/logtalk.css $output_dir

	echo
	echo converting XML files to HTML...

	for file in *.xml; do
		echo "  converting" $file
		name="`expr "$file" : '\(.*\)\.[^./]*$' \| "$file"`"
		eval xsltproc --output $output_dir/$name.html $XSLT $file
	done

	echo conversion done
	echo
	echo generating index file...

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

	echo index file generated
	echo

	rm logtalk.dtd
	rm logtalk.xsd

fi
