#!/bin/sh

XT_PATH="."
SAX_PATH="."
XP_PATH="."

XSLT="lgthtml.xsl"

echo 
echo This script converts all .xml files in the current directory to .html
echo files applying the XSLT transformation defined in the $XSLT file
echo using the James Clark XT XSLT Java processor
echo
echo
echo converting XML files to HTML...

foreach file (*.xml)
	echo converting $file
	name="`expr "$file" : '\(.*\)\.[^./]*$' \| "$file"`"
	eval java -cp ${XT_PATH}/xt.jar:${SAX_PATH}/sax.jar:${XP_PATH}/xp.jar -Dcom.jclark.xsl.sax.parser=com.jclark.xml.sax.CommentDriver com.jclark.xsl.sax.Driver $file $XSLT $name.html
end

echo
echo conversion done
echo
echo generating index file...
echo

> index.html

foreach file (*.xml)
	name="`expr "$file" : '\(.*\)\.[^./]*$' \| "$file"`"
	echo indexing $name.html
end

echo
echo index file generated
echo
echo 
