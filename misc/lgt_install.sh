#!/bin/sh

echo
echo "Installing Logtalk..."

if [ -z "$1" ]; then
	prefix=/usr/local
else
	prefix="$1"
fi

rm -rf $prefix/lgt2192
rm -f $prefix/logtalk

mkdir $prefix/lgt2192

cd ..
cp -R * $prefix/lgt2192

cd $prefix
chmod -R go-w,a+r lgt2192
chmod a+x lgt2192
chmod a+x lgt2192/misc/*.sh
chmod a+x lgt2192/xml/*.sh
ln -sf lgt2192 logtalk

cd bin
ln -sf ../lgt2192/misc/cplgtdirs.sh cplgtdirs.sh

echo "Installation completed."
echo "Users should define the environment variable LOGTALKHOME pointing"
echo "to $prefix/logtalk and then run the shell script cplgtdirs.sh in"
echo "order to make a local copy of the Logtalk examples, library, and"
echo "xml directories in ~/logtalk."
echo
