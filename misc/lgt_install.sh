#!/bin/sh

echo
echo "Installing Logtalk..."

if [ -z "$1" ]; then
	prefix=/usr/local
else
	prefix="$1"
fi

rm -rf $prefix/lgt2173
rm -f $prefix/logtalk

mkdir $prefix/lgt2173

cd ..
cp -R * $prefix/lgt2173

cd $prefix
chmod -R go-w,a+r lgt2173
chmod a+x lgt2173
chmod a+x lgt2173/misc/*.sh
chmod a+x lgt2173/xml/*.sh
ln -sf lgt2173 logtalk

cd bin
ln -sf ../lgt2173/misc/cplgtdirs.sh cplgtdirs.sh

echo "Installation completed."
echo "Users should define the environment variable LOGTALKHOME pointing"
echo "to $prefix/logtalk and then run the shell script cplgtdirs.sh in"
echo "order to make a local copy of the Logtalk examples, library, and"
echo "xml directories in ~/logtalk."
echo
