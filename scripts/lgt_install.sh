#!/bin/sh

echo
echo "Installing Logtalk..."

if [ -z "$1" ]; then
	prefix=/usr/local
else
	prefix="$1"
fi

rm -rf $prefix/lgt2170
rm -f $prefix/logtalk

mkdir $prefix/lgt2170

cd ..
cp -R * $prefix/lgt2170

cd $prefix/lgt2170
chmod -R go-w,a+r .

cd ..
chmod a+x lgt2170
ln -sf lgt2170 logtalk

cd bin
ln -sf ../lgt2170/misc/cplgtdirs.sh cplgtdirs.sh

echo "Installation completed."
echo "Users should define the env variable LOGTALKHOME pointing to $prefix/lgt2170"
echo "and then run the shell script cplgtdirs.sh in order to make a local copy of "
echo "Logtalk examples, xml, and library directories in ~/logtalk."
echo
