#!/bin/sh

LGT_LIB_DIR="/usr/local/lib/logtalk"
LGT_SHARE_LIB="/usr/local/share/logtalk"

echo
echo This script copies the Logtalk library, xml, and  
echo examples directories to the user home directory
echo

mkdir -p $HOME/logtalk/examples
mkdir -p $HOME/logtalk/library
mkdir -p $HOME/logtalk/xml

cp -RL $LGT_SHARE_LIB/examples $HOME/logtalk/
cp -RL $LGT_LIB_DIR/library $HOME/logtalk/
cp -RL $LGT_LIB_DIR/xml $HOME/logtalk/

echo Logtalk library and examples directories copy done
echo
