#/bin/bash

## ================================================================
## Logtalk - Open source object-oriented logic programming language
## Release 2.31.4
##
## Copyright (c) 1998-2008 Paulo Moura.  All Rights Reserved.
## ================================================================

source $LOGTALKHOME/integration/checks.sh

if sicstus -f --goal "halt." 2>&1 | grep "SICStus 4" 2>&1 >/dev/null; then
	exec sicstus -l "$LOGTALKHOME/integration/logtalk_sicstus4.pl" "$@"
else
	exec sicstus -l "$LOGTALKHOME/integration/logtalk_sicstus3.pl" "$@"
fi
