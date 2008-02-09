#/bin/bash

## ================================================================
## Logtalk - Open source object-oriented logic programming language
## Release 2.31.4
##
## Copyright (c) 1998-2008 Paulo Moura.  All Rights Reserved.
## ================================================================

source $LOGTALKHOME/integration/checks.sh

if pl -t halt 2>&1 | grep "SWI-Prolog"; then
	exec pl -f "$LOGTALKHOME/integration/logtalk_swi.pl" "$@"
elif swipl -t halt 2>&1 | grep "SWI-Prolog"; then
	exec swipl -f "$LOGTALKHOME/integration/logtalk_swi.pl" "$@"
else case $( uname -s ) in
	Darwin	) exec swipl -f "$LOGTALKHOME/integration/logtalk_swi.pl" "$@";;
	*		) exec pl -f "$LOGTALKHOME/integration/logtalk_swi.pl" "$@";;
esac
fi
