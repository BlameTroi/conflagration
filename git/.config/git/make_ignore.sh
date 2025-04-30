#!/bin/sh
#
# make_ignore.sh
#
# Filter one or more Git ignore templates to remove
# all comments duplicate exclusion patterns. Arguments
# are one or more file names, output is to stdout.
#
# <scriptname> file(s)

cat "$@" | awk '!visited[$0]++' | grep -e "^[^#]"
