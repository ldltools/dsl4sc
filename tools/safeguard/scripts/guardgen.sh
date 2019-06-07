#! /bin/bash
#
# (C) Copyright IBM Corp. 2019.
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# http://www.apache.org/licenses/LICENSE-2.0
# 

set -eu

BINDIR=$(readlink -f `dirname $0`)
LIBDIR=$BINDIR/guardgen_helpers
test -d $LIBDIR || { echo "$LIBDIR does not exist" > /dev/stderr; exit 1; }

RULES2SCXML=rules2scxml
DFA2SCXML=$BINDIR/dfa2scxml.sh
SCXML2GUARDS=$LIBDIR/scxml2guards.xq

usage () {
    echo "guardgen -- pre/post condition generator"
    echo "usage: `basename $0` <option>* <spec_file>"
    echo "options:"
    echo -e "  -s <fmt>\t\tspecify <fmt> as the input type (<fmt> ::= dsl | dfa | scxml)"
    echo -e "  -t <fmt>\t\tspecify <fmt> as the output type (<fmt> ::= json | xml)"
    echo -e "  -o <file>\t\twrite output to <file>"
    echo -e "  -h\t\t\tshow this help"
}

abort ()
{
    local message=$1
    echo "** $message" > /dev/stderr
    exit 1
}

infile=/dev/stdin
intype=

outfile=/dev/stdout
outtype=json

verbose=0

while test $# -gt 0
do
    case $1 in
	-s)
	    intype=$2
	    shift
	    ;;
	-t)
	    outtype=$2
	    shift
	    ;;

	-o | --output)
	    outfile=$2
	    shift
	    ;;

	-h | --help)
	    usage
	    exit 0
	    ;;
	-v | --verbose)
	    verbose=1
	    ;;

	-*)
	    abort "unknow option: $1"
	    ;;
	*) infile=$1
    esac
    shift
done

test -e "$infile" || abort "\"${infile}\" not found"

# detect $intype
if test ".$intype" = .
then
    if test -f "$infile"
    then
	suffix=${infile##*.}
	#suffix=$(echo $infile | sed -r 's/^.*\.([^\.]*$)/\1/')
	case "$suffix" in
	    spec | dsl | rules)
		intype=dsl
		;;
	    dfa | dfa2)
		intype=dfa
		;;
	    scxml)
		intype=scxml
		;;
	    guards)
		intype=guards
		;;
	esac
    fi
    test ".$intype" = . && intype=dsl
fi

test $intype = guards && cat $infile > $outfile && exit 0

# --------------------------------------------------------------------------------
# spec -> scxml
# --------------------------------------------------------------------------------

mkdir -p /tmp/.dsl4sc

scxmlfile=$(tempfile -d /tmp/.dsl4sc -s .scxml)

case $intype in
    spec | dsl | rules)
	${RULES2SCXML} $infile -o $scxmlfile
	;;
    dfa)
	${DFA2SCXML} $infile > $scxmlfile
	;;
    scxml)
	cat $infile > $scxmlfile
	;;
    *)
	abort "unknow input type: $intype"
esac     

# --------------------------------------------------------------------------------
# scxml -> guards
# --------------------------------------------------------------------------------

case $outtype in
    xml)
	SCXML2GUARDS_main="local:scxml2guards (.)"
	;;
    json)
	SCXML2GUARDS_main="local:to_json (local:scxml2guards (.))"
	;;
    *)
	abort "unknown output type: $outtype"
esac

cat <<EOF | xqilla /dev/stdin -i $scxmlfile -o $outfile || { rm -f ${scxmlfile}; abort "xqilla crashed"; }
$(cat ${SCXML2GUARDS})
${SCXML2GUARDS_main}
EOF

rm -f $scxmlfile
true
