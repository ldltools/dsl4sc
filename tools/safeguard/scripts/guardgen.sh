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
    echo "usage: `basename $0` <option>* <file>"
    echo "options:"
    echo -e "  --spec[:<lang>] <file>\tread <file> in <lang> as spec (<lang> ::= dsl | dfa | scxml)"
    echo -e "  -s <lang>\t\t\tspecify <lang> as the spec language"
    echo -e "  -t <fmt>\t\t\tspecify <fmt> as the target type (<fmt> ::= json | xml)"
    echo -e "  -o <file>\t\t\twrite output to <file>"
    echo -e "  -h\t\t\t\tshow this help"
}

abort ()
{
    local message=$1
    echo "** $message" > /dev/stderr
    exit 1
}

#infile=/dev/stdin
#intype=xml
specfile=/dev/stdin
speclang=
outfile=/dev/stdout
outtype=json

verbose=0

while test $# -gt 0
do
    case $1 in
	-s)
	    speclang=$2
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

	--spec)
	    specfile=$2
	    shift
	    ;;
	--spec:*)
	    specfile=$2
	    speclang=${1##*:}
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
	*)
	    specfile=$1
    esac
    shift
done

test -e "$specfile" || abort "spec (\"$specfile\") not found"
test "$speclang" = guards && cat $specfile > $outfile && exit 0

mkdir -p /tmp/.dsl4sc

# --------------------------------------------------------------------------------
# spec -> scxml
# --------------------------------------------------------------------------------

if test -f "$specfile" -a ".$speclang" = .
then
    suffix=${specfile##*.}
    #suffix=$(echo $specfile | sed -r 's/^.*\.([^\.]*$)/\1/')
    case "$suffix" in
	spec | dsl | rules)
	    speclang=dsl
	    ;;
	dfa | dfa2)
	    speclang=dfa
	    ;;
	scxml)
	    speclang=scxml
	    ;;
	*)
	    speclang=$suffix
    esac
fi

test ".$speclang" = . && speclang=dsl

scxmlfile=$(tempfile -d /tmp/.dsl4sc -s .scxml)

case $speclang in
    spec | dsl | rules)
	${RULES2SCXML} $specfile -o $scxmlfile
	;;
    dfa)
	${DFA2SCXML} $specfile > $scxmlfile
	;;
    scxml)
	cat $specfile > $scxmlfile
	;;
    *)
	abort "unknown spec language: $speclang"
esac     

# --------------------------------------------------------------------------------
# scxml -> guards
# --------------------------------------------------------------------------------

case $outtype in
    xml | json)
	;;
    *)
	abort "unknown output type: $outtype"
esac

cat <<EOF | xqilla /dev/stdin -o $outfile || { rm -f $scxmlfile; abort "xqilla crashed"; }
declare namespace scxml_ns = "http://www.w3.org/2005/07/scxml";
declare namespace dsl4sc_ns = "https://github.com/ldltools/dsl4sc";
declare default element namespace "https://github.com/ldltools/dsl4sc";
declare variable \$scxml := doc ("$scxmlfile");
$(cat ${SCXML2GUARDS})
local:scxml2guards (\$scxml, "$outtype")
EOF

rm -f $scxmlfile
true
