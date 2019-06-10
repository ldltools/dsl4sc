#! /bin/bash
#
# (C) Copyright IBM Corp. 2018.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# http://www.apache.org/licenses/LICENSE-2.0
#

set -eu

LIBDIR=$(readlink -f `dirname $0`/../lib/dsl4sc)

# helpers
RULES2SCXML=rules2scxml
GUARDGEN=${LIBDIR}/tools/guardgen.sh
TRANSPILER=${LIBDIR}/tools/safeguard_helpers/safeguard_ts.js
NODE=${NODE:-node}

usage ()
{
    echo "$(basename $0) -- attach pre/post conditions to event handlers"
    echo "usage: $(basename $0) <option>* <infile>"
    echo "options:"
    echo -e "  --spec[:<lang>] <file>\tread <file> in <lang> as spec (<lang> ::= dsl | dfa | scxml)"
    echo -e "  --code[:<lang>] <file>\tread <file> in <lang> as code (<lang> ::= js | ts)"
    echo -e "  --conf <file>\t\t\tread configuration from <file>"
    echo -e "  --confout <file>\t\toutput internally-generated configuration to <file>"
    echo -e "  -o <outfile>\t\t\toutput to <outfile>"
    echo -e "  -v\t\t\t\tbecome verbose"
    echo -e "  -h\t\t\t\tdisplay this message"
}

abort ()
{
    local message=$1
    echo "** $message" > /dev/stderr
    exit 1
}

#infile=
#intype=
outfile=/dev/stdout

specfile=
speclang=
codefile=
codelang=

conffile=
confoutfile=

guardgen_only=0

verbose=0

while test $# -gt 0
do
    case $1 in
	--spec)
	    specfile=$2
	    shift
	    ;;
	--spec:*)
	    specfile=$2
	    speclang=${1##*:}
	    shift
	    ;;
	--code)
	    codefile=$2
	    shift
	    ;;
	--code:*)
	    codefile=$2
	    codelang=${1##*:}
	    shift
	    ;;
	--conf)
	    conffile=$2
	    shift
	    ;;
	--confout)
	    confoutfile=$2
	    shift
	    ;;

	-g | --guardgen_only)
	    guardgen_only=1
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
	    abort "unknown option: $1"
	    ;;
	*)
	    codefile=$1
    esac
    shift
done

mkdir -p /tmp/.dsl4sc

# --------------------------------------------------------------------------------
# transpile: (codefile, guardfile) -> outfile
# --------------------------------------------------------------------------------

transpile ()
{
    test -f "$TRANSPILER" || exit 1
    test -e "$conffile" || abort "conf file (\"$conffile\") not found"
    $NODE $TRANSPILER $conffile
}

test -e "$conffile" && { transpile > $outfile && exit 0; }

# --------------------------------------------------------------------------------
# guardgen: specfile -> guardfile
# --------------------------------------------------------------------------------

# if $specfile is not specified, try auto-detection
if test ! -e "$specfile"
then
    test ."$codefile" = . && abort "code (\"$codefile\") not found"

    if test -f $codefile
    then
	stem=${codefile%.*}
	for suffix in dsl dfa scxml
	do
	    test -f ${stem}.$suffix && { specfile=${stem}.$suffix; spectype=$suffix; }
	done
    fi
fi

test -e "$specfile" || abort "spec (\"$specfile\") not found"

guardgen ()
{
    test -x $GUARDGEN || abort "guardgen (\"$GUARDGEN\") not found"

    local opts="-t json"
    test ."$speclang" = . || opts="$opts -s $speclang"

    $GUARDGEN $opts $specfile
}

guardfile=$(tempfile -d /tmp/.dsl4sc -s .guards)
guardgen > $guardfile
#cat $guardfile

test ${guardgen_only} -ne 0 && { cat $guardfile > $outfile; rm -f $guardfile; exit 0; }

# --------------------------------------------------------------------------------
# confgen: specfile, codefile -> conffile
# --------------------------------------------------------------------------------

test -e "$codefile" || abort "code (\"$codefile\") not found"

if test -f "$codefile" -a ".$codelang" = .
then
    suffix=${codefile##*.}
    case "$suffix" in
	ts | typescript)
	    codelang=ts
	    ;;
	js | javascript)
	    codelang=js
	    ;;
	*)
	    abort "unknown code language: $codelang"
    esac
fi

test ".$codelang" = . && codelang=ts

confgen ()
{
local class=${safeguard_code_class:-""}
local deco_expects=${safeguard_decorator_expects:-true}
local deco_ensures=${safeguard_decorator_ensures:-true}
local deco_update=${safeguard_decorator_update:-true}
local deco_initial=${safeguard_decorator_initial:-true}
local deco_transitions=${safeguard_decorator_transitions:-true}
local tracker_name=${safeguard_tracker_name:-"_state"}
local tracker_local=${safeguard_tracker_local:-false}
local tracker_global=${safeguard_tracker_global:-false}

cat <<EOF
{
"spec" : {
"location" : "$specfile", \
$(test ."$speclang" = . || echo "\"language\" : \"$speclang\",")
"guards" : $(cat $guardfile)
},
"code" : {
"location" : "$codefile", \
$(test ."$codelang" = . || echo "\"language\" : \"$codelang\",") \
$(test ."$class" = . || echo "\"class\" : \"$class\",")
"handlers" : []
},
"options" : {
"decorators" : {
"expects" : ${deco_expects}, "update" : ${deco_update}, "ensures" : ${deco_ensures},
"initial" : ${deco_initial}, "transitions" : ${deco_transitions}
},
"tracker" : {
"name" : "${tracker_name}", "local" : ${tracker_local},  "global" : ${tracker_global}
}
}
}
EOF
}

tempfile=$(tempfile -d /tmp/.dsl4sc -s .conf)
test ."$conffile" = . && confgen > $tempfile || cat $conffile > $tempfile
conffile=$tempfile
test ."$confoutfile" != . && cat $conffile > $confoutfile

# --------------------------------------------------------------------------------
# transpile: (codefile, guardfile) -> outfile
# --------------------------------------------------------------------------------

transpile > $outfile || { rm -f $guardfile $conffile; exit 1; }

rm -f $guardfile $conffile
true
