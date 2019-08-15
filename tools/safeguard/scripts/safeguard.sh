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

PREFIX=$(readlink -f $(dirname $0)/..)
LIBDIR=${PREFIX}/lib/dsl4sc

# helpers (for preprocessing spec)
RULES2SCXML=${PREFIX}/bin/rules2scxml
GUARDGEN=${LIBDIR}/tools/guardgen.sh

# js/ts
TRANSPILER=${LIBDIR}/tools/safeguard_helpers/safeguard_ts.js
NODE=${NODE:-node}

usage ()
{
    echo "$(basename $0) -- attach pre/post conditions to event handlers"
    echo "usage: $(basename $0) <option>* <infile>"
    echo "options:"
    echo -e "  --spec[:<lang>] <file>\tread <file> in <lang> as spec (<lang> ::= dsl | dfa | scxml)"
    echo -e "  --code[:<lang>] <file>\tread <file> in <lang> as code (<lang> ::= js | ts)"
    echo -e "  -o <outfile>\t\t\toutput to <outfile>"
    echo -e "  -v, --verbose\t\t\tbecome verbose"
    echo -e "  -h, --help\t\t\tdisplay this message"
}

usage_advanced ()
{
    echo
    echo "advanced/experimental options:"
    echo -e "  --conf <file>\t\t\tread configuration from <file>"
    echo -e "  --confout <file>\t\toutput internally-generated configuration to <file>"
    echo -e "  --print-spec\t\t\tprint state-machine information"
    echo -e "  --parse-only\t\t\tparse code, print its ast, and terminate"
    echo -e "  --conditions=<c>,<c>,..\tspecify conditions to attach (<c> ::= pre | post | ..)"
    echo
    echo -e "  --js-class=<class>\t\tspecify class that carries event handlers"
    echo -e "  --js-initializer=<init>\tname the initializer function"
    echo -e "  --js-decorators=<d>,<d>,..\tuse decorators (experimental)"
    echo -e "  --js-decorators-lib=<file>\timport decorators from <file>"
    echo -e "  --js-keep-decorators\t\tretain decorators without expanding"
    echo
}

abort ()
{
    local message=$1
    echo "[safeguard] $message" > /dev/stderr
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
print_spec=0

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
	-o | --output)
	    outfile=$2
	    shift
	    ;;
	-h | --help)
	    usage
	    exit 0
	    ;;
	-hh)
	    usage; usage_advanced
	    exit 0
	    ;;
	-v | --verbose)
	    verbose=1
	    ;;

	--conf)
	    conffile=$2
	    shift
	    ;;
	--confout)
	    confoutfile=$2
	    shift
	    ;;

	--print-spec)
	    print_spec=1
	    ;;
	-p | --parse-only)
	    # propagate to the transpiler
	    export safeguard_parse_only=true
	    ;;

	--conditions=*)
	    safeguard_conditions=${1:9}
	    ;;
	--no-cond*)
	    safeguard_conditions=
	    ;;

	--js-class=*)
	    safeguard_js_class=${1:11}
	    ;;
	--js-initializer=*)
	    safeguard_js_initializer=${1:17}
	    ;;
	--js-decorators=*)
	    safeguard_js_decorators=${1:16}
	    ;;
	--js-no-deco*)
	    safeguard_js_decorators=
	    ;;
	--js-keep-decorators)
	    safeguard_js_decorators_keep=true
	    ;;
	--js-decorators-lib=*)
	    safeguard_js_decorators_lib=${1:20}
	    ;;

	-*)
	    abort "unknown option: $1"
	    ;;
	*)
	    if test .$codefile = .
	    then codefile=$1
	    elif test .$specfile = .
	    then specfile=$1
	    else abort "invalid argument: \"$1\""
	    fi
    esac
    shift
done

mkdir -p /tmp/.dsl4sc

# --------------------------------------------------------------------------------
# transpile: (codefile, guardfile) -> outfile
# --------------------------------------------------------------------------------

transpile_js ()
{
    test -f "$TRANSPILER" || exit 1
    test -e "$conffile" || abort "conf file (\"$conffile\") not found"

    # case: no decorator in need
    test -z "${safeguard_js_decorators-}" \
      && { $NODE $TRANSPILER $conffile || abort "error in transpilation"; return; }

    # add decorators
    #local decorators=${LIBDIR}/tools/safeguard_helpers/decorators
    local decorators=${safeguard_js_decorators_lib-${LIBDIR}/tools/safeguard_helpers/decorators}
    test -f ${decorators}.ts -o $verbose -eq 0 || echo "[safeguard] file \"$decorators\" not found" 1>&2
    local decorated=$(tempfile -d /tmp/.dsl4sc -s .ts)
    echo "import { ${safeguard_js_decorators-} } from '$decorators'" > $decorated
    $NODE $TRANSPILER $conffile >> $decorated || { rm -f $decorated; abort "error in transpilation"; }
    # case: no decorator expansion
    test ${safeguard_js_decorators_keep-false} = true \
      && { cat $decorated; rm -f $decorated; return; }

    # decorator expansion using tsc
    test -d "$(npm bin)" || abort "npm is not properly configured"
    TSC=$(npm bin)/tsc
    test -x $TSC || abort "tsc not found"
    $TSC -m amd --allowJS --experimentalDecorators -t ES2016 $decorated --outFile /dev/stdout
    rm -f $decorated
}

test -e "$conffile" && { transpile_js > $outfile && exit 0; }

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
	for suffix in spec dsl rules dfa scxml
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

test ${print_spec} -ne 0 && { cat $guardfile > $outfile; rm -f $guardfile; exit 0; }

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

# env variables to look up
# - safeguard_conditions: array
# - safeguard_tracker: string
# - safeguard_tracker_global: bool
# - safeguard_initializer: string
#
# - safeguard_js_class: string
# - safeguard_js_decorators: array

# pre/post-conditions
# - safeguard_conditions = "pre,post,update"
# - conditions = (pre post update)
# - conditions_str = '"pre", "post", "update"' -- to be inserted into the resulting conf file
local conditions
IFS=',' read -r -a conditions <<< ${safeguard_conditions-"pre,post,update"}
local conditions_str=
if test ${#conditions[@]} -gt 0
then
  conditions_str=\"${conditions[0]}\";
  local i=1
  while test $i -lt ${#conditions[@]}
  do
    conditions_str+=", \"${conditions[$i]}\"";
    let i=i+1
  done
fi

# tracker
safeguard_tracker=${safeguard_tracker-"_state"}
safeguard_tracker_global=${safeguard_tracker_global-"null"}

# initializer
safeguard_initializer=${safeguard_initializer-"_reset"}

# js-specific
local js_class=${safeguard_js_class-""}
local js_decorators=
IFS=',' read -r -a js_decorators <<< ${safeguard_js_decorators-""}
local js_decorators_str=
if test ${#js_decorators[@]} -gt 0
then
  js_decorators_str=\"${js_decorators[0]}\";
  local i=1
  while test $i -lt ${#js_decorators[@]}
  do
    js_decorators_str+=", \"${js_decorators[$i]}\"";
    let i=i+1
  done

  # overrides conditions_str
  conditions_str=

  # if the "initial" decorator is specified, tracker cannot be a global variable
  if [[ " ${js_decorators[@]} " == *" initial "* ]]; then safeguard_tracker_global=false; fi
fi

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
$(test ."${js_class}" = . || echo "\"js_class\" : \"${js_class}\",")
"handlers" : []
},
"options" : {
"conditions" : [ ${conditions_str} ],\
$(test ${#js_decorators[@]} -gt 0 && echo -en "\n\"js_decorators\" : [${js_decorators_str}],")
"tracker" : {"name" : "${safeguard_tracker}", "global" : ${safeguard_tracker_global}},
"initializer" : {"name" : "${safeguard_initializer}"}
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

transpile_js > $outfile || { rm -f $guardfile $conffile; exit 1; }

rm -f $guardfile $conffile
true
