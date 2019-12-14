#! /bin/bash
# $Id: dfa2scxml.sh,v 1.1 2018/02/09 10:19:12 sato Exp sato $
#
# (C) Copyright IBM Corp. 2018.
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

BINDIR=$(readlink -f `dirname $0`)
LIBDIR=$BINDIR/dfa2scxml_helpers
MODELGEN=$BINDIR/modelgen.opt
test -x ${MODELGEN} || { echo "** ${MODELGEN} not found" > /dev/stderr; exit 1; }

infile=/dev/stdin
outfile=/dev/stdout
verbose=0
until="scxml"

# extra options that affect post-processing/scxml-generation
generate_monitor=${generate_monitor:-0}
reject_invalid_events=${reject_invalid_events:-0}
accept_transition=${accept_transition:-_accept}

while test $# -gt 0
do
    case $1 in
	-o | --output)
	    outfile=$2
	    shift
	    ;;
	-u | --until)
	    until=$2
	    shift
	    ;;
	-h | --help)
	    echo "usage: `basename $0` <dfa_file>"
	    exit 0
	    ;;
	-v | --verbose)
	    verbose=1
	    ;;

	## monitor
	--monitor)
	    generate_monitor=1
	    ;;
	--ignore*)
	    reject_invalid_events=0
	    ;;
	--reject*)
	    reject_invalid_events=1
	    ;;

	-*)
	    echo "** unknow option: $1"
	    exit 1
	    ;;
	*) infile=$1
    esac
    shift
done

#test -e $infile || { echo "$infile not found" > /dev/stderr; exit 1; }
test -f $infile || { echo "$infile not found" > /dev/stderr; exit 1; }
test $until = "dfa" && { cat $infile > $outfile; exit 0; }

mkdir -p /tmp/.dsl4sc
#echo "dfa2scxml: $infile -> $outfile" > /dev/stderr

test ${generate_monitor} -ne 0 && reject_invalid_events=1

# --------------------------------------------------------------------------------
# dfa1 -> dfa2
#
# information introduced to dfa2
# [state]
# - <formula> indicates possible worlds that correspond with the sate.
# [transition]
# - <formula>
# - <alt_event>, namely, _init, _accept, or _reject
# [rule]
# - <applicable> includes the transitions to which each rule can be applied.
# --------------------------------------------------------------------------------
dfa1file=$infile
dfa2file=$(tempfile -d /tmp/.dsl4sc -s .dfa2)
${MODELGEN} ${dfa1file} -o ${dfa2file} || { echo "** ${MODELGEN} crashed"; rm -f ${dfa1file} ${dfa2file}; exit 1; }

rm -f ${dfa1file}

test $until = "dfa2" && { xmllint --format ${dfa2file} > $outfile; rm -f ${dfa2file}; exit 0; }

# --------------------------------------------------------------------------------
# dfa2 -> dfa3 (postprocessing)
#
# - to each transition, insert the applicable rules
# - to each state, associate the transitions
#
# - (when generating a monitor) add "reject" transitions
# - rename _accept when accept_transition is specified
# --------------------------------------------------------------------------------
#
elim_rejecting=$LIBDIR/elim_rejecting.xq
insert_rules=$LIBDIR/insert_rules.xq
merge_rules=$LIBDIR/merge_rules.xq
attach_transitions=$LIBDIR/attach_transitions.xq
attach_error_transitions=$LIBDIR/attach_error_transitions.xq
rename_acc_transitions=$LIBDIR/rename_acc_transitions.xq
test -f ${insert_rules} || { echo "${insert_rules} not found" > /dev/stderr; exit 1; }
test -f ${attach_transitions} || { echo "${attach_transitions} not found" > /dev/stderr; exit 1; }
#echo "dfa2scxml : $infile -> $outfile" > /dev/stderr

main1="local:elim_rejecting (.)"
main2="local:insert_rules (.)"
main3="local:merge_rules (local:insert_rules (.))"
main4="local:attach_transitions (.)"
main5="local:attach_error_transitions (.)"
main11="local:merge_rules (local:insert_rules (.))"
main99="local:attach_transitions (local:insert_rules (local:elim_rejecting (local:rename_acc_transitions (.))))"
test ${reject_invalid_events} -ne 0 && main99="local:attach_error_transitions (${main99})"

case $until in
    dfa3-1) main=${main1} ;;
    dfa3-2) main=${main2} ;;
    dfa3-3) main=${main3} ;;
    dfa3-4) main=${main4} ;;
    dfa3-5) main=${main5} ;;
    *) main=${main99} ;;
esac

dfa3file=$(tempfile -d /tmp/.dsl4sc -s .dfa3)
cat <<EOF | xqilla /dev/stdin -i ${dfa2file} -o ${dfa3file} || { echo "** xqilla crashed" > /dev/stderr; rm -f ${dfa2file} ${dfa3file}; exit 1; }
declare default element namespace "https://github.com/ldltools/dsl4sc";
declare variable \$accept_transition := "${accept_transition}";
`cat ${rename_acc_transitions}`
`cat ${elim_rejecting}`
`cat ${insert_rules}`
`cat ${merge_rules}`
`cat ${attach_transitions}`
`cat ${attach_error_transitions}`
$main
EOF

rm -f ${dfa2file}

#test $until = "dfa3" && { xmllint --format ${dfa3file} > $outfile; rm -f ${dfa3file}; exit 0; }
case $until in
dfa3*) 
    xmllint --format ${dfa3file} > $outfile; rm -f ${dfa3file}; exit 0
    ;;
esac

# --------------------------------------------------------------------------------
# dfa3 -> puml
# --------------------------------------------------------------------------------

print_in_puml=$LIBDIR/print_in_puml.xq

_print_in_puml () {
local infile=$1
local outfile=$2
test -f ${print_in_puml} || { echo "${print_in_puml} not found" > /dev/stderr; exit 1; }
#echo "** print_in_puml $infile $outfile" > /dev/stderr
cat <<EOF | xqilla /dev/stdin -i $infile -o $outfile || { echo "** xqilla crashed" > /dev/stderr; exit 1; }
`cat ${print_in_puml}`
local:print_in_puml (.)
EOF
}

test $until = "puml" && { _print_in_puml ${dfa3file} $outfile; rm -f ${dfa3file}; exit 0; }

# --------------------------------------------------------------------------------
# dfa3 -> scxml (printing)
#
# - <dfa> -> <scxml>
# - adjust "initial" states (skip mona-generated initial states)
# - copy script/datamodel/* in dfa3 to scxml/datamodel
# --------------------------------------------------------------------------------
print_in_scxml=$LIBDIR/print_in_scxml.xq
test -f ${print_in_scxml} || { echo "${print_in_scxml} not found" > /dev/stderr; exit 1; }

scxmlfile=$(tempfile -d /tmp/.dsl4sc -s .scxml)

# -----
# unescape each scripts/script element of ${dfa3file} -- quick dirty work-around
unescape () {
local file=$1
local escape="${LIBDIR}/escape.opt"
test -x "$escape" || { echo "$escape not found" > /dev/stderr; exit 1; }
#
local unescaped="$(echo "declare default element namespace \"https://github.com/ldltools/dsl4sc\"; .//dfa/scripts" | xqilla /dev/stdin -i $file | $escape -u)"
#echo $unescaped > /dev/stderr
#
local tempfile=${file}.unescaped
cat <<EOF | xqilla /dev/stdin -i $file |\
    gawk -v unescaped="$unescaped" '/^__implementation__$/{print(unescaped);next}{print($0)}' > $tempfile
declare default element namespace "https://github.com/ldltools/dsl4sc";
element dfa {
  .//dfa/@*,
  (for \$n in .//dfa/node() where name (\$n) != "scripts" return \$n),
  element implementation { text { "&#x0a;__implementation__&#x0a;" } }
}
EOF
#cat $tempfile; exit 0
test -f $tempfile && mv -f $tempfile $file
}
fgrep -q '<scripts>' ${dfa3file} && unescape ${dfa3file}
# -----

#
cat <<EOF | xqilla /dev/stdin -i ${dfa3file} -o ${scxmlfile} || { echo "** xqilla crashed" > /dev/stderr; rm -f ${dfa3file} ${scxmlfile}; exit 1; }
`cat ${print_in_scxml}`
local:print_in_scxml (.)
EOF

rm -f ${dfa3file}

test $until = "scxml" && { xmllint --format $scxmlfile > $outfile; rm -f ${dfa3file} $scxmlfile; exit 0; }

#
echo "** unknown output format: $until" > /dev/stderr
rm -f $scxmlfile
false
