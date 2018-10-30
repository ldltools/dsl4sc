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
DFA2DFA=$LIBDIR/dfa2dfa.opt
test -x ${DFA2DFA} || { echo "** ${DFA2DFA} not found" > /dev/stderr; exit 1; }

infile=/dev/stdin
mapfile=/dev/null
xmlrulesfile=/dev/null
outfile=/dev/stdout
verbose=0
until="scxml"
reject_invalid_events=${reject_invalid_events:-0}

while test $# -gt 0
do
    case $1 in
	--map)
	    mapfile=$2
	    shift
	    ;;
	--rules)
	    xmlrulesfile=$2
	    shift
	    ;;
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

# --------------------------------------------------------------------------------
# dfa -> dfa2 (preprocessing)
#
# dfa2 combines dfa, map (event->bits), and rules that carry code
# --------------------------------------------------------------------------------
decode_events=$LIBDIR/decode_events.xq
include_rules=$LIBDIR/include_rules.xq
test -f ${decode_events} || { echo "${decode_events} not found" > /dev/stderr; exit 1; }
test -f ${include_rules} || { echo "${include_rules} not found" > /dev/stderr; exit 1; }

main1="local:decode_events (.)"
main2="local:include_rules (.)"
main3="local:include_rules (local:decode_events (.))"

case $until in
    dfa2-1) main=${main1} ;;
    dfa2-2) main=${main2} ;;
    *) main=${main3} ;;
esac

test -f "$mapfile" || { echo "** spurious map ($mapfile)" > /dev/stderr; exit 1; }
test -f "$xmlrulesfile" || { echo "** spurious rules ($xmlrulesfile)" > /dev/stderr; exit 1; }
dfa2file=$(tempfile -d /tmp/.dsl4sc -s .dfa2)
#echo "preprocess : $infile -> ${dfa2file}"
cat <<EOF | xqilla /dev/stdin -i $infile -o ${dfa2file} || { echo "** xqilla crashed" > /dev/stderr; rm -f ${dfa2file}; exit 1; }
declare default element namespace "https://github.com/ldltools/dsl4sc";
declare variable \$alist := doc("`readlink -f $mapfile`")//bits;
`cat ${decode_events}`
declare variable \$rules := doc("`readlink -f $xmlrulesfile`")//rules/rule;
declare variable \$vars := doc("`readlink -f $xmlrulesfile`")//variables/variable;
`cat ${include_rules}`
$main
EOF

case $until in
    dfa2*)
	xmllint --format ${dfa2file} > $outfile; rm -f ${dfa2file}; exit 0
	;;
    *)
esac	

# --------------------------------------------------------------------------------
# dfa2 -> dfa3
#
# information introduced to dfa3
# [state]
# - <formula> indicates possible worlds that correspond with the sate.
# [transition]
# - <formula>
# - <alt_event>, namely, _init, _accept, or _reject
# [rule]
# - <applicable> includes the transitions to which each rule can be applied.
# --------------------------------------------------------------------------------
dfa3file=$(tempfile -d /tmp/.dsl4sc -s .dfa3)
${DFA2DFA} ${dfa2file} -o ${dfa3file} || { echo "** ${DFA2DFA} crashed"; rm -f ${dfa2file} ${dfa3file}; exit 1; }

rm -f ${dfa2file}

test $until = "dfa3" && { xmllint --format ${dfa3file} > $outfile; rm -f ${dfa3file}; exit 0; }

# --------------------------------------------------------------------------------
# dfa3 -> dfa4 (postprocessing)
#
# - to each transition, insert the applicable rules
# - to each state, associate the transitions
# --------------------------------------------------------------------------------
#
elim_rejecting=$LIBDIR/elim_rejecting.xq
insert_rules=$LIBDIR/insert_rules.xq
merge_rules=$LIBDIR/merge_rules.xq
attach_transitions=$LIBDIR/attach_transitions.xq
attach_error_transitions=$LIBDIR/attach_error_transitions.xq
test -f ${insert_rules} || { echo "${insert_rules} not found" > /dev/stderr; exit 1; }
test -f ${attach_transitions} || { echo "${attach_transitions} not found" > /dev/stderr; exit 1; }
#echo "dfa2scxml : $infile -> $outfile" > /dev/stderr

main1="local:elim_rejecting (.)"
main2="local:insert_rules (.)"
main3="local:merge_rules (local:insert_rules (.))"
main4="local:attach_transitions (.)"
main5="local:attach_error_transitions (.)"
main11="local:merge_rules (local:insert_rules (.))"
main99="local:attach_transitions (local:insert_rules (local:elim_rejecting (.)))"
test ${reject_invalid_events} -ne 0 && main99="local:attach_error_transitions (${main99})"

case $until in
    dfa4-1) main=${main1} ;;
    dfa4-2) main=${main2} ;;
    dfa4-3) main=${main3} ;;
    dfa4-4) main=${main4} ;;
    dfa4-5) main=${main5} ;;
    *) main=${main99} ;;
esac

dfa4file=$(tempfile -d /tmp/.dsl4sc -s .dfa4)
cat <<EOF | xqilla /dev/stdin -i ${dfa3file} -o ${dfa4file} || { echo "** xqilla crashed" > /dev/stderr; rm -f ${dfa3file} ${dfa4file}; exit 1; }
declare default element namespace "https://github.com/ldltools/dsl4sc";
`cat ${elim_rejecting}`
`cat ${insert_rules}`
`cat ${merge_rules}`
`cat ${attach_transitions}`
`cat ${attach_error_transitions}`
$main
EOF

rm -f ${dfa3file}

#test $until = "dfa4" && { xmllint --format ${dfa4file} > $outfile; rm -f ${dfa4file}; exit 0; }
case $until in
dfa4*) 
    xmllint --format ${dfa4file} > $outfile; rm -f ${dfa4file}; exit 0
    ;;
esac

# --------------------------------------------------------------------------------
# dfa4 -> scxml (printing)
# --------------------------------------------------------------------------------
print_in_scxml=$LIBDIR/print_in_scxml.xq
test -f ${print_in_scxml} || { echo "${print_in_scxml} not found" > /dev/stderr; exit 1; }

scxmlfile=$(tempfile -d /tmp/.dsl4sc -s .scxml)

cat <<EOF | xqilla /dev/stdin -i ${dfa4file} -o ${scxmlfile} || { echo "** xqilla crashed" > /dev/stderr; rm -f ${dfa4file} ${scxmlfile}; exit 1; }
`cat ${print_in_scxml}`
local:print_in_scxml (.)
EOF

rm -f ${dfa4file}

test $until = "scxml" && { xmllint --format $scxmlfile > $outfile; rm -f ${dfa4file} $scxmlfile; exit 0; }

#
echo "** unknown output format: $until" > /dev/stderr
rm -f $scxmlfile
false
