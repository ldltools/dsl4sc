#! /bin/bash
# $Id: ldl2scxml.sh,v 1.1 2018/02/09 10:19:00 sato Exp sato $

usage () {
    echo "usage: `basename $0` <option>* <ldl_file>"
    echo "options:"
    echo -e "\t-o <out_file>\t\toutput to <out_file>"
    echo -e "\t-h\t\t\tdisplay this message<outfile>"
    echo -e "\t-v\t\t\tbecome verbose"
    echo -e "\t-u <stage>\t\tterminate when <stage> gets reached"
    echo -e "\t\t\t\t<stage> ::= ldl | mso | dfa | dfadot | dfa2 | dfa3"
    echo -e "\t--map <map_file>\tset <map_file> for scxml generation"
    echo -e "\t--rules <rules_file>\tset <rules_file> for scxml generation"
}

# helpers
BINDIR=$(readlink -f `dirname $0`/../lib/dsl4sc/tools)
LDL2DFA=$BINDIR/ldl2dfa.sh
DFA2SCXML=$BINDIR/dfa2scxml.sh
test -x $LDL2DFA   || { echo "$LDL2DFA not found"; exit 1; }
test -x $DFA2SCXML || { echo "$DFA2SCXML not found"; exit 1; }

infile=/dev/stdin
mapfile=/dev/null
xmlrulesfile=/dev/null
outfile=/dev/stdout
until=scxml
verbose=0

while test $# -gt 0
do
    case $1 in
	-h | --help)
	    usage
	    exit 0
	    ;;
	-v | --verbose)
	    verbose=1
	    ;;
	--map)
	    mapfile=$2
	    shift
	    ;;
	--rules)
	    xmlrulesfile=$2
	    shift
	    ;;
	-u | --until)
	    until=$2
	    shift
	    ;;
	-o | --output)
	    outfile=$2
	    shift
	    ;;
	-*)
	    echo "** unknow option: $1"
	    exit 1
	    ;;
	*) infile=$1
    esac
    shift
done

test -e ${infile} || { echo \"${infile}\" not found; exit 1; }
test $until = "ldl" && { cat $infile > $outfile; exit 0; }

mkdir -p /tmp/.dsl4sc

# --------------------------------------------------------------------------------
# ldl -> dfa
# --------------------------------------------------------------------------------
dfafile=$(tempfile -d /tmp/.dsl4sc -s .dfa)
#echo "ldl2scxml.ldl2dfa : $infile -> $dfafile" > /dev/stderr
${LDL2DFA} $infile -o $dfafile -u $until || { echo "** ${LDL2DFA} crashed" > /dev/stderr; rm -f $dfafile; exit 1; }

case $until in
    ldl | mso)
	cat $dfafile > $outfile; rm -f $dfafile; exit 0 ;;
    dfa)
	xmllint --format $dfafile > $outfile; rm -f $dfafile; exit 0 ;;
    dfadot)
	cat $dfafile > $outfile; rm -f $dfafile; exit 0 ;;
esac

# --------------------------------------------------------------------------------
# dfa -> scxml
# --------------------------------------------------------------------------------
scxmlfile=$(tempfile -d /tmp/.dsl4sc -s .scxml)
#echo "ldl2scxml.dfa2scxml : $dfafile -> $scxmlfile" > /dev/stderr
${DFA2SCXML} ${dfafile} --map "$mapfile" --rules "$xmlrulesfile" -u $until -o $scxmlfile || { echo "** ${DFA2SCXML} crashed"; rm -f $dfafile $scxmlfile; exit 1; }

xmllint --format --encode utf-8 $scxmlfile > $outfile

rm -f $dfafile $scxmlfile
true
