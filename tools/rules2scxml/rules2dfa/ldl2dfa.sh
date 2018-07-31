#! /bin/bash
# $Id: $

usage () {
    echo "usage: `basename $0` <option>* <ldl_file>"
    echo "options:"
    echo -e "\t-o <out_file>\t\toutput to <out_file>"
    echo -e "\t-h\t\t\tdisplay this message<outfile>"
    echo -e "\t-v\t\t\tbecome verbose"
    echo -e "\t-u <stage>\t\tterminate when <stage> gets reached"
    echo -e "\t\t\t\t<stage> ::= ldl | mso | dfa | dfadot"
}

LDL2MSO=ldl2mso
# helpers
BINDIR=$(readlink -f `dirname $0`)
LIBDIR=$BINDIR/ldl2dfa_helpers
MSO2DFA=$LIBDIR/mso2dfa.sh
DFA2DOT=$BINDIR/dfa2dot.sh
test -x $MSO2DFA || { echo "$MSO2DFA not found"; exit 1; }
test -x $DFA2DOT || { echo "$DFA2DOT not found"; exit 1; }

infile=/dev/stdin
outfile=/dev/stdout
until=dfa
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

test $until = "ldl" && { ${LDL2MSO} $infile -o $outfile --parse-only -t ldl; exit 0; }

# --------------------------------------------------------------------------------
# ldl -> mso
# --------------------------------------------------------------------------------
msofile=$(tempfile -d /tmp/.dsl4sc -s .mso)
${LDL2MSO} $infile -o $msofile || { echo "** ${LDL2MSO} crashed" > /dev/stderr; rm -f $msofile; exit 1; }

test $until = "mso" && { cat $msofile > $outfile; rm -f $msofile; exit 0; }

# --------------------------------------------------------------------------------
# mso -> dfa
# --------------------------------------------------------------------------------
dfafile=$(tempfile -d /tmp/.dsl4sc -s .dfa.xml)
#echo "ldl2dfa.mso2dfa : $msofile -> $dfafile" > /dev/stderr
${MSO2DFA} $msofile -o $dfafile || { echo "** ${MSO2DFA} crashed" > /dev/stderr; rm -f $msofile $dfafile; exit 1; }
rm -f $msofile

test $until = "dfadot" && { ${DFA2DOT} $dfafile -o $outfile; rm -f $dfafile; exit 0; }

cat $dfafile > $outfile
rm -f $dfafile
true
