#! /bin/bash

set -euC

usage ()
{
    echo "usage: $0 <n>"
    exit 0
}

test $# -lt 1 && usage

nstate=1
gen=gen_proto
outfile=/dev/stdout

while test $# -gt 0
do
    case $1 in
	-p | --proto*)
	    gen=gen_proto
	    ;;
	-r | --rule*)
	    gen=gen_rules
	    ;;
	-o)
	    outfile=$2;
	    shift;;
	-*)
	    echo "** unknow option: $1"
	    exit 1
	    ;;
	*) nstate=$1
    esac
    shift
done

gen_proto ()
{
n=$1
i=1
echo protocol
while [[ $i -le $n ]]
do
    echo -n " e;"
    let i=i+1
done
echo ";"
}

gen_rules ()
{
n=$1
i=0

cat<<EOF
event e;
variable q : nat ($n);
property
q = 0;
[](<{true}>last -> q = $(expr $n - 1));
EOF

echo rule
while [[ $i -lt $(expr $n - 1) ]]
do
    echo "on e when q=$i ensure q=$(expr $i + 1);"
    let i=i+1
done
echo "on e when q=$i ensure false;"
}

touch $outfile
$gen $nstate >> $outfile
