#! /bin/bash
#
# JS/AltJS parsers (for transpilation) as of mid-2019
# - babel (v7.4)
#   https://github.com/babel/babel/tree/master/packages/babel-parser
#   packages: @babel/core @babel/parser @babel/preset-typescript
# - esprima (v4.0)
#   http://esprima.org/
#   packages: esprima
# - flow (v0.100)
#   https://github.com/facebook/flow/tree/master/src/parser
#   packages: flow flow-bin
#

NODE_BINDIR=${NODE_BINDIR:-$(npm bin)}
NODE_LIBDIR=${NODE_LIBDIR:-$(npm prefix)/node_modules}

BABEL="${NODE_LIBDIR}/@babel/parser/bin/babel-parser.js"
ESPARSE="${NODE_BINDIR}/esparse"
FLOW="${NODE_BINDIR}/flow"

#PARSER=${PARSER:-babel}
PARSER=
language=

infile=/dev/stdin
outfile=/dev/stdout

babel ()
{
test -x "$BABEL" || { echo "$BABEL not found" > /dev/stderr; exit 1; }
test "$language" = ecmascript && { $BABEL $infile > $outfile; exit 0; }
test "$language" = typescript || { echo "$language not recognized" > /dev/null; exit 1; }

# case: language = typescript
cat <<EOF | node > $outfile
var fs = require ('fs');
var parser = require ("@babel/parser");
var code = fs.readFileSync ('$infile', 'utf8');
var ast = parser.parse (code, {sourceType: "module",
   plugins: ["typescript"]});
   // ["@babel/plugin-proposal-decorators", {legacy: true}]
console.log (JSON.stringify (ast, null, "  "));
EOF
}

esprima ()
{
    test -x "$ESPARSE" || { echo "$ESPARSE not found" > /dev/stderr; exit 1; }
    $ESPARSE ${ESPARSE_OPTS} $infile > $outfile
}    

flow ()
{
    test -x "$FLOW" || { echo "$FLOW not found" > /dev/stderr; exit 1; }
    $FLOW ast $infile | jq . > $outfile
}

usage () {
    echo "es2ast -- JS/AltJS parser and AST constructor"
    echo "usage: `basename $0` <option>* <file>"
    echo "options:"
    echo -e "  -p <parser>\t\tparse with <parser> (babel | esprima | flow)"
    echo -e "  -o <file>\t\twrite to <file>"
}

verbose=0

while test $# -gt 0
do
    case $1 in
	-l | --language)
	    language=$2
	    shift
	    ;;
	-p | --parser)
	    PARSER=$2
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
	    echo "** unknow option: $1"
	    exit 1
	    ;;
	*) infile=$1
    esac
    shift
done

test -e "$infile" || { echo "$infile not found" > /dev/stderr; exit 1; }

if test ".$language" = .
then
    if test ! -f $infile
    then
	language=ecmascript
    else
	suffix=$(echo $infile | sed -r 's/^.*\.([^\.]*$)/\1/')
	case "$suffix" in
	    ts)
		language=typescript
		;;
	    js)
		language=ecmascript
		;;
	    *)
		language=ecmascript
	esac
    fi
else
    case "$language" in
	ts)
	    language=typescript
	    ;;
	js)
	    language=ecmascript
	    ;;
	*)
    esac
fi

if test .$PARSER = .
then
    case "$language" in
	*)
	    PARSER=babel
    esac
fi

case $PARSER in
    babel)
	babel
	;;
    esprima | esparse)
	esprima
	;;
    flow)
	flow
	;;
    *)
	echo "invalid specification: \"$PARSER\""
	exit 1
esac
