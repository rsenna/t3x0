#!/bin/sh

T3XDIR=/usr/local/t3x/0

tcvm=`which tcvm`
target='unix'
cc=cc
bytecode=no

  CFLAGS=""
  CFLAGS=$CFLAGS" -m32"			# on 64-bit systems
# CFLAGS=$CFLAGS" -fPIC"		# get rid of stupid linker errors
# CFLAGS=$CFLAGS" -Wl,-z,notext"	# get rid of stupid linker errors

externs=""

targets="targets:"
for x in tcvm unix fbsd dos cpm; do
	if [ -f $T3XDIR/tx-$x ]; then
		targets="$targets $x"
	fi
done

usage() {
	echo \
	"Usage: `basename $0` [-bc] [-t target] [-e extern] infile [outfile]"
	echo "       $targets"
	echo "       -b - use bytecode compiler"
	echo "       -c - link curses library (Unix only)"
	exit 1
}

if [ "$tcvm" = "" ]; then
	echo "$0: error: TCVM not installed"
	exit 1
fi

if [ "$1" = "" ]; then usage; fi

while true; do
	case $1 in
	-t)	target=$2; shift 2 ;;
	-e)	externs="$externs $2"; shift 2 ;;
	-c)	externs="$externs $T3XDIR/unx386/console.c -lcurses"
		shift ;;
	-b)	bytecode=yes; target=tcvm; shift ;;
	-*)	usage ;;
	*)	break ;;
	esac
done

case $target in
	dos|cpm|fbsd|unix|tcvm)
		;;
	*)	echo "$0: no such target: $target"
		exit 1
esac

out=${2:-$1}

if [ -x $T3XDIR/tx-$target -a $bytecode = no ]; then
	$T3XDIR/tx-$target $1 $out
	if [ $? != 0 ]; then exit 1; fi
	if [ "$target" = "fbsd" ]; then
		as -32 -o $out.o $out.s
		ld -o $out $out.o $externs
		strip $out
		rm -f $out.s $out.o
	fi
	if [ "$target" = "unix" ]; then
		$cc $CFLAGS -s -o $out $out.s $T3XDIR/unx386/ux386lib.c \
			$externs
		rm -f $out.s
	fi
elif [ -x $T3XDIR/tx-$target.tc ]; then
	$tcvm $T3XDIR/tx-$target.tc $1 $out
else
	if [ "$target" != "tcvm" ]; then
		echo "$0: target not installed: $target"
		exit 1
	fi
	$tcvm $T3XDIR/txtrn.tc $1 $out
fi
