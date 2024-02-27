#!/bin/sh

  CFLAGS=""
  CFLAGS=$CFLAGS" -m32"			# on 64-bit systems
# CFLAGS=$CFLAGS" -fPIC"		# get rid of stupid linker errors
# CFLAGS=$CFLAGS" -Wl,-z,notext"	# get rid of stupid linker errors

usage() {
	echo "Usage: $0 host target"
	echo "       available hosts/targets: tcvm cpm dos fbsd unix"
	exit 1
}

unsupported() {
	echo "$0: host=$1, target=$2 is not supported (too big)"
	exit 1
}

reset() {
	ln -fs txemtbin.t txemit.t
	ln -fs txtcvm.t t3x.t
	ln -fs cgtcvm.t cg.t
}

keep=0
if [ "x$1" = "x-k" ]; then
	keep=1
	shift
fi

if [ $# != 2 ]; then usage; fi

case "$1$2" in
tcvmtcvm) eh=bin; et=bin; h=tcvm;   t=tcvm;   s=.tc;  dh=.;       dt=. ;;
tcvmcpm)  eh=bin; et=bin; h=tcvm;   t=xtocpm; s=.tc;  dh=.;       dt=targets ;;
tcvmdos)  eh=bin; et=bin; h=tcvm;   t=dos86c; s=.tc;  dh=.;       dt=targets ;;
tcvmfbsd) eh=src; et=bin; h=tcvm;   t=fbd386; s=.tc;  dh=.;       dt=targets ;;
tcvmunix) eh=src; et=bin; h=tcvm;   t=unx386; s=.tc;  dh=.;       dt=targets ;;
cpmtcvm)  eh=bin; et=bin; h=cpmz80; t=cpmtvm; s=.com; dh=targets; dt=targets ;;
cpmcpm)   eh=bin; et=bin; h=cpmz80; t=cpmz80; s=.com; dh=targets; dt=targets ;;
cpmdos)   eh=bin; et=bin; h=cpmz80; t=cpmdos; s=.com; dh=targets; dt=targets ;;
cpmfbsd)  unsupported $1 $2 ;;
cpmunix)  unsupported $1 $2 ;;
dostcvm)  eh=bin; et=bin; h=dos86c; t=tcvm;   s=.com; dh=targets; dt=. ;;
doscpm)   eh=bin; et=bin; h=dos86c; t=xtocpm; s=.com; dh=targets; dt=targets ;;
dosdos)   eh=bin; et=bin; h=dos86c; t=dos86c; s=.com; dh=targets; dt=targets ;;
dosfbsd)  unsupported $1 $2 ;;
dosunix)  unsupported $1 $2 ;;
fbsdtcvm) eh=src; et=bin; h=fbd386; t=tcvm;   s="";   dh=targets; dt=. ;;
fbsdcpm)  eh=src; et=bin; h=fbd386; t=xtocpm; s="";   dh=targets; dt=targets ;;
fbsddos)  eh=src; et=bin; h=fbd386; t=dos86c; s="";   dh=targets; dt=targets ;;
fbsdfbsd) eh=src; et=src; h=fbd386; t=fbd386; s="";   dh=targets; dt=targets ;;
unixtcvm) eh=src; et=bin; h=unx386; t=tcvm;   s="";   dh=targets; dt=. ;;
unixcpm)  eh=src; et=bin; h=unx386; t=xtocpm; s="";   dh=targets; dt=targets ;;
unixdos)  eh=src; et=bin; h=unx386; t=dos86c; s="";   dh=targets; dt=targets ;;
unixunix) eh=src; et=src; h=unx386; t=unx386; s="";   dh=targets; dt=targets ;;
*)        echo "$0: unknown host/target combination"; exit 1 ;;
esac

if [ ! -e tcvm ]; then
	echo "Tcode/0 VM (./tcvm) not found"
	exit 1
fi

if [ $h = tcvm ]; then
	echo "building tx-$2.tc"
	ln -fs txemt$eh.t txemit.t
	ln -fs txtcvm.t t3x.t
	ln -fs $dt/cg$t.t cg.t
	./tcvm bin/txtrn0.tc txtrn tx-$2
else
	echo "building tx-$2.tc (as a side effect)"
	ln -fs txemt$eh.t txemit.t
	ln -fs txtcvm.t t3x.t
	ln -fs $dh/cg$h.t cg.t
	./tcvm bin/txtrn0.tc txtrn tx-$2
	if [ $h = fbd386 -o $h = unx386 ]; then
		echo "building tx-$2.s (intermediate)"
	else
		echo "building tx-$2$s"
	fi
	ln -fs txemt$et.t txemit.t
	ln -fs $dt/cg$t.t cg.t
	ln -fs $dh/tx$h.t t3x.t
	./tcvm tx-$2.tc txtrn tx-$2
	if [ $keep = 0 ]; then rm -f tx-$2.tc; fi
fi

if [ $h = fbd386 ]; then
	echo "building tx-$2$s"
	as -32 -o tx-$2.o tx-$2.s
	ld -o tx-$2$s tx-$2.o
	strip tx-$2$s
	if [ $keep = 0 ]; then rm -f tx-$2.s tx-$2.o; fi
fi

if [ $h = unx386 ]; then
	echo "building tx-$2$s"
	cc $CFLAGS -s -o tx-$2$s tx-$2.s targets/ux386lib.c
	if [ $keep = 0 ]; then rm -f tx-$2.s; fi
fi

reset
