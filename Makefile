# T3X/0 Makefile
# Nils M Holm, 2022, 2023, 2024
# Public domain / 0BSD license

V=15

# Flags for compiling T3X/0 assembly output
#
  CFLAGS=
  CFLAGS+=-m32                 # on 64-bit systems
# CFLAGS+=-fPIC                # get rid of stupid linker errors
# CFLAGS+=-Wl,-z,notext        # get rid of stupid linker errors

# Where to keep the T3X libraries
#
T3XDIR=	/usr/local/t3x/0

# Where the binaries go
#
BINDIR=	/u/bin

HOST=	unix

all:	tcvm txtrn.tc

all-tcode:	tcvm txtrn.tc tx-dos.tc tx-cpm.tc

all-native:	tcvm txtrn.tc tx-$(HOST) tx-dos tx-cpm tx-tcvm

# When something goes wrong during cross-compilation, this
# target will reset the code generator and runtime links.
#
reset:
	ln -fs cgtcvm.t cg.t
	ln -fs txtcvm.t t3x.t
	ln -fs txemtbin.t txemit.t

# Regular bootstrap using a pre-compiled Tcode/0 image
#
txtrn.tc:	txtrn.t cg.t txemit.t tcvm bin/txtrn0.tc
	./tcvm bin/txtrn0.tc txtrn

# Alternative bootstrap using an existing T3Xr7 compiler
#
alt: txtrn.t cg.t txemit.t txtrn0
	txx txtrn0 txtrn

txtrn0:	txtrn0.t
	tx txtrn0.t

# The Tcode/0 VM
#
tcvm:	tcvm.c
	cc -O2 -g -o tcvm tcvm.c

# Triple test of the Tcode/0 compiler
#
triple: txtrn.tc
	./tcvm txtrn.tc txtrn txtrn1
	./tcvm txtrn1.tc txtrn txtrn2
	cmp txtrn1.tc txtrn2.tc
	rm -f txtrn1.tc txtrn2.tc

# Run the test suite under the Tcode/0 VM
#
test:	programs/test.t tcvm txtrn.tc
	./tcvm txtrn.tc programs/test test
	./tcvm test.tc

# Native CP/M-Z80 compiler
#
tx-cpm.com:
	bin/build.sh cpm cpm

# Native DOS-8086 compiler
#
tx-dos.com:
	bin/build.sh dos dos

# HOST -> DOS-8086 cross compiler
#
tx-dos:
	bin/build.sh $(HOST) dos

# HOST -> CP/M-Z80 cross compiler
#
tx-cpm:
	bin/build.sh $(HOST) cpm

# HOST -> TCVM cross compiler
#
tx-tcvm:
	bin/build.sh $(HOST) tcvm

# Native HOST compiler
#
tx-$(HOST):
	bin/build.sh $(HOST) $(HOST)

# TCVM -> DOS compiler
#
tx-dos.tc:
	bin/build.sh tcvm dos

# TCVM -> CP/M compiler
#
tx-cpm.tc:
	bin/build.sh tcvm cpm

# Triple test of the Unix-386 compiler
#
unix-triple: tx-unix
	ln -fs txemtsrc.t txemit.t
	ln -fs targets/cgunx386.t cg.t
	ln -fs targets/txunx386.t t3x.t
	./tx-unix txtrn tx-unix2
	cc $(CFLAGS) -s -o tx-unix2 tx-unix2.s targets/ux386lib.c && \
		rm tx-unix2.s
	./tx-unix2 txtrn tx-unix3
	cc $(CFLAGS) -s -o tx-unix3 tx-unix3.s targets/ux386lib.c && \
		rm tx-unix3.s
	make reset
	cmp tx-unix2 tx-unix3 && rm tx-unix2 tx-unix3

# Triple test of the FreeBSD-386 compiler
#
fbsd-triple:
	bin/build.sh fbsd fbsd
	ln -fs txemtsrc.t txemit.t
	ln -fs targets/cgfbd386.t cg.t
	ln -fs targets/txfbd386.t t3x.t
	./tx-fbsd txtrn tx-fbsd2
	as -32 -o tx-fbsd2.o tx-fbsd2.s
	ld -o tx-fbsd2 tx-fbsd2.o
	rm -f tx-fbsd2.[so]
	./tx-fbsd2 txtrn tx-fbsd3
	as -32 -o tx-fbsd3.o tx-fbsd3.s
	ld -o tx-fbsd3 tx-fbsd3.o
	rm -f tx-fbsd3.[so]
	make reset
	cmp tx-fbsd2 tx-fbsd3 && rm tx-fbsd2 tx-fbsd3

# System-wide installation
# Use this with HOST=unix or HOST=fbsd
#
install-native:	all-native tx-dos.tc tx-cpm.tc
	install -d $(T3XDIR)
	install -d $(T3XDIR)/cpmz80
	install -d $(T3XDIR)/dos86c
	install -d $(T3XDIR)/fbd386
	install -d $(T3XDIR)/unx386
	install -c -m 0644 txtrn.tc $(T3XDIR)
	install -c -m 0644 txtcvm.t $(T3XDIR)/t3x.t
	install -c -m 0644 targets/txunx386.t $(T3XDIR)/unx386/t3x.t
	install -c -m 0644 targets/ux386lib.c $(T3XDIR)/unx386
	install -c -m 0644 targets/txfbd386.t $(T3XDIR)/fbd386/t3x.t
	install -c -m 0644 targets/txcpmz80.t $(T3XDIR)/cpmz80/t3x.t
	install -c -m 0644 targets/txdos86c.t $(T3XDIR)/dos86c/t3x.t
	install -c -m 0644 library/*.t $(T3XDIR)
	install -c -m 0644 library/console.cpm $(T3XDIR)/cpmz80/console.t
	install -c -m 0644 library/console.dos $(T3XDIR)/dos86c/console.t
	install -c -m 0644 library/console.unx $(T3XDIR)/unx386/console.t
	install -c -m 0644 library/console.c $(T3XDIR)/unx386/console.c
	install -c tx-$(HOST) $(T3XDIR)
	install -c tx-dos $(T3XDIR)
	install -c tx-cpm $(T3XDIR)
	install -c tx-tcvm $(T3XDIR)
	install -c tx-dos.tc $(T3XDIR)
	install -c tx-cpm.tc $(T3XDIR)
	install -c tcvm $(BINDIR)
	install -c bin/tx0.sh $(BINDIR)/tx0

# System-wide installation
# Use this with HOST=unix to also install the fbsd backend
#
install-fbsd-native:	install-native
	bin/build.sh fbsd fbsd
	install -d $(T3XDIR)
	install -d $(T3XDIR)/fbd386
	install -c -m 0644 targets/txfbd386.t $(T3XDIR)/fbd386/t3x.t
	install -c tx-fbsd $(T3XDIR)

# System-wide minimum (bytecode) installation
# Use this if there is no native code support for your system
#
install-tcode:	all-tcode
	install -d $(T3XDIR)
	install -d $(T3XDIR)/cpmz80
	install -d $(T3XDIR)/dos86c
	install -c -m 0644 txtrn.tc $(T3XDIR)
	install -c -m 0644 txtcvm.t $(T3XDIR)/t3x.t
	install -c -m 0644 targets/txcpmz80.t $(T3XDIR)/cpmz80/t3x.t
	install -c -m 0644 targets/txdos86c.t $(T3XDIR)/dos86c/t3x.t
	install -c -m 0644 library/* $(T3XDIR)
	install -c tx-dos.tc $(T3XDIR)
	install -c tx-cpm.tc $(T3XDIR)
	install -c tcvm $(BINDIR)
	install -c bin/tx0.sh $(BINDIR)/tx0

csums:
	txsum -u <_checksums >_checksums.new
	mv -f _checksums.new _checksums

mksums:	clean
	find . -type f | grep -v t3x0-$V.zip | grep -v _checksums \
		| txsum -m >_checksums

arc:	clean
	(cd ..; zip -9r t3x0-$V.zip t3x0)
	mv ../t3x0-$V.zip .

clean:
	rm -f txtrn0 txtrn1.tc txtrn.tc \
		tx-tcvm.tc tx-tcvm.com tx-tcvm \
		tx-cpm.tc tx-cpm.com tx-cpm \
		tx-dos.tc tx-dos.com tx-dos \
		tx-dos.tc tx-cpm.tc \
		tx-fbsd.tc tx-fbsd \
		tx-unix.tc tx-unix \
		cpmfile.tc dosfile.tc \
		tcvm test.tc *.core core t3x0-$V.zip

