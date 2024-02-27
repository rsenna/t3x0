! T3X/0 bootstrapping compiler in T3Xr7
! Nils M Holm, 2017,2019,2020,2022
! Public Domain / 0BSD license

module txtrn(t3x);

object t[t3x];

const	BPW = 2;

const	GPOOL_SIZE = 7;

const	BUFLEN = 128;

const	SYMTBL_SIZE = 512;
const	LABEL_SIZE = 1536;
const	NLIST_SIZE = 3072;
const	FWDCL_SIZE = 64;

const	TOKLEN = 80;

var	Modname::TOKLEN;

var	Outname::14;

var	Line;

var	Verbose;

const	ENDFILE = %1;
const	EOFCHAR = 0x1a;

var ntoa_buf::100;

ntoa(x) do var i, k;
	if (x = 0) return "0";
	i := 0;
	k := x<0-> -x: x;
	while (k > 0) do
		i := i+1;
		k := k/10;
	end
	i := i+1;
	if (x < 0) i := i+1;
	ntoa_buf::i := 0;
	k := x<0-> -x: x;
	while (k > 0) do
		i := i-1;
		ntoa_buf::i := '0' + k mod 10;
		k := k/10;
	end
	if (x < 0) do
		i := i-1;
		ntoa_buf::i := '-';
	end
	return @ntoa_buf::i;
end

ntox(x) do var i, d;
	d := "0123456789ABCDEF";
	i := 99;
	ntoa_buf::i := 0;
	while (x > 0 \/ i = 99) do
		i := i-1;
		ntoa_buf::i := d::(x mod 16);
		x := x>>4;
	end
	while (i > 99-t.bpw()*2) do
		i := i-1;
		ntoa_buf::i := '0';
	end
	return @ntoa_buf::i;
end

str_length(s) return t.memscan(s, 0, 32767);

str_copy(sd, ss) t.memcopy(sd, ss, str_length(ss)+1);

str_append(sd, ss) t.memcopy(@sd::str_length(sd), ss, str_length(ss)+1);

str_equal(s1, s2) return t.memcomp(s1, s2, str_length(s1)+1) = 0;

writes(s) t.write(1, s, str_length(s));

nl() do var b::3;
	writes(t.newline(b));
end

pad(s, n) do var i, k;
	k := str_length(s);
	for (i=k, n) writes("\s");
end

wrpad(s, n) do
	writes(s);
	pad(s, n);
end

aw(m, s) do
	writes("Error: ");
	if (Modname::0) do
		writes(Modname);
		writes(": ");
	end
	writes(ntoa(Line));
	writes(": ");
	writes(m);
	if (s \= 0) do
		writes(": ");
		writes(s);
	end
	nl();
	if (Outname::0) t.remove(Outname);
	halt 1;
end

oops(m, s) do
	writes("Internal error");
	nl();
	aw(m, s);
end

numeric(c) return '0' <= c /\ c <= '9';

alphabetic(c) return 'a' <= c /\ c <= 'z' \/
		     'A' <= c /\ c <= 'Z';

!
! Symbol tables
!

struct	SYM = SNAME, SFLAGS, SVALUE;
const	ARSHIFT = 10;

const	GLOB = 1;
const	CNST = 2;
const	VECT = 4;
const	FORW = 8;
const	FUNC = 16;
const	MODU = 32;
const	PUBL = 64;
const	ALIA = 128;  
const	EXTN = 256;

var	Syms[SYM*SYMTBL_SIZE];
var	Labels[LABEL_SIZE];
Var	Lab;
var	Nlist::NLIST_SIZE;

var	Yp, Np;

var	Fwlab[FWDCL_SIZE],
	Fwaddr[FWDCL_SIZE];
var	Fwp;

find(s) do var i, inmod, skip;
	skip := Modname::0;
	i := Yp-SYM;
	inmod := 0;
	while (i >= 0) do
		if (Syms[i+SFLAGS] & MODU) do
			if (\skip) inmod := \inmod;
			skip := 0;
		end
		if (\inmod /\ str_equal(Syms[i+SNAME], s))
			return @Syms[i];
		i := i - SYM;
	end
	return 0;
end

findmem(s, m) do var i;
	i := (m - Syms) / t.bpw();
	for (i = i+SYM, Yp, SYM) do
		if (Syms[i+SFLAGS] & MODU) leave;
		if (str_equal(Syms[i+SNAME], s))
			return @Syms[i];
	end
	aw("no such member", s);
end

lookup(s, f) do var y;
	y := find(s);
	if (y = 0) aw("undefined", s);
	if (y[SFLAGS] & f \= f)
		aw("unexpected type", s);
	return y;
end

newname(s) do var k, new;
	k := str_length(s)+1;
	if (Np+k >= NLIST_SIZE)
		aw("name pool overflow", s);
	new := @Nlist::Np;
	t.memcopy(new, s, k);
	Np := Np+k;
	return new;
end

add(s, f, v) do var y;
	y := find(s);
	if (y \= 0) do
		ie (y[SFLAGS] & FORW /\ f & FUNC)
			return y;
		else
			aw("redefined", s);
	end
	if (Yp+SYM >= SYMTBL_SIZE*SYM)
		aw("too many symbols", 0);
	y := @Syms[Yp];
	Yp := Yp+SYM;
	y[SNAME] := newname(s);
	y[SFLAGS] := f;
	y[SVALUE] := v;
	return y;
end

addfwd(l, a) do
	if (Fwp >= FWDCL_SIZE)
		aw("too many forward declarations", 0);
	Fwlab[Fwp] := l;
	Fwaddr[Fwp] := a;
	Fwp := Fwp+1;
end

atomic(y) return y[SFLAGS] & (CNST|FUNC|FORW|EXTN|VECT|MODU) = 0;

prflag(v, f, s) writes(v&f-> s: "-");

dumpsym(y) do var f;
	writes("; ");
	writes(ntox(y));
	writes("\s");
	writes(ntox(y[SFLAGS]));
	writes("\s");
	f := y[SFLAGS];
	prflag(f, EXTN, "X");
	prflag(f, ALIA, "A");
	prflag(f, PUBL, "P");
	prflag(f, MODU, "M");
	prflag(f, FUNC, "F");
	prflag(f, FORW, "W");
	prflag(f, VECT, "V");
	prflag(f, CNST, "C");
	prflag(f, GLOB, "G");
	writes("\s");
	writes(ntox(y[SVALUE]));
	writes("\s");
	writes(y[SNAME]);
	if (f & (FUNC|FORW|EXTN)) do
		writes(" (");
		writes(ntoa(f >> ARSHIFT));
		writes(")");
	end
	nl();
end

dumpsyms(y0, yn) do var i, f;
	if (y0 = yn) return;
	nl();
	writes("; ");
	wrpad("ADDR", 1+t.bpw()*2);
	wrpad("FLGS", 11+t.bpw()*2);
	wrpad("VALU", 1+t.bpw()*2);
	wrpad("NAME", 1+t.bpw()*2);
	nl();
	for (i=y0, yn, SYM) dumpsym(@Syms[i]);
end

!
! Emitter
!

var	Pass;

var	Outfile;
var	Outbuf::BUFLEN;
var	Outp;

var	Gp, Gtop;

var	Tp, Dp, Lp, Ls, Lp0;

var	Acc;

var	Codetbl;

struct	OPT = OINST1, OARG, OINST2, OREPL;

var	Opttbl;

struct	CGOPS =	CG_NULL,
		CG_PUSH, CG_CLEAR, CG_DROP,
		CG_LDVAL, CG_LDADDR, CG_LDLREF, CG_LDGLOB,
		CG_LDLOCL,
		CG_STGLOB, CG_STLOCL, CG_STINDR, CG_STINDB,
		CG_INCGLOB, CG_INCLOCL, CG_INCR,
		CG_STACK, CG_UNSTACK, CG_LOCLVEC, CG_GLOBVEC,
		CG_INDEX, CG_DEREF, CG_INDXB, CG_DREFB,
		CG_CALL, CG_CALR, CG_JUMP, CG_RJUMP,
		CG_JMPFALSE, CG_JMPTRUE, CG_FOR, CG_FORDOWN,
		CG_MKFRAME, CG_DELFRAME, CG_RET, CG_HALT,
		CG_NEG, CG_INV, CG_LOGNOT, CG_ADD, CG_SUB,
		CG_MUL, CG_DIV, CG_MOD, CG_AND, CG_OR, CG_XOR,
		CG_SHL, CG_SHR, CG_EQ, CG_NE, CG_LT, CG_GT,
		CG_LE, CG_GE, CG_UMUL, CG_UDIV, CG_ULT, CG_UGT,
		CG_ULE, CG_UGE, CG_JMPEQ, CG_JMPNE, CG_JMPLT,
		CG_JMPGT, CG_JMPLE, CG_JMPGE;

findlab(id) return Labels[id];

newlab() do
	if (Lab >= LABEL_SIZE) aw("too many labels", 0);
	Lab := Lab+1;
	return Lab;
end

decl	commit(0);

resolve(id) do
	commit();
	Labels[id] := Tp;
end

resolve_fwd(a) do var i;
	i := 0;
	while (i < Fwp) do
		if (Fwaddr[i] = a) do
			resolve(Fwlab[i]);
			return;
		end
		i := i+1;
	end
	oops("unknown forward reference", 0);
end

flush() do
	if (\Outp) return;
	if (t.write(Outfile, Outbuf, Outp) \= Outp)
		aw("file write error", 0);
	Outp := 0;
end

emit(x) do
	Tp := Tp+1;
	if (Pass = 0) return;
	if (Outp >= BUFLEN) flush();
	Outbuf::Outp := x;
	Outp := Outp + 1;
end

emitw(x) do
	emit(255 & x);
	emit(255 & (x>>8));
end

hex(c)	ie (numeric(c))
		return c-'0';
	else
		return c-'a'+10;

byte(s) return 16*hex(s::0) + hex(s::1);

rgen(s, v) do var n;
	while (s::0) do
		ie (s::0 = ',') do
			ie (s::1 = 'w')
				emitw(v);
			else ie (s::1 = 'l')
				emit(v);
			else ie (s::1 = 'h')
				emit(v+1);
			else ie (s::1 = 'r')
				emit(v-Tp-1);
			else ie (s::1 = 'R') do
				n := byte(s+4) << 8 | byte(s+2);
				emitw(n-Tp-2);
				s := s+4;
			end
			else ie (s::1 = 'b')
				emit(v);
			else
				oops("bad code", 0);
		end
		else do
			emit(byte(s));
		end
		s := s+2;
	end
end

var	Qi, Qa;

commit() do
	rgen(Codetbl[Qi][1], Qa);
	Qi := CG_NULL;
end

gen(id, a) do var i, skiparg;
	skiparg := %1;
	i := 0;
	while (Opttbl[i] \= %1) do
		ie (Opttbl[i][OINST1] = %1)
			skiparg := 0;
		else if (Qi = Opttbl[i][OINST1] /\
			 id = Opttbl[i][OINST2] /\
			 (skiparg \/ Qa = Opttbl[i][OARG]))
		do
			Qi := Opttbl[i][OREPL];
			Qa := a;
			return;
		end
		i := i+1;
	end
	if (Qi \= CG_NULL) commit();
	Qi := id;
	Qa := a;
end

spill() ie (Acc)
		gen(CG_PUSH, 0);
	else
		Acc := 1;

active() return Acc;

clear() Acc := 0;

activate() Acc := 1;

globaddr() do var l, i, g;
	if (Gp >= Gtop) do
		l := newlab();
		gen(CG_RJUMP, findlab(l));
		commit();
		Gp := Tp;
		for (i=0, GPOOL_SIZE) emitw(0);
		Gtop := Tp;
		resolve(l);
	end
	g := Gp;
	Gp := Gp+2;
	return g;
end

align(x, a) return (x+a) & ~(a-1);

!
! Scanner
!

const	META	 = 256;

var	Infile;
var	Inbuf::BUFLEN;
var	Ip, Ep;
var	Rejected;
var	Tk;
var	Str::TOKLEN;
var	Val;
var	Oid;

var	Equal_op, Minus_op, Mul_op, Add_op;

struct	OPER = OPREC, OLEN, ONAME, OTOK, OCODE;

var	Ops;

struct	TOKENS =
	SYMBOL, INTEGER, STRING,
	ADDROF, ASSIGN, BINOP, BYTEOP, COLON, COMMA, COND,
	CONJ, DISJ, DOT, LBRACK, LPAREN, RBRACK, RPAREN, SEMI, UNOP,
	KCALL, KCONST, KDECL, KDO, KELSE, KEND, KEXTERN, KFOR,
	KHALT, KIE, KIF, KINLINE, KLEAVE, KLOOP, KMODULE, KOBJECT,
	KPACKED, KPUBLIC, KRETURN, KSTRUCT, KUSE, KVAR, KWHILE;

readrc() do var c;
	if (Rejected) do
		c := Rejected;
		Rejected := 0;
		return c;
	end
	if (Ip >= Ep) do
		Ep := t.read(Infile, Inbuf, BUFLEN);
		Ip := 0;
	end
	if (Ip >= Ep) return ENDFILE;
	c := Inbuf::Ip;
	Ip := Ip+1;
	return c;
end

readc() do var c;
	c := readrc();
	return 'A' <= c /\ c <= 'Z'-> c-'A'+'a': c;
end

readec() do var c;
	c := readrc();
	if (c \= '\\') return c;
	c := readrc();
	if (c = 'a') return '\a';
	if (c = 'b') return '\b';
	if (c = 'e') return '\e';
	if (c = 'f') return '\f';
	if (c = 'n') return '\n';
	if (c = 'q') return '"' | META;
	if (c = 'r') return '\r';
	if (c = 's') return '\s';
	if (c = 't') return '\t';
	if (c = 'v') return '\v';
	return c;
end

reject(c) Rejected := c;

skip() do var c;
	c := readc();
	while (1) do
		while (c = ' ' \/ c = '\t' \/ c = '\n' \/ c = '\r') do
			if (c = '\n') Line := Line+1;
			c := readc();
		end
		if (c \= '!')
			return c;
		while (c \= '\n' /\ c \= ENDFILE)
			c := readc();
	end
end

findkw(s) do
	if (s::0 = 'c') do
		if (str_equal(s, "call")) return KCALL;
		if (str_equal(s, "const")) return KCONST;
		return 0;
	end
	if (s::0 = 'd') do
		if (str_equal(s, "do")) return KDO;
		if (str_equal(s, "decl")) return KDECL;
		return 0;
	end
	if (s::0 = 'e') do
		if (str_equal(s, "else")) return KELSE;
		if (str_equal(s, "end")) return KEND;
		if (str_equal(s, "extern")) return KEXTERN;
		return 0;
	end
	if (s::0 = 'f') do
		if (str_equal(s, "for")) return KFOR;
		return 0;
	end
	if (s::0 = 'h') do
		if (str_equal(s, "halt")) return KHALT;
		return 0;
	end
	if (s::0 = 'i') do
		if (str_equal(s, "if")) return KIF;
		if (str_equal(s, "ie")) return KIE;
		if (str_equal(s, "inline")) return KINLINE;
		return 0;
	end
	if (s::0 = 'l') do
		if (str_equal(s, "leave")) return KLEAVE;
		if (str_equal(s, "loop")) return KLOOP;
		return 0;
	end
	if (s::0 = 'm') do
		if (str_equal(s, "mod")) return BINOP;
		if (str_equal(s, "module")) return KMODULE;
		return 0;
	end
	if (s::0 = 'o') do
		if (str_equal(s, "object")) return KOBJECT;
		return 0;
	end
	if (s::0 = 'p') do
		if (str_equal(s, "packed")) return KPACKED;
		if (str_equal(s, "public")) return KPUBLIC;
		return 0;
	end
	if (s::0 = 'r') do
		if (str_equal(s, "return")) return KRETURN;
		return 0;
	end
	if (s::0 = 's') do
		if (str_equal(s, "struct")) return KSTRUCT;
		return 0;
	end
	if (s::0 = 'u') do
		if (str_equal(s, "use")) return KUSE;
		return 0;
	end
	if (s::0 = 'v') do
		if (str_equal(s, "var")) return KVAR;
		return 0;
	end
	if (s::0 = 'w') do
		if (str_equal(s, "while")) return KWHILE;
		return 0;
	end
	return 0;
end

scanop(c) do var i, j;
	i := 0;
	j := 0;
	Oid := %1;
	while (Ops[i][OLEN] > 0) do
		ie (Ops[i][OLEN] > j) do
			if (Ops[i][ONAME]::j = c) do
				Oid := i;
				Str::j := c;
				c := readc();
				j := j+1;
			end
		end
		else do
			leave;
		end
		i := i+1;
	end
	if (Oid = %1) do
		Str::j := c;
		j := j+1;
		Str::j := 0;
		aw("unknown operator", Str);
	end
	Str::j := 0;
	reject(c);
	return Ops[Oid][OTOK];
end

findop(s) do var i;
	i := 0;
	while (Ops[i][OLEN] > 0) do
		if (str_equal(s, Ops[i][ONAME])) do
			Oid := i;
			return Oid;
		end
		i := i+1;
	end
	oops("operator not found", s);
end

symbolic(c) return alphabetic(c) \/ c = '_';

scan() do var c, i, k, sgn, base;
	c := skip();
	if (c = ENDFILE \/ c = EOFCHAR) do
		str_copy(Str, "end of file");
		return ENDFILE;
	end
	if (symbolic(c)) do
		i := 0;
		while (symbolic(c) \/ numeric(c)) do
			if (i >= TOKLEN-1) do
				Str::i := 0;
				aw("symbol too long", Str);
			end
			Str::i := c;
			i := i+1;
			c := readc();
		end
		Str::i := 0;
		reject(c);
		k := findkw(Str);
		if (k \= 0) do
			if (k = BINOP) findop(Str);
			return k;
		end
		return SYMBOL;
	end
	if (numeric(c) \/ c = '%') do
		sgn := 1;
		i := 0;
		if (c = '%') do
			sgn := %1;
			c := readc();
			Str::i := c;
			i := i+1;
			if (\numeric(c))
				aw("missing digits after '%'", 0);
		end
		base := 10;
		if (c = '0') do
			c := readc();
			if (c = 'x') do
				base := 16;
				c := readc();
				if (\numeric(c) /\ (c < 'a' \/ c > 'f'))
					aw("missing digits after '0x'", 0);
			end
		end
		Val := 0;
		while (	numeric(c) \/
			base = 16 /\ 'a' <= c /\ c <= 'f'
		) do
			if (i >= TOKLEN-1) do
				Str::i := 0;
				aw("integer too long", Str);
			end
			Str::i := c;
			i := i+1;
			c := c >= 'a'-> c-'a'+10: c-'0';
			Val := Val * base + c;
			c := readc();
		end
		Str::i := 0;
		reject(c);
		Val := Val * sgn;
		return INTEGER;
	end
	if (c = '\'') do
		Val := readec();
		if (readc() \= '\'')
			aw("missing ''' in character", 0);
		return INTEGER;
	end
	if (c = '"') do
		i := 0;
		c := readec();
		while (c \= '"' /\ c \= ENDFILE) do
			if (i >= TOKLEN-1) do
				Str::i := 0;
				aw("string too long", Str);
			end
			Str::i := c & (META-1);
			i := i+1;
			c := readec();
		end
		Str::i := 0;
		return STRING;
	end
	return scanop(c);
end

!
! Parser
!

const	MAXTBL	 = 128;
const	MAXLOOP	 = 100;

var	Retlab;
var	Frame;
var	Loop0;
var	Leaves[MAXLOOP], Lvp;
var	Loops[MAXLOOP], Llp;

expect(tok, s) do var b::100;
	if (tok = Tk) return;
	str_copy(b, s);
	str_append(b, " expected");
	aw(b, Str);
end

xeqsign() do
	if (Tk \= BINOP \/ Oid \= Equal_op)
		expect(BINOP, "'='");
	Tk := scan();
end

xsemi() do
	expect(SEMI, "';'");
	Tk := scan();
end

xlparen() do
	expect(LPAREN, "'('");
	Tk := scan();
end

xrparen() do
	expect(RPAREN, "')'");
	Tk := scan();
end

xsymbol() expect(SYMBOL, "symbol");

member(y, b) do
	Tk := scan();
	if (y[SFLAGS] & ALIA) y := y[SVALUE];
	if (y[SFLAGS] & MODU = 0)
		aw("module name expected", b[SNAME]);
	y := findmem(Str, y);
	Tk := scan();
	return y;
end

constfac() do var v, y, b;
	if (Tk = INTEGER) do
		v := Val;
		Tk := scan();
		return v;
	end
	if (Tk = SYMBOL) do
		y := lookup(Str, 0);
		b := y;
		Tk := scan();
		if (Tk = DOT) y := member(y, b);
		if (y[SFLAGS] & CNST = 0)
			aw("constant expected", Str);
		return y[SVALUE];
	end
	aw("constant value expected", Str);
end

constval() do var v;
	v := constfac();
	ie (Tk = BINOP /\ Oid = Mul_op) do
		Tk := scan();
		v := v * constfac();
	end
	else if (Tk = BINOP /\ Oid = Add_op) do
		Tk := scan();
		v := v + constfac();
	end
	return v;
end

checklocal(y)
	if (y[SVALUE] > 126 \/ y[SVALUE] < -126)
		aw("local storage exceeded", y[SNAME]);

vardecl(glb) do var y, size, a;
	Tk := scan();
	while (1) do
		xsymbol();
		ie (glb & GLOB) do
			a := globaddr();
			y := add(Str, glb, a);
		end
		else do
			y := add(Str, 0, Lp);
		end
		Tk := scan();
		size := 1;
		ie (Tk = LBRACK) do
			Tk := scan();
			size := constval();
			if (size < 1)
				aw("invalid size", 0);
			y[SFLAGS] := y[SFLAGS] | VECT;
			expect(RBRACK, "']'");
			Tk := scan();
		end
		else if (Tk = BYTEOP) do
			Tk := scan();
			size := constval();
			if (size < 1)
				aw("invalid size", 0);
			size := (size + BPW-1) / BPW;
			y[SFLAGS] := y[SFLAGS] | VECT;
		end
		ie (glb & GLOB) do
			if (y[SFLAGS] & VECT) do
				gen(CG_STACK, -(size*BPW));
				Dp := Dp + size*BPW;
				gen(CG_GLOBVEC, a);
			end
		end
		else do
			ie (y[SFLAGS] & VECT) do
				gen(CG_STACK, -((Ls+size)*BPW));
				Lp := Lp - size*BPW;
				Ls := 0;
				gen(CG_LOCLVEC, 0);
			end
			else do
				Ls := Ls + 1;
			end
			Lp := Lp - BPW;
			y[SVALUE] := Lp;
			! checklocal(y);
		end
		if (Tk \= COMMA) leave;
		Tk := scan();
	end
	xsemi();
end

constdecl(flg) do var y;
	Tk := scan();
	while (1) do
		xsymbol();
		y := add(Str, flg|CNST, 0);
		Tk := scan();
		xeqsign();
		y[SVALUE] := constval();
		if (Tk \= COMMA) leave;
		Tk := scan();
	end
	xsemi();
end

stcdecl(flg) do var y, i;
	Tk := scan();
	xsymbol();
	y := add(Str, flg|CNST, 0);
	Tk := scan();
	xeqsign();
	i := 0;
	while (1) do
		xsymbol();
		add(Str, flg|CNST, i);
		i := i+1;
		Tk := scan();
		if (Tk \= COMMA) leave;
		Tk := scan();
	end
	y[SVALUE] := i;
	xsemi();
end

inline_decl(flg) do var k, y, n, l1, l2;
	Tk := scan();
	l1 := newlab();
	gen(CG_JUMP, findlab(l1));
	while (1) do
		xsymbol();
		commit();
		y := add(Str, GLOB|FUNC|flg, Tp);
		Tk := scan();
		xlparen();
		n := constval();
		if (n < 0) aw("invalid arity", 0);
		y[SFLAGS] := y[SFLAGS] | (n << ARSHIFT);
		xrparen();
		xeqsign();
		expect(LBRACK, "'['");
		Tk := scan();
		while (1) do
			k := constval();
			if (k < 0 \/ k > 255)
				aw("inline code out of range", Str);
			emit(k);
			if (Tk \= COMMA) leave;
			Tk := scan();
		end
		expect(RBRACK, "']'");
		Tk := scan();
		if (Tk \= COMMA) leave;
		Tk := scan();
	end
	resolve(l1);
	xsemi();
end

f_e_decl(flg) do var y, n, l1, l2, ext;
	Tk := scan();
	l1 := newlab();
	ext := flg & EXTN-> %1: 0;
	if (\ext) gen(CG_JUMP, findlab(l1));
	while (1) do
		xsymbol();
		l2 := newlab();
		commit();
		addfwd(l2, Tp);
		y := add(Str, GLOB|flg, Tp);
		if (\ext) gen(CG_JUMP, findlab(l2));
		Tk := scan();
		xlparen();
		n := constval();
		if (n < 0) aw("invalid arity", 0);
		y[SFLAGS] := y[SFLAGS] | (n << ARSHIFT);
		xrparen();
		if (Tk \= COMMA) leave;
		Tk := scan();
	end
	if (\ext) resolve(l1);
	xsemi();
end

decl	stmt(1);

fundecl(f) do
	var	l_base, l_addr;
	var	i, na, oyp, onp;
	var	y, l;

	if (f = PUBL) Tk := scan();
	if (Verbose) do
		writes(Str);
		nl();
	end
	l_addr := 2*BPW;
	na := 0;
	l := newlab();
	gen(CG_JUMP, findlab(l));
	commit();
	y := add(Str, GLOB|FUNC|f, Tp);
	Tk := scan();
	oyp := Yp;
	onp := Np;
	l_base := Yp;
	xlparen();
	while (Tk = SYMBOL) do
		add(Str, 0, l_addr);
		l_addr := l_addr + BPW;
		na := na+1;
		Tk := scan();
		if (Tk \= COMMA) leave;
		Tk := scan();
	end
	xrparen();
	for (i = l_base, Yp, SYM) do
		Syms[i+SVALUE] := 6+na*BPW - Syms[i+SVALUE];
	end
	if (y[SFLAGS] & FORW) do
		if (na \= y[SFLAGS] >> ARSHIFT)
			aw("function does not match DECL", y[SNAME]);
		y[SFLAGS] := y[SFLAGS] & ~FORW | FUNC | f;
		resolve_fwd(y[SVALUE]);
		y[SVALUE] := Tp;
	end
	y[SFLAGS] := y[SFLAGS] | (na << ARSHIFT);
	if (na) gen(CG_MKFRAME, 0);
	Frame := na;
	Retlab := newlab();
	stmt(1);
	if (Retlab) resolve(Retlab);
	Retlab := 0;
	if (Frame) gen(CG_DELFRAME, 0);
	Frame := 0;
	gen(CG_RET, 0);
	resolve(l);
	Yp := oyp;
	Np := onp;
	Lp := 0;
end

pubdecl() do
	if (Modname::0 = 0) aw("'public' declaration not in module", 0);
	Tk := scan();
	ie (Tk = KCONST)
		constdecl(GLOB|PUBL);
	else ie (Tk = KSTRUCT)
		stcdecl(GLOB|PUBL);
	else ie (Tk = KEXTERN)
		f_e_decl(EXTN|PUBL);
	else ie (Tk = KINLINE)
		inline_decl(PUBL);
	else
		fundecl(GLOB|PUBL);
end

decl declaration(1);

! XXX also clean up name pool
modcleanup(m) do var i, j;
	i := m+SYM;
	j := i;
	for (i=i, Yp, SYM) do
		if (Syms[i+SFLAGS] & PUBL) do
			if (i \= j)
				t.memcopy(@Syms[j], @Syms[i], SYM*t.bpw());
			j := j+SYM;
		end
	end
	Yp := j;
end

decl compound(1);

moddecl() do var m;
	Tk := scan();
	xsymbol();
	m := Yp;
	str_copy(Modname, Str);
	add(Str, GLOB|MODU|PUBL, 0);
	Tk := scan();
	xsemi();
	while (	Tk = KVAR \/ Tk = KCONST \/ Tk = SYMBOL \/
		Tk = KDECL \/ Tk = KSTRUCT \/ Tk = KPUBLIC \/
		Tk = KEXTERN \/ Tk = KINLINE
	)
		declaration(GLOB);
	if (Tk = KDO) compound(0);
	if (Tk \= KEND)
		aw("unexpected end of module", Str);
	Tk := scan();
	add("*", GLOB|MODU|PUBL, 0);
	Modname::0 := 0;
	modcleanup(m);
end

var	Mpath::TOKLEN;

loadmodule(s) do
	var	name::TOKLEN;
	var	obuf::BUFLEN;
	var	k, in, oin, oip, oep, oln, m;

	m := @Syms[Yp];
	str_copy(name, s);
	k := str_length(name);
	if (k >= TOKLEN-10)
		aw("module name too long", name);
	str_copy(@name::k, ".t");
	in := t.open(name, T3X.OREAD);
	if (in < 0) do
		str_copy(Mpath, "library/");
		str_copy(@Mpath::8, name);
		in := t.open(Mpath, T3X.OREAD);
		if (in < 0) aw("module not found", name);
	end
	oin := Infile;
	t.memcopy(obuf, Inbuf, Ep);
	oip := Ip;
	oep := Ep;
	oln := Line;
	Infile := in;
	Ip := 0;
	Ep := 0;
	Line := 1;
	Tk := scan();
	moddecl();
	t.close(in);
	Infile := oin;
	t.memcopy(Inbuf, obuf, oep);
	Ip := oip;
	Ep := oep;
	Line := oln;
	Rejected := 0;
	return m;
end

usemod() do var name::TOKLEN, alias::TOKLEN, m, a;
	Tk := scan();
	xsymbol();
	str_copy(name, Str);
	Tk := scan();
	m := find(name);
	if (m /\ m[SFLAGS] & MODU = 0)
		aw("not a module", name);
	a := 0;
	if (Tk = COLON) do
		Tk := scan();
		xsymbol();
		str_copy(alias, Str);
		a := 1;
		Tk := scan();
	end
	if (Tk \= SEMI) xsemi();
	if (m = 0) m := loadmodule(name);
	if (a) add(alias, GLOB|ALIA, m);
	Tk := scan();
end

declaration(glb)
	ie (Tk = KVAR)
		vardecl(glb);
	else ie (Tk = KCONST)
		constdecl(glb);
	else ie (Tk = KSTRUCT)
		stcdecl(glb);
	else ie (Tk = KDECL)
		f_e_decl(FORW);
	else ie (Tk = KEXTERN)
		f_e_decl(EXTN);
	else ie (Tk = KINLINE)
		inline_decl(0);
	else ie (Tk = KMODULE)
		moddecl();
	else ie (Tk = KUSE)
		usemod();
	else ie (Tk = KPUBLIC)
		pubdecl();
	else
		fundecl(0);

decl	expr(1);

load(y) ie (y[SFLAGS] & GLOB)
		gen(CG_LDGLOB, y[SVALUE]);
	else
		gen(CG_LDLOCL, y[SVALUE]);

store(y)
	ie (y[SFLAGS] & GLOB)
		gen(CG_STGLOB, y[SVALUE]);
	else
		gen(CG_STLOCL, y[SVALUE]);

fncall(fn, ind) do var i , msg;
	msg := "call of non-function";
	Tk := scan();
	if (fn = 0) aw(msg, 0);
	if (\ind /\ fn[SFLAGS] & (FUNC|FORW|EXTN) = 0) aw(msg, fn[SNAME]);
	i := 0;
	while (Tk \= RPAREN) do
		expr(0);
		i := i+1;
		if (Tk \= COMMA) leave;
		Tk := scan();
		if (Tk = RPAREN)
			aw("syntax error", Str);
	end
	if (\ind /\ i \= fn[SFLAGS] >> ARSHIFT)
		aw("wrong number of arguments", fn[SNAME]);
	expect(RPAREN, "')'");
	Tk := scan();
	if (active()) spill();
	ie (fn[SFLAGS] & (FUNC|FORW))
		gen(CG_CALL, fn[SVALUE]);
	else do
		load(fn);
		gen(CG_CALR, 0);
	end
	if (i \= 0) gen(CG_UNSTACK, i*BPW);
	activate();
end

mkstring(s) do var i, a, k, l;
	k := str_length(s);
	l := newlab();
	gen(CG_JUMP, findlab(l));
	commit();
	a := Tp;
	for (i=0, k+1) emit(s::i);
	resolve(l);
	return a;
end

mkpackvec() do var a, l, k, j, m;
	Tk := scan();
	expect(LBRACK, "'['");
	Tk := scan();
	l := newlab();
	gen(CG_JUMP, findlab(l));
	commit();
	a := Tp;
	while (1) do
		ie (Tk = STRING) do
			m := str_length(Str);
			for (j=0, m) emit(Str::j);
			Tk := scan();
		end
		else do
			k := constval();
			if (k > 255 \/ k < 0)
				aw("byte vector member out of range", Str);
			emit(k);
		end
		if (Tk \= COMMA) leave;
		Tk := scan();
	end
	expect(RBRACK, "']'");
	Tk := scan();
	resolve(l);
	return a;
end

var	gtbl[MAXTBL*3], gaf[MAXTBL*3];

mktable2(depth) do
	var	n, i, a, l, y;
	var	tbl, af;
	var	dynamic;

	if (depth > 2) aw("table nesting too deep", 0);
	tbl := @gtbl[depth*128];
	af := @gaf[depth*128];
	Tk := scan();
	dynamic := 0;
	n := 0;
	while (Tk \= RBRACK) do
		if (n >= MAXTBL)
			aw("table too big", 0);
		ie (Tk = LPAREN /\ \dynamic) do
			Tk := scan();
			dynamic := 1;
			loop;
		end
		else ie (dynamic) do
			expr(1);
			l := newlab();
			gen(CG_STGLOB, findlab(l));
			tbl[n] := 0;
			af[n] := l;
			if (Tk = RPAREN) do
				Tk := scan();
				dynamic := 0;
			end
		end
		else ie (Tk = INTEGER \/ Tk = SYMBOL) do
			tbl[n] := constval();
			af[n] := 0;
		end
		else ie (Tk = STRING) do
			tbl[n] := mkstring(Str);
			af[n] := 0;
			Tk := scan();
		end
		else ie (Tk = LBRACK) do
			tbl[n] := mktable2(depth+1);
			af[n] := 0;
		end
		else ie (Tk = KPACKED) do
			tbl[n] := mkpackvec();
			af[n] := 0;
		end
		else ie (Tk = ADDROF) do
			Tk := scan();
			xsymbol();
			y := lookup(Str, FUNC);
			tbl[n] := y[SVALUE];
			af[n] := 0;
			Tk := scan();
		end
		else do
			aw("invalid table element", Str);
		end
		n := n+1;
		if (Tk \= COMMA) leave;
		Tk := scan();
		if (Tk = RBRACK)
			aw("syntax error", Str);
	end
	if (dynamic)
		aw("missing ')' in dynamic table", 0);
	expect(RBRACK, "']'");
	if (n = 0) aw("empty table", 0);
	Tk := scan();
	l := newlab();
	gen(CG_JUMP, findlab(l));
	commit();
	a := Tp;
	for (i=0, n) do
		if (af[i]) resolve(af[i]);
		emitw(tbl[i]);
	end
	resolve(l);
	return a;
end

mktable() return mktable2(0);

decl	factor(0);

address(lv, bp) do var b, y;
	y := lookup(Str, 0);
	Tk := scan();
	b := y;
	if (Tk = DOT) y := member(y, b);
	if (y[SFLAGS] & (MODU|ALIA))
		aw("unexpected module name", y[SNAME]);
	ie (y[SFLAGS] & CNST) do
		if (lv > 0) aw("invalid location", y[SNAME]);
		spill();
		gen(CG_LDVAL, y[SVALUE]);
	end
	else ie (y[SFLAGS] & (FUNC|FORW)) do
		! Don't load
	end
	else if (lv = 0 \/ Tk = LBRACK \/ Tk = BYTEOP) do
		spill();
		load(y);
	end
	if (Tk = LBRACK \/ Tk = BYTEOP)
		if (y[SFLAGS] & (FUNC|FORW|EXTN|CNST))
			aw("bad subscript", y[SNAME]);
	while (Tk = LBRACK) do
		Tk := scan();
		bp[0] := 0;
		expr(0);
		expect(RBRACK, "']'");
		Tk := scan();
		y := 0;
		gen(CG_INDEX, 0);
		if (lv = 0 \/ Tk = LBRACK  \/ Tk = BYTEOP)
			gen(CG_DEREF, 0);
	end
	if (Tk = BYTEOP) do
		Tk := scan();
		bp[0] := 1;
		factor();
		y := 0;
		gen(CG_INDXB, 0);
		if (lv = 0) gen(CG_DREFB, 0);
	end
	return y;
end

factor() do var y, op, b;
	ie (Tk = INTEGER) do
		spill();
		gen(CG_LDVAL, Val);
		Tk := scan();
	end
	else ie (Tk = SYMBOL) do
		y := address(0, @b);
		if (Tk = LPAREN) fncall(y, 0);
	end
	else ie (Tk = STRING) do
		spill();
		gen(CG_LDADDR, mkstring(Str));
		Tk := scan();
	end
	else ie (Tk = LBRACK) do
		spill();
		gen(CG_LDADDR, mktable());
	end
	else ie (Tk = KPACKED) do
		spill();
		gen(CG_LDADDR, mkpackvec());
	end
	else ie (Tk = ADDROF) do
		Tk := scan();
		y := address(2, @b);
		ie (y = 0) do
			;
		end
		else ie (y[SFLAGS] & GLOB) do
			spill();
			gen(CG_LDADDR, y[SVALUE]);
		end
		else do
			spill();
			gen(CG_LDLREF, y[SVALUE]);
		end
	end
	else ie (Tk = BINOP) do
		if (Oid \= Minus_op)
			aw("syntax error", Str);
		Tk := scan();
		factor();
		gen(CG_NEG, 0);
	end
	else ie (Tk = UNOP) do
		op := Oid;
		Tk := scan();
		factor();
		gen(Ops[op][OCODE], 0);
	end
	else ie (Tk = LPAREN) do
		Tk := scan();
		expr(0);
		xrparen();
	end
	else ie (Tk = KCALL) do
		Tk := scan();
		xsymbol();
		y := lookup(Str, 0);
		Tk := scan();
		if (Tk \= LPAREN) aw("incomplete CALL", 0);
		fncall(y, 1);
	end
	else do
		aw("syntax error", Str);
	end
end

emitop(stk, p) do
	gen(Ops[stk[p-1]][OCODE], 0);
	return p-1;
end

arith() do var stk[10], p;
	factor();
	p := 0;
	while (Tk = BINOP) do
		while (p /\ Ops[Oid][OPREC] <= Ops[stk[p-1]][OPREC])
			p := emitop(stk, p);
		stk[p] := Oid;
		p := p+1;
		Tk := scan();
		factor();
	end
	while (p > 0)
		p := emitop(stk, p);
end

logop(conop) do var l;
	ie (conop)
		arith();
	else
		logop(%1);
	l := 0;
	while (Tk = (conop-> CONJ: DISJ)) do
		Tk := scan();
		if (\l) l := newlab();
		commit();
		gen(conop-> CG_JMPFALSE: CG_JMPTRUE, findlab(l));
		clear();
		ie (conop)
			arith();
		else
			logop(%1);
	end
	if (l) resolve(l);
end

expr(clr) do var l1, l2;
	if (clr) clear();
	logop(0);
	if (Tk = COND) do
		Tk := scan();
		l1 := newlab();
		l2 := newlab();
		gen(CG_JMPFALSE, findlab(l1));
		expr(1);
		expect(COLON, "':'");
		Tk := scan();
		gen(CG_JUMP, findlab(l2));
		resolve(l1);
		expr(1);
		resolve(l2);
	end
end

halt_stmt() do var r;
	Tk := scan();
	r := Tk = SEMI-> 0: constval();
	gen(CG_HALT, r);
	xsemi();
end

return_stmt() do
	Tk := scan();
	if (Retlab = 0)
		aw("cannot return from main body", 0);
	ie (Tk = SEMI)
		gen(CG_CLEAR, 0);
	else
		expr(1);
	ie (Frame /\ Lp /\ Lp0 = Lp) do
		gen(CG_JUMP, findlab(Retlab));
	end
	else do
		if (Lp \= 0) gen(CG_UNSTACK, -Lp);
		if (Frame) gen(CG_DELFRAME, 0);
		gen(CG_RET, 0);
	end
	xsemi();
end

if_stmt(alt) do var l1, l2;
	Tk := scan();
	xlparen();
	expr(1);
	l1 := newlab();
	gen(CG_JMPFALSE, findlab(l1));
	xrparen();
	stmt(0);
	if (alt) do
		l2 := newlab();
		gen(CG_JUMP, findlab(l2));
		resolve(l1);
		l1 := l2;
		expect(KELSE, "ELSE");
		Tk := scan();
		stmt(0);
	end
	resolve(l1);
end

while_stmt() do var olp, olv, l, a0;
	Tk := scan();
	commit();
	olp := Loop0;
	olv := Lvp;
	a0 := Tp;
	Loop0 := Tp;
	xlparen();
	expr(1);
	xrparen();
	l := newlab();
	gen(CG_JMPFALSE, findlab(l));
	stmt(0);
	gen(CG_JUMP, a0);
	resolve(l);
	while (Lvp > olv) do
		resolve(Leaves[Lvp-1]);
		Lvp := Lvp-1;
	end
	Loop0 := olp;
end

for_stmt() do
	var	y, l, a0;
	var	step;
	var	oll, olp, olv;
	var	test;

	Tk := scan();
	oll := Llp;
	olv := Lvp;
	olp := Loop0;
	Loop0 := 0;
	xlparen();
	xsymbol();
	y := lookup(Str, 0);
	if (\atomic(y)) aw("unexpected type", y[SNAME]);
	Tk := scan();
	xeqsign();
	expr(1);
	store(y);
	expect(COMMA, "','");
	Tk := scan();
	commit();
	a0 := Tp;
	test := Tp;
	load(y);
	expr(0);
	ie (Tk = COMMA) do
		Tk := scan();
		step := constval();
	end
	else do
		step := 1;
	end
	l := newlab();
	gen(step<0-> CG_FORDOWN: CG_FOR, findlab(l));
	xrparen();
	stmt(0);
	while (Llp > oll) do
		resolve(Loops[Llp-1]);
		Llp := Llp-1;
	end
	ie (y[SFLAGS] & GLOB) do
		ie (step = 1) do
			gen(CG_INCGLOB, y[SVALUE]);
		end
		else do
			gen(CG_LDGLOB, y[SVALUE]);
			gen(CG_INCR, step);
			gen(CG_STGLOB, y[SVALUE]);
		end
	end
	else do
		ie (step = 1) do
			gen(CG_INCLOCL, y[SVALUE]);
		end
		else do
			gen(CG_LDLOCL, y[SVALUE]);
			gen(CG_INCR, step);
			gen(CG_STLOCL, y[SVALUE]);
		end
	end
	gen(CG_JUMP, a0);
	resolve(l);
	while (Lvp > olv) do
		resolve(Leaves[Lvp-1]);
		Lvp := Lvp-1;
	end
	Loop0 := olp;
end

leave_stmt() do var l;
	Tk := scan();
	if (Loop0 < 0)
		aw("LEAVE not in loop context", 0);
	xsemi();
	if (Lvp >= MAXLOOP)
		aw("too many LEAVEs", 0);
	l := newlab();
	Leaves[Lvp] := l;
	gen(CG_JUMP, findlab(l));
	Lvp := Lvp+1;
end

loop_stmt() do var l;
	Tk := scan();
	if (Loop0 < 0)
		aw("LOOP not in loop context", 0);
	xsemi();
	ie (Loop0 > 0) do
		gen(CG_JUMP, Loop0);
	end
	else do
		if (Llp >= MAXLOOP)
			aw("too many LOOPs", 0);
		l := newlab();
		Loops[Llp] := l;
		gen(CG_JUMP, findlab(l));
		Llp := Llp+1;
	end
end

asg_or_call() do var y, b;
	clear();
	y := address(1, @b);
	ie (Tk = LPAREN) do
		fncall(y, 0);
	end
	else ie (Tk = ASSIGN) do
		Tk := scan();
		expr(0);
		ie (y = 0)
			gen(b-> CG_STINDB: CG_STINDR, 0);
		else ie (atomic(y))
			store(y);
		else
			aw("bad location", y[SNAME]);
	end
	else do
		aw("syntax error", Str);
	end
	xsemi();
end

stmt(body) ie (Tk = KFOR)
		for_stmt();
	else ie (Tk = KHALT)
		halt_stmt();
	else ie (Tk = KIE)
		if_stmt(1);
	else ie (Tk = KIF)
		if_stmt(0);
	else ie (Tk = KELSE)
		aw("ELSE without IE", 0);
	else ie (Tk = KLEAVE)
		leave_stmt();
	else ie (Tk = KLOOP)
		loop_stmt();
	else ie (Tk = KRETURN)
		return_stmt();
	else ie (Tk = KWHILE)
		while_stmt();
	else ie (Tk = KDO)
		compound(body);
	else ie (Tk = SYMBOL)
		asg_or_call();
	else ie (Tk = KCALL) do
		clear();
		factor();
	end
	else ie (Tk = SEMI)
		Tk := scan();
	else
		expect(%1, "statement");

compound(body) do var oyp, olp, onp, ols, msg;
	msg := "unexpected end of compound statement";
	Tk := scan();
	oyp := Yp;
	onp := Np;
	olp := Lp;
	ols := Ls;
	Ls := 0;
	while (Tk = KVAR \/ Tk = KCONST \/ Tk = KSTRUCT) do
		if (Tk = KVAR /\ \Frame) do
			gen(CG_MKFRAME, 0);
			Frame := 1;
		end
		declaration(0);
	end
	if (Ls) gen(CG_STACK, -(Ls*BPW));
	if (body) Lp0 := Lp;
	while (Tk \= KEND) do
		if (Tk = ENDFILE) aw(msg, 0);
		stmt(0);
	end
	Tk := scan();
	if (body) do
		gen(CG_CLEAR, 0);
		resolve(Retlab);
		Retlab := 0;
	end
	if (olp \= Lp) gen(CG_UNSTACK, olp-Lp);
	if (body /\ Frame) do
		gen(CG_DELFRAME, 0);
		Frame := 0;
	end
	Yp := oyp;
	Np := onp;
	Lp := olp;
	Ls := ols;
end

program() do var i;
	Tk := scan();
	while (	Tk = KVAR \/ Tk = KCONST \/ Tk = SYMBOL \/
		Tk = KDECL \/ Tk = KSTRUCT \/ Tk = KPUBLIC \/
		Tk = KEXTERN \/ Tk = KMODULE \/ Tk = KUSE
	)
		declaration(GLOB);
	if (Tk \= KDO)
		aw("DO or declaration expected", 0);
	compound(0);
	if (Tk \= ENDFILE)
		aw("trailing characters", Str);
	gen(CG_HALT, 0);
	for (i=0, Yp, SYM)
		if (Syms[i+SFLAGS] & FORW /\ Syms[i+SVALUE])
			aw("undefined function", Syms[i+SNAME]);
end

!
! Main
!

init(p) do var i, b::10;
	Pass := p;
	Rejected := 0;
	Ip := 0;
	Ep := 0;
	Gp := 0;
	Gtop := 0;
	Outp := 0;
	Tp := 0;
	Dp := 0;
	Lp := 0;
	Yp := 0;
	Np := 0;
	Fwp := 0;
	Lab := 0;
	Line := 1;
	Acc := 0;
	Retlab := 0;
	Frame := 0;
	Loop0 := %1;
	Lvp := 0;
	Llp := 0;
	Modname::0 := 0;
	Qi := CG_NULL;
	Codetbl := [
		[ CG_NULL,	""	],
		[ CG_PUSH,	"01"	],
		[ CG_CLEAR,	"02"	],
		[ CG_DROP,	"03"	],
		[ CG_LDVAL,	"04,w"	],
		[ CG_LDADDR,	"05,w"	],
		[ CG_LDLREF,	"06,w"	],
		[ CG_LDGLOB,	"07,w"	],
		[ CG_LDLOCL,	"08,w"	],
		[ CG_STGLOB,	"09,w"	],
		[ CG_STLOCL,	"0a,w"	],
		[ CG_STINDR,	"0b"	],
		[ CG_STINDB,	"0c"	],
		[ CG_INCGLOB,	"0d,w"	],
		[ CG_INCLOCL,	"0e,w"	],
		[ CG_INCR,	"0f,w"	],
		[ CG_STACK,	"10,w"	],
		[ CG_UNSTACK,	"11,w"	],
		[ CG_LOCLVEC,	"12"	],
		[ CG_GLOBVEC,	"13,w"	],
		[ CG_INDEX,	"14"	],
		[ CG_DEREF,	"15"	],
		[ CG_INDXB,	"16"	],
		[ CG_DREFB,	"17"	],
		[ CG_CALL,	"18,w"	],
		[ CG_CALR,	"19"	],
		[ CG_JUMP,	"1a,w"	],
		[ CG_RJUMP,	"1b,r"	],
		[ CG_JMPFALSE,	"1c,w"	],
		[ CG_JMPTRUE,	"1d,w"	],
		[ CG_FOR,	"1e,w"	],
		[ CG_FORDOWN,	"1f,w"	],
		[ CG_MKFRAME,	"20"	],
		[ CG_DELFRAME,	"21"	],
		[ CG_RET,	"22"	],
		[ CG_HALT,	"23,w"	],
		[ CG_NEG,	"24"	],
		[ CG_INV,	"25"	],
		[ CG_LOGNOT,	"26"	],
		[ CG_ADD,	"27"	],
		[ CG_SUB,	"28"	],
		[ CG_MUL,	"29"	],
		[ CG_DIV,	"2a"	],
		[ CG_MOD,	"2b"	],
		[ CG_AND,	"2c"	],
		[ CG_OR,	"2d"	],
		[ CG_XOR,	"2e"	],
		[ CG_SHL,	"2f"	],
		[ CG_SHR,	"30"	],
		[ CG_EQ,	"31"	],
		[ CG_NE,	"32"	],
		[ CG_LT,	"33"	],
		[ CG_GT,	"34"	],
		[ CG_LE,	"35"	],
		[ CG_GE,	"36"	],
		[ CG_UMUL,	"37"	],
		[ CG_UDIV,	"38"	],
		[ CG_ULT,	"39"	],
		[ CG_UGT,	"3a"	],
		[ CG_ULE,	"3b"	],
		[ CG_UGE,	"3c"	],
		[ CG_JMPEQ,	""	],
		[ CG_JMPNE,	""	],
		[ CG_JMPLT,	""	],
		[ CG_JMPGT,	""	],
		[ CG_JMPLE,	""	],
		[ CG_JMPGE,	""	],
		[ %1,		""	] ];
	Opttbl := [ %1 ];
	Ops := [[ 7, 3, "mod",	BINOP,  CG_MOD		],
		[ 6, 1, "+",	BINOP,  CG_ADD		],
		[ 7, 1, "*",	BINOP,  CG_MUL		],
		[ 0, 1, ";",	SEMI,   0		],
		[ 0, 1, ",",	COMMA,  0		],
		[ 0, 1, "(",	LPAREN, 0		],
		[ 0, 1, ")",	RPAREN, 0		],
		[ 0, 1, "[",	LBRACK, 0		],
		[ 0, 1, "]",	RBRACK, 0		],
		[ 3, 1, "=",	BINOP,  CG_EQ		],
		[ 5, 1, "&",	BINOP,  CG_AND		],
		[ 5, 1, "|",	BINOP,  CG_OR		],
		[ 5, 1, "^",	BINOP,  CG_XOR		],
		[ 0, 1, "@",	ADDROF, 0		],
		[ 0, 1, "~",	UNOP,   CG_INV		],
		[ 0, 1, ":",	COLON,  0		],
		[ 0, 2, "::",	BYTEOP, 0		],
		[ 0, 2, ":=",	ASSIGN, 0		],
		[ 0, 1, "\\",	UNOP,   CG_LOGNOT	],
		[ 1, 2, "\\/",	DISJ,   0		],
		[ 3, 2, "\\=",	BINOP,  CG_NE		],
		[ 4, 1, "<",	BINOP,  CG_LT		],
		[ 4, 2, "<=",	BINOP,  CG_LE		],
		[ 5, 2, "<<",	BINOP,  CG_SHL		],
		[ 4, 1, ">",	BINOP,  CG_GT		],
		[ 4, 2, ">=",   BINOP,  CG_GE		],
		[ 5, 2, ">>",	BINOP,  CG_SHR		],
		[ 6, 1, "-",	BINOP,  CG_SUB		],
		[ 0, 2, "->",	COND,   0		],
		[ 7, 1, "/",	BINOP,  CG_DIV		],
		[ 2, 2, "/\\",	CONJ,   0		],
		[ 0, 1, ".",	DOT,    0		],
		[ 7, 2, ".*",	BINOP,	CG_UMUL		],
		[ 7, 2, "./",	BINOP,	CG_UDIV		],
		[ 4, 2, ".<",	BINOP,	CG_ULT		],
		[ 4, 3, ".<=",	BINOP,	CG_ULE		],
		[ 4, 2, ".>",	BINOP,	CG_UGT		],
		[ 4, 3, ".>=",	BINOP,	CG_UGE		],
		[ 0, 0, 0,	0,      0		] ];
	Equal_op := findop("=");
	Minus_op := findop("-");
	Mul_op := findop("*");
	Add_op := findop("+");
	i := 0;
	while (Codetbl[i][0] \= %1) do
		if (Codetbl[i][0] \= i) do
			str_copy(b, ntoa(i));
			oops("bad code table entry", b);
		end
		i := i+1;
	end
	emit('T');
	emit('3');
	emit('X');
	emit('0');
	emitw(0);
	Tp := 0;
end

info() do
	writes("Text = ");
	writes(ntoa(Tp));
	writes(", Data = ");
	writes(ntoa(Dp+622));
	writes(", Symbols = ");
	writes(ntoa(Yp/SYM));
	writes(", Nlist = ");
	writes(ntoa(Np));
	writes(", Labels = ");
	writes(ntoa(Lab));
	nl();
end

phase(in, n) do
	if (Verbose) do
		writes(n-> "Pass 2:": "Pass 1:");
		nl();
	end
	Infile := t.open(in, T3X.OREAD);
	if (Infile < 0) aw("no such file", in);
	Outfile := t.open(Outname, T3X.OWRITE);
	if (Outfile < 0) aw("cannot create", Outname);
	init(n);
	program();
	commit();
	t.close(Infile);
	flush();
	t.close(Outfile);
end

upcase(s) do var i;
	i := 0;
	while (s::i) do
		if ('a' <= s::i /\ s::i <= 'z')
			s::i := s::i-'a'+'A';
		i := i+1;
	end
	return s;
end

do var in::14, k;
	Outname::0 := 0;
	Verbose := 0;
	if (t.getarg(2, in, 4) \= %1 /\ str_equal(upcase(in), "/V"))
		Verbose := 1;
	k := t.getarg(1, in, 11);
	if (k < 0) aw("missing file name", 0);
	t.memcopy(@in::k, ".t", 3);
	str_copy(Outname, in);
	t.memcopy(@Outname::k, ".tc", 5);
	phase(in, 0);
	phase(in, 1);
	info();
	! dumpsyms(0, Yp);
end
