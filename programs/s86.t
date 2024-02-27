! S86 -- A simple one-pass assembler for an 8086 subset
! Nils M Holm, 1998,2000,2002,2021-2023
! Public domain / 0BSD license

! Still lacking a few instructions and addressing modes.
! Lots of bugs, see s86-bugs.

use t3x: t;

const	DEBUG = 0;

const	BUFSIZE=	1026,	! Must be <= 2050 !
	OBUFL=		1024,	! Must be <= 1024 !
	SYMBSPACE=	8192,	! symbol table size
	NSPACE=		4096,	! name pool size
	IMKSPACE=	512,	! mark table size
	TEXTLEN=	129,	! max. token length
	CSPACE=		16384;	! code buffer size

const	META = 256;

! tokens ...

const	ENDOFLINE = 1, ENDOFFILE = %1;

const	SYMBOL = 20, STRING = 22, MNEMO = 23;

const	COMMA = 30, COLON = 31, PLUS = 37, STAR = 38;

const	K_BYTE = 100, K_DB = 101, K_DW = 102, K_EQU = 103,
	K_WORD = 104, K_OFFSET = 105, K_TEXT = 106;

struct	MNEMONIC =	! mnemonic/opcode mapping
	MNAME,		! text
	MCODE,		! opcode
	MTYPE;		! type, see assemble()

struct	SYMENT =	! symbol table entry
	SNAME,		! symbol name
	SADDR,		! address
	SFLGS;		! flags (FUNDEFD)

struct	MARKENT =	! mark entry
	MADDR,		! address
	MCLSS,		! class (MRELATIVE, MSHORT, %1)
	MNEXT;		! pointer to next mark

const	MRELATIVE = 256,	! mark is PC-relative
	MSHORT = 512;		! mark is short (8-bit displacement)

! symbol classes
const	TCODE='T', TNONE='X';

! symbols flags
const	FUNDEFD=1;	! not yet defined

var	Infile;				! input file
var	Outfile;			! output file
var	Symbols[SYMBSPACE], St;		! symbol table
var	Names::NSPACE, Nt;		! name pool
var	Imarks[IMKSPACE], It, Ni;	! mark pool
var	Code::CSPACE, Ctop;		! code buffer
var	Origin;				! beginning of text segment
var	Line;				! input line
var	Errcount;			! error counter
var	Token;				! curr. token
var	Text, Textbuf::TEXTLEN, Tp;	! curr. token text
var	Op;				! curr. instruction
var	Buffer::BUFSIZE, Cp, Ep, Lowp, Nomore;	! input buffer, etc
var	Mlist;				! mnemonics
var	Nmn;				! # of mnemonics
var	Wop,				! word operation flag (0=byte)
	IsOffset,			! operand uses OFFSET
	Off1, Off2;			! operand1/2 +offset value
var	Oper1, Oper2;			! literal text of 1st/2nd operand

init() do var i;
	Tp := 0;
	St := SYMBSPACE;
	Nt := 0;
	Line := 1;
	Errcount := 0;
	Cp := 0;
	Ep := 0;
	Lowp := 0;
	Nomore := 0;
	Ctop := 0;
	Origin := 0;
	It := 0;
	Ni := 0;
	for (i=0, IMKSPACE-MARKENT, MARKENT) Imarks[i+MCLSS] := %1;
	Mlist := [
	[ "",		0,	0	],
	[ "aaa",	"37",	'b'	],
	[ "aad",	"0AD5",	'w'	],
	[ "aam",	"0AD4",	'w'	],
	[ "aas",	"3F",	'b'	],
	[ "adc",	"8010",	1	],
	[ "add",	"8000",	1	],
	[ "and",	"8020",	1	],
	[ "call",	"E8FF10",4	],
	[ "cbw",	"98",	'b'	],
	[ "clc",	"F8",	'b'	],
	[ "cld",	"FC",	'b'	],
	[ "cli",	"FA",	'b'	],
	[ "cmc",	"F5",	'b'	],
	[ "cmp",	"8038",	1	],
	[ "cmpsb",	"A6",	'b'	],
	[ "cmpsw",	"A7",	'b'	],
	[ "cseg",	"2E",	'b'	],
	[ "cwd",	"99",	'b'	],
	[ "daa",	"27",	'b'	],
	[ "das",	"2F",	'b'	],
	[ "dec",	"FE08",	2	],
	[ "div",	"F630",	2	],
	[ "dseg",	"3E",	'b'	],
	[ "eseg",	"26",	'b'	],
	[ "hlt",	"F4",	'b'	],
	[ "idiv",	"F638",	2	],
	[ "imul",	"F628",	2	],
	[ "inb",	"EC",	'b'	],
	[ "inc",	"FE00",	2	],
	[ "int",	"CCCD",	6	],
	[ "into",	"CE",	'b'	],
	[ "inw",	"ED",	'b'	],
	[ "iret",	"CF",	'b'	],
	[ "ja",		"77",	4	],
	[ "jae",	"73",	4	],
	[ "jb",		"72",	4	],
	[ "jbe",	"76",	4	],
	[ "jc",		"72",	4	],
	[ "jcxz",	"E3",	4	],
	[ "je",		"74",	4	],
	[ "jg",		"7F",	4	],
	[ "jge",	"7D",	4	],
	[ "jl",		"7C",	4	],
	[ "jle",	"7E",	4	],
	[ "jmp",	"E9FF20",4	],
	[ "jmps",	"EB",	4	],
	[ "jnc",	"73",	4	],
	[ "jne",	"75",	4	],
	[ "jno",	"71",	4	],
	[ "jnp",	"7B",	4	],
	[ "jns",	"79",	4	],
	[ "jnz",	"75",	4	],
	[ "jo",		"70",	4	],
	[ "jp",		"7A",	4	],
	[ "js",		"78",	4	],
	[ "jz",		"74",	4	],
	[ "lahf",	"9F",	'b'	],
	[ "lock",	"F0",	'b'	],
	[ "lodsb",	"AC",	'b'	],
	[ "lodsw",	"AD",	'b'	],
	[ "loop",	"E2",	4	],
	[ "loopnz",	"E0",	4	],
	[ "loopz",	"E1",	4	],
	[ "mov",	"C688",	1	],
	[ "movsb",	"A4",	'b'	],
	[ "movsw",	"A5",	'b'	],
	[ "mul",	"F620",	2	],
	[ "neg",	"F618",	2	],
	[ "nop",	"90",	'b'	],
	[ "not",	"F610",	2	],
	[ "or",		"8008",	1	],
	[ "outb",	"EE",	'b'	],
	[ "outw",	"EF",	'b'	],
	[ "pop",	"8F00",	5	],
	[ "popf",	"9D",	'b'	],
	[ "push",	"FF30",	5	],
	[ "pushf",	"9C",	'b'	],
	[ "rcl",	"D010",	3	],
	[ "rcr",	"D018",	3	],
	[ "rep",	"F3",	'b'	],
	[ "repnz",	"F2",	'b'	],
	[ "repz",	"F3",	'b'	],
	[ "ret",	"C3",	'b'	],
	[ "rol",	"D000",	3	],
	[ "ror",	"D008",	3	],
	[ "sahf",	"9E",	'b'	],
	[ "sal",	"D020",	3	],
	[ "sar",	"D038",	3	],
	[ "sbb",	"8018",	1	],
	[ "scasb",	"AE",	'b'	],
	[ "scasw",	"AF",	'b'	],
	[ "shl",	"D020",	3	],
	[ "shr",	"D028",	3	],
	[ "sseg",	"36",	'b'	],
	[ "stc",	"F9",	'b'	],
	[ "std",	"FD",	'b'	],
	[ "sti",	"FB",	'b'	],
	[ "stosb",	"AA",	'b'	],
	[ "stosw",	"AB",	'b'	],
	[ "sub",	"8028",	1	],
	[ "test",	"F684",	1	],
	[ "wait",	"9B",	'b'	],
	[ "xchg",	"9086",	1	],
	[ "xlat",	"D7",	'b'	],
	[ "xor",	"8030",	1	],
	%1
	];
	Nmn := 0;
	while (Mlist[Nmn] \= %1) Nmn := Nmn+1;
end

length(a) return t.memscan(a, 0, 32767);

report(s) t.write(T3X.SYSERR, s, length(s));

reptnl() do var b::3;
	report(t.newline(b));
end

! convert number to string
var	ntoa_buf::32;
ntoa(v) do var g, i;
	g := 0;
	if (v < 0) do
		g := 1;
		v := -v;
	end
        ntoa_buf::31:= 0;
        i := 30;
        while (v \/ i = 30) do
                ntoa_buf::i := v mod 10 + '0';
                i := i - 1;
                v := v / 10;
        end
        if (g) do
                ntoa_buf::i := '-';
                i := i-1;
        end
        return @ntoa_buf::(i+1);
end

issym(x) return 'a' <= x /\ x <= 'z' \/ 'A' <= x /\ x <= 'Z' \/ x = '_';

isdigit(x) return '0' <= x /\ x <= '9' \/ x = '$';

error(m, s) do var o;
	report("S86: ");
	report(ntoa(Line));
	report(": ");
	report(m);
	if (s) do
		report(": ");
		report(s);
	end
	reptnl();
	Errcount := Errcount +1;
end

fatal(m, s) do
	error(m, s);
	report("terminating.");
	reptnl();
	halt 1;
end

bigval() error("value too big", Text);

badops() error("bad operand(s)", 0);

fillbuf() do var k;
	if (Nomore) return;
	! move remaining input to start of buffer
	ie (Ep > Cp) do
		t.memcopy(Buffer, @Buffer::Cp, Ep-Cp);
		Ep := Ep-Cp;
	end
	else do
		Ep := 0;
	end
	Cp := 0;
	k := t.read(Infile, @Buffer::Ep, BUFSIZE/2-1);
	ie (k < 1)
		Nomore := 1;
	else
		Ep := Ep + k;
	Lowp := Ep-TEXTLEN;
end

eof() return Nomore /\ Cp >= Ep;

getce() do var c;
	c := Buffer::Cp;
	Cp := Cp+1;
	if (c \= '\\') return c;
	c := Buffer::Cp;
	Cp := Cp+1;
	if (c = 'a') return '\a';
	if (c = 'b') return '\b';
	if (c = 'e') return '\e';
	if (c = 'f') return '\f';
	if (c = 'n') return '\n';
	if (c = 'q') return '"' | META;
	if (c = '"') return '"' | META;
	if (c = 'r') return '\r';
	if (c = 's') return '\s';
	if (c = 't') return '\t';
	if (c = 'v') return '\v';
	return c;
end

findkw(s) do
	if (s::0 = '.' /\ \t.memcomp(s, ".text", 6)) return K_TEXT;
	if (s::0 = 'b' /\ \t.memcomp(s, "byte", 5)) return K_BYTE;
	if (s::0 = 'd' /\ \t.memcomp(s, "db", 3)) return K_DB;
	if (s::0 = 'd' /\ \t.memcomp(s, "dw", 3)) return K_DW;
	if (s::0 = 'e' /\ \t.memcomp(s, "equ", 4)) return K_EQU;
	if (s::0 = 'o' /\ \t.memcomp(s, "offset", 7)) return K_OFFSET;
	if (s::0 = 'w' /\ \t.memcomp(s, "word", 5)) return K_WORD;
	return 0;
end

! find mnemonic using binary tree search
findmnemo(s) do var p, d, r, k;
	p := 64;
	d := 32;
	k := length(s)+1;
	while (d) do
		ie (p < 0) do
			p := p+d;
		end
		else ie (p >= Nmn) do
			p := p-d;
		end
		else do
			r := t.memcomp(s, Mlist[p][MNAME], k);
			ie (\r) return p;
			else ie (r > 0) p := p + d;
			else p := p - d;
		end
		d := d >> 1;
		! adjust for uneven (non 2^n sized) mnemonic array
		if (p & 1 /\ \d) d := 1;
	end
	return 0;
end

scan() do var c, i, bc[3];
	if (Cp >= Lowp) fillbuf();
	c := Buffer::Cp;
	Cp := Cp+1;
	! skip over white space and comments
	while (1) do
		while (	c = '\s' \/ c = '\t' \/
			c = '\r' \/ c = '\f'
		) do
			c := Buffer::Cp;
			Cp := Cp+1;
		end
		if (c \= ';') leave;
		while (c \= '\n') do
			c := Buffer::Cp;
			Cp := Cp+1;
		end
		if (Cp >= Lowp) fillbuf();
	end
	if (c = '/') do
		Tp := 0;
		return ENDOFLINE;
	end
	if (c = '\n') do
		Line := Line+1;
		Tp := 0;
		return ENDOFLINE;
	end
	Text := @Textbuf::Tp;
	if (eof()) return ENDOFFILE;
	if (c = ',') return COMMA;
	if (c = ':') return COLON;
	if (c = '+') return PLUS;
	if (c = '@') return K_OFFSET;
	if (c = '*') return STAR;
	if (issym(c) \/ c = '.') do
		while (1) do
			if (\(issym(c) \/ '0' <= c /\ c <= '9' \/ c = '.'))
				leave;
			if (Tp >= TEXTLEN-1) fatal("line too long", 0);
			Textbuf::Tp := c;
			Tp := Tp+1;
			c := Buffer::Cp;
			Cp := Cp+1;
		end
		Textbuf::Tp := 0;
		Tp := Tp+1;
		Cp := Cp-1;
		c := findkw(Text);
		if (c) return c;
		Op := findmnemo(Text);
		if (Op) return MNEMO;
		return SYMBOL;
	end
	if (c = '[') do	! indirection, like [bp+4]
		i := 0;
		while (c \= ']') do
			if (Tp >= TEXTLEN-1) fatal("line too long", 0);
			Textbuf::Tp := c;
			Tp := Tp+1;
			c := Buffer::Cp;
			Cp := Cp+1;
		end
		Textbuf::Tp := ']';
		Textbuf::(Tp+1) := 0;
		Tp := Tp+2;
		return SYMBOL;
	end
	if ('0' <= c /\ c <= '9' \/ c = '-') do
		i := 0;
		while ('0' <= c /\ c <= '9' \/ c = '-') do
			if (Tp >= TEXTLEN-1) fatal("line too long", 0);
			Textbuf::Tp := c;
			Tp := Tp+1;
			c := Buffer::Cp;
			Cp := Cp+1;
		end
		Textbuf::Tp := 0;
		Tp := Tp+1;
		Cp := Cp-1;
		return SYMBOL;
	end
	if (c = '$') do
		i := 0;
		while (	'0' <= c /\ c <= '9' \/
			'A' <= c /\ c <= 'F' \/ c = '$'
		) do
			if (Tp >= TEXTLEN-1) fatal("line too long", 0);
			Textbuf::Tp := c;
			Tp := Tp+1;
			c := Buffer::Cp;
			Cp := Cp+1;
		end
		Textbuf::Tp := 0;
		Tp := Tp+1;
		Cp := Cp-1;
		return SYMBOL;
	end
	if (c = '\'') do
		Textbuf::Tp := '\'';
		Textbuf::(Tp+1) := getce();
		Textbuf::(Tp+2) := '\'';
		Textbuf::(Tp+3) := 0;
		Tp := Tp+4;
		ie (c \= '\'')
			error("missing `''", 0);
		else
			Cp := Cp+1;
		return SYMBOL;
	end
	if (c = '"') do
		i := 0;
		c := getce();
		while (c \= '"') do
			if (Tp >= TEXTLEN-2) fatal("line too long", 0);
			Textbuf::Tp := c & ~META;
			Tp := Tp+1;
			c := getce();
			if (eof()) fatal("unexpected EOF", 0);
		end
		Textbuf::Tp := 0;
		Tp := Tp+1;
		return STRING;
	end
	bc::0 := c/16 + (c/16 > 9-> 'A'-10: '0');
	bc::1 := c mod 16 + (c mod 16 > 9-> 'A'-10: '0');
	bc::2 := 0;
	fatal("bad input character", bc);
end

match(tok, s) do
	ie (tok = Token)
		Token := scan();
	else
		error(s, 0);
end

synch() do
	while (Token \= ENDOFLINE /\ Token \= ENDOFFILE)
		Token := scan();
end

xnl() do
	ie (Token = ENDOFLINE) do
		Token := scan();
	end
	else do
		error("end of line expected", 0);
		synch();
	end
end

byte(s) do var l, h;
	h := s::0;
	l := s::1;
	return   (('0'<=h /\ h<='9' -> h-'0': h-'A'+10) << 4)
		+ ('0'<=l /\ l<='9' -> l-'0': l-'A'+10);
end

word(s) do var v, i, d;
	v := 0;
	i := 0;
	while(s::i) do
		d := s::i;
		ie ('0' <= d /\ d <= '9')
			d := d-'0';
		else ie ('A' <= d /\ d <= 'F')
			d := d-'A'+10;
		else
			leave;
		v := (v<<4) + d;
		i := i+1;
	end
	return v;
end

decl findsym(2);

! convert anything that has a value (or address)
! to a machine word
xtoi(s) do var v, d, i, g, y;
	i := 0;
	g := 0;
	ie (s::i = '-') do
		g := 1; i := 1;
	end
	else if (s::i = '+') do
		i := 1;
	end
	if (s::i = '\'') return g-> -s::(i+1): s::(i+1);
	if (s::i = '$') return g-> -word(@s::(i+1)): word(@s::(i+1));
	if (s::i = '@') do
		v := length(s);
		ie (s::(v-1) = ']')
			s::(v-1) := 0;
		else
			v := 0;
		y := findsym(@s::(i+1), 0);
		if (y = %1) error("undefined symbol", @s::(i+1));
		if (v) s::(v-1) := ']';
		return y = %1-> %1: y[SADDR];
	end
	v := 0;
	while (s::i) do
		d := s::i;
		ie ('0' <= d /\ d <= '9')
			d := d-'0';
		else
			leave;
		v := v*10 + d;
		i := i+1;
	end
	return g-> -v: v;
end

emit(n) do
	if (Ctop >= CSPACE) fatal("text segment overflow", 0);
	Code::Ctop := n;
	Ctop := Ctop+1;
end

emitword(n) do
	emit(n);
	emit(n>>8);
end

decl	newsym(3);

findsym(name, add) do var i, s, k;
	k := length(name);
	for (i=St, SYMBSPACE, SYMENT) do
		s := @Symbols[i];
		if (s[SNAME]::0 = k /\ \t.memcomp(@s[SNAME]::1, name, k))
			return s;
	end
	if (add) return newsym(name, FUNDEFD, 0);
	return %1;
end

intmark(sym, rel, shrt) do var m, here;
	! search a free entry, treating the table as a ring
	here := It >= IMKSPACE-MARKENT-> 0: It;
	while (1) do
		It := It+MARKENT;
		if (It >= IMKSPACE-MARKENT) It := 0;
		if (Imarks[It+MCLSS] = %1) leave;
		if (It = here) fatal("ouf of free imarks", 0);
	end
	m := @Imarks[It];
	m[MADDR] := Ctop;
	m[MCLSS] := (rel-> MRELATIVE: 0) | (shrt-> MSHORT: 0);
	m[MNEXT] := sym[SADDR];
	sym[SADDR] := m;
	Ni := Ni+1;
end

! Find the address of a symbol.
! If the symbol does not (yet) exist,
! add a mark for the current address
findaddr(name, rel, rloc, shrt) do var s, a;
	s := findsym(name, 1);
	a := s[SADDR];
	if (s[SFLGS] & FUNDEFD) do
		intmark(s, rel, shrt);
		a := Ctop+Origin;
	end
	return a;
end

! resolve all marks connected to the symbol S
iresolve(s) do
	var	a, m, n, i;
	var	seg, dest, clss, disp;

	m := s[SADDR];
	a := Ctop+Origin;
	s[SADDR] := a;
	while (m \= %1) do
		clss := m[MCLSS];
		seg := Code;
		dest := m[MADDR];
		ie (clss & MRELATIVE) do
			ie (clss & MSHORT) do
				disp := a - (dest+Origin) - 1;
				if (disp < %128 \/ disp > 127)
					error("short branch out of range", 0);
				seg::dest := disp;
			end
			else do
				disp := a - (dest+Origin) - 2;
				seg::dest := disp;
				seg::(dest+1) := disp>>8;
			end
		end
		else do
			seg::dest := a;
			seg::(dest+1) := a>>8;
		end
		n := m;
		m := m[MNEXT];
		n[MCLSS] := %1;
		Ni := Ni-1;
	end
end

newsym(name, flags, check) do
	var	s, k, redef, c;
	var	pname[TEXTLEN];

	redef := 0;
	if (check) do
		s := findsym(name, 0);
		if (s \= %1) do
			if (\(s[SFLGS] & FUNDEFD) \/ flags) do
				error("duplicate symbol", name);
				return 0;
			end
			redef := 1;
		end
	end
	ie (redef) do
		s[SFLGS] := s[SFLGS] & ~FUNDEFD;
		if (\flags) iresolve(s);
	end
	else do
		k := length(name);
		if (St - SYMENT < 0)
			fatal("symbol table overflow", name);
		St := St-SYMENT;
		s := @Symbols[St];
		s[SNAME] := @Names::Nt;
		if (Nt + k+1 >= NSPACE)
			fatal("out of name space", name);
		Names::Nt := k;
		t.memcopy(@Names::(Nt+1), name, k);
		Nt := Nt + k+1;
		s[SFLGS] := flags;
	end
	ie (flags & FUNDEFD)
		s[SADDR] := %1;
	else
		s[SADDR] := Ctop+Origin;
	return s;
end

emitdef(n, w) ie (w)
	emitword(n);
else
	emit(n);

defdata() do var w, v, r, i;
	w := Token = K_DW;
	Token := scan();
	while (1) do
		ie (Token = SYMBOL) do
			ie (	isdigit(Text::0) \/ Text::0 = '-' \/
				Text::0 = '\'' \/ Text::0 = '$'
			) do
				v := xtoi(Text);
				if ((v > 255 \/ v < %128) /\ \w) bigval();
				emitdef(v, w);
			end
			else do
				error("invalid number", Text);
			end
			r := 1;
		end
		else ie (Token = STRING) do
			r := 0;
			i := 0;
			while (Text::i) do
				emitdef(Text::i, w); i := i+1;
			end
		end
		else ie (Token = STAR) do
			Token := scan();
			if (Token \= SYMBOL \/ \isdigit(Text::0))
				error("size expected after *", Text);
			v := xtoi(Text);
			for (i=0, v) emit(0);
		end
		else ie (Token = K_OFFSET) do
			Token := scan();
			ie (Token = SYMBOL)
				emitword(findaddr(Text, 0, 1, 0));
			else
				error("symbol expected", Text);
			r := 0;
		end
		else do
			error("initializer expected", Text);
			synch();
			leave;
		end
		Token := scan();
		if (Token \= COMMA) leave;
		Token := scan();
	end
	xnl();
end

defequ() do
	Token := scan();
	if (St = SYMBSPACE) error("missing label", 0);
	ie (	isdigit(Text::0) \/ Text::0 = '-' \/
		Text::0 = '\''
	) do
		Symbols[St+SADDR] := xtoi(Text);
	end
	else do
		error("invalid number", Text);
	end
	Token := scan();
	xnl();
end

deftext() do
	Token := scan();
	if (Token \= ENDOFLINE) do
		ie (	isdigit(Text::0) \/ Text::0 = '-' \/
			Text::0 = '\'' \/ Text::0 = '$'
		) do
			Origin := xtoi(Text);
		end
		else do
			error("invalid number", Text);
		end
		Token := scan();
	end
	xnl();
end

reg(s) do
	var	owop, p;
	var	regs;

	if (\(s::0 /\ s::1) \/ s::2) return %1;
	owop := Wop;
	Wop := 1;
	regs := "axcxdxbxspbpsidi";	! order is important !
	for (p=0, 16, 2) do
		if (s::0 = regs::p /\ s::1 = regs::(p+1))
			return p>>1;
	end
	Wop := 0;
	regs := "alcldlblahchdhbh";	! order is important !
	for (p=0, 16, 2) do
		if (s::0 = regs::p /\ s::1 = regs::(p+1))
			return p>>1;
	end
	Wop := owop;
	return %1;
end

sreg(s) do
	var	owop;

	owop := Wop;
	Wop := 1;
	if (\t.memcomp(s, "cs", 3)) return 1;
	if (\t.memcomp(s, "ds", 3)) return 3;
	if (\t.memcomp(s, "es", 3)) return 0;
	if (\t.memcomp(s, "ss", 3)) return 2;
	Wop := owop;
	return %1;
end

indirect(s) do
	if (s::0 \= '[') return %1;
	if (\t.memcomp(s, "[si]", 5)) return 4;
	if (\t.memcomp(s, "[di]", 5)) return 5;
	if (\t.memcomp(s, "[bx]", 5)) return 7;
	if (\t.memcomp(s, "[bx+si]", 8)) return 0;
	if (\t.memcomp(s, "[bx+di]", 8)) return 1;
	if (\t.memcomp(s, "[bp+si]", 8)) return 2;
	if (\t.memcomp(s, "[bp+di]", 8)) return 3;
	return %1;
end

inddisp(s) do
	if (s::0 \= '[') return %1;
	if (\t.memcomp(s, "[bp]", 4)) return 6;		! is really [bp+disp]
	if (	\('0' <= s::4 /\ s::4 <= '9') /\
		\('A' <= s::4 /\ s::4 <= 'F') /\
		s::4 \= '$' /\ s::4 \= '@'
	)
		return %1;
	if (s::3 \= '+' /\ s::3 \= '-') return %1;
	if (\t.memcomp(s, "[bx", 3)) return 7;
	if (\t.memcomp(s, "[bp", 3)) return 6;
	if (\t.memcomp(s, "[di", 3)) return 5;
	if (\t.memcomp(s, "[si", 3)) return 4;
	return %1;
end

rrasm(o1, o2, m1, m2) do	! Register, Register
	emit(o1 | Wop);
	emit(o2 | 0xC0 | m1 | (m2<<3));
end

riasm(o1, o2, m1, m2) do	! Register, Indirect
	emit(o1 | 2 | Wop);
	emit(o2 | m2 | (m1<<3));
end

rmasm(o1, o2, m1, m2, sym) do	! Register, Memory
	emit(o1 | 2 | Wop);
	emit(o2 | m2 | (m1<<3));
	emitword(findaddr(sym, 0, 1, 0) + Off2);
end

rnasm(o1, o2, m1, val) do	! Register, Immediate
	ie (o1 = 0xC6) do
		emit(0xB0 | (Wop<<3) | m1);
	end
	else do
		if (o2 & 128) o2 := 0;	! patch TEST, XCHG
		emit(o1 | Wop);
		emit(o2 | 0xC0 | m1);
	end
	ie (Wop)
		emitword(val);
	else ie (val > 255 \/ val < %127)
		bigval();
	else
		emit(val);
end

roasm(o1, o2, m1, sym) do	! Register, Offset
	ie (o1 = 198) do	! 0cx6
		emit(0xB0| (Wop<<3) | m1);
	end
	else do
		emit(o1 | Wop);
		emit(o2 | 0xC0 | m1);
	end
	emitword(findaddr(sym, 0, 1, 0) + Off2);
end

irasm(o1, o2, m1, m2) do	! Indirect, Register
	emit(o1 | Wop);
	emit(o2 | m1 | (m2<<3));
end

mrasm(o1, o2, m1, m2, sym) do	! Memory, Register
	emit(o1 | Wop);
	emit(o2 | m1 | (m2<<3));
	emitword(findaddr(sym, 0, 1, 0) + Off1);
end

mnasm(o1, o2, m1, sym, val) do	! Memory, Immediate
	emit(o1 | Wop);
	emit(o2 | m1);
	emitword(findaddr(sym, 0, 1, 0) + Off1);
	ie (Wop)
		emitword(val);
	else ie (val > 255 \/ val < %127)
		bigval();
	else
		emit(val);
end

inasm(o1, o2, m1, val) do	! Indirect, Immediate
	emit(o1 | Wop);
	emit(o2 | m1);
	ie (Wop)
		emitword(val);
	else ie (val > 255 \/ val < %127)
		bigval();
	else
		emit(val);
end

rdasm(o1, o2, m1, m2, val) do	! Register, Register+Displacement
	emit(o1 | 2 | Wop);
	ie (val > 127 \/ val < %128) do
		emit(o2 | 0x80 | (m1<<3) | m2);
		emitword(val);
	end
	else do
		emit(o2 | 0x40 | (m1<<3) | m2);
		emit(val);
	end
end

drasm(o1, o2, m1, m2, val) do	! Register+Displacement, Register
	emit(o1 | Wop);
	ie (val > 127 \/ val < %128) do
		emit(o2 | 0x80 | m1 | (m2<<3));
		emitword(val);
	end
	else do
		emit(o2 | 0x40 | m1 | (m2<<3));
		emit(val);
	end
end

dnasm(o1, o2, m, disp, val) do	! Register+Displacement, Immediate
	emit(o1 | Wop);
	ie (disp > 127 \/ disp < %128) do
		emit(o2 | 0x80 | m);
		emitword(disp);
	end
	else do
		emit(o2 | 0x40 | m);
		emit(disp);
	end
	ie (Wop)
		emitword(val);
	else
		emit(val);
end

asm1(immed, op1, op2) do	! Group 1 (Binary) Instructions
	var	m1, m2, x;

	m1 := reg(Oper1);
	if (op1 = 0x84)	do	! TEST
		if (m1 < 0 /\ \(issym(Oper1::0) /\ isdigit(Oper2::0))) do
			badops();
			return;
		end
		if (\isdigit(Oper2::0)) do
			x := Oper1;
			Oper1 := Oper2;
			Oper2 := x;
			m1 := reg(Oper1);
		end
	end
	ie (m1 >= 0) do
		if (immed = 0xC6) do
			m2 := sreg(Oper2);
			if (m2 >= 0) do
				emit(0x8C);
				emit(0xC0| m1 | (m2<<3));
				return 0;
			end
		end
		m2 := reg(Oper2);
		ie (m2 >= 0) do
			rrasm(op1, op2, m1, m2);
		end
		else ie (issym(Oper2::0)) do
			ie (IsOffset) do
				roasm(immed, op1, m1, Oper2);
			end
			else do
				rmasm(op1, op2, m1, 6, Oper2);
			end
		end
		else ie (isdigit(Oper2::0) \/ Oper2::0 = '\'' \/
			Oper2::0 = '-'	
		) do
			rnasm(immed, op1, m1, xtoi(Oper2));
		end
		else ie (indirect(Oper2) >= 0)
			riasm(op1, op2, m1, indirect(Oper2));
		else ie (inddisp(Oper2) >= 0)
			rdasm(op1, op2, m1, inddisp(Oper2), xtoi(@Oper2::3));
		else
			badops();
	end
	else ie (immed = 0xC6 /\ sreg(Oper1) >= 0) do
		m2 := reg(Oper2);
		if (m2 < 0) badops();
		emit(0x8E);
		emit(0xC0| (sreg(Oper1)<<3) | m2);
	end
	else ie (issym(Oper1::0)) do
		ie (reg(Oper2) >= 0) do
			mrasm(op1, op2, 6, reg(Oper2), Oper1);
		end
		else ie ((isdigit(Oper2::0) \/ Oper2::0 = '\'' \/
			Oper2::0 = '-') /\
			immed = 0xC6 \/ op1 = 0x00 \/ op1 = 0x38 \/ op1 = 0x84
		) do
			mnasm(immed, op2, 6, Oper1, xtoi(Oper2));
		end
		else do
			badops();
		end
	end
	else ie (indirect(Oper1) >= 0) do
		m1 := indirect(Oper1);
		m2 := reg(Oper2);
		ie (m2 >= 0)
			irasm(op1, op2, m1, m2);
		else ie (isdigit(Oper2::0) \/ Oper2::0 = '\'' \/
			Oper2::0 = '-'
		)
			! inasm(immed, immed=0xC6-> 0: op1, m1, xtoi(Oper2));
			inasm(immed, 0, m1, xtoi(Oper2));
		else
			badops();
	end
	else ie (inddisp(Oper1) >= 0) do
		m2 := reg(Oper2);
		ie (m2 >= 0)
			drasm(op1, op2, inddisp(Oper1), m2, xtoi(@Oper1::3));
		else ie (isdigit(Oper2::0) \/ Oper2::0 = '\'' \/
			Oper2::0 = '-'
		) do
			dnasm(immed, op2, inddisp(Oper1), xtoi(@Oper1::3),
				xtoi(Oper2));
		end
		else do
			badops();
		end
	end
	else do
		badops();
	end
end

rasm(o1, o2, m1) do	! Register
	emit(o1 | Wop);
	emit(o2 | 0xC0 | m1);
end

iasm(o1, o2, m1) do	! Indirect
	emit(o1 | Wop);
	emit(o2 | m1);
end

masm(o1, o2, m1, sym) do	! Memory
	emit(o1 | Wop);
	emit(o2 | m1);
	emitword(findaddr(sym, 0, 1, 0) + Off1);
end

asm2(op1, op2) do	! Group 2 (Unary) Instructions
	var	m1;

	m1 := reg(Oper1);
	if (m1 >= 0) do
		rasm(op1, op2, m1);
		return 0;
	end
	m1 := indirect(Oper1);
	if (m1 >= 0) do
		iasm(op1, op2, m1);
		return 0;
	end
	if (issym(Oper1::0)) do
		masm(op1, op2, 6, Oper1);
		return 0;
	end
	badops();
end

asm3(op1, op2) do	! Group 3 (Shift,Rotate) Instructions
	ie (Oper2::0 = '1' /\ Oper2::1 = 0)
		asm2(op1, op2);
	else ie (\t.memcomp(Oper2, "cl", 3))
		asm2(op1|2, op2);
	else
		badops();
end

asm4(op1, a1, a2, jlong) do	! Group 4 (Jump,Call) Instructions
	var	dest, disp;

	if (\issym(Oper1::0) \/ reg(Oper1) >= 0) do
		if (\jlong) error("invalid indirect branch", 0);
		asm2(a1, a2);
		return 0;
	end
	emit(op1);
	dest := findaddr(Oper1, 1, 0, \jlong) + Off1;
	disp := dest - ((Ctop+Origin)+(jlong-> 2: 1));
	ie (jlong)
		emitword(disp);
	else ie (dest \= %1 /\ (disp < %128 \/ disp > 127))
		error("short branch out of range", 0);
	else
		emit(disp);
end

asm5(op1, op2) do	! Group 5 (Push,Pop) Instructions
	var	m1;

	m1 := reg(Oper1);
	if (m1 >= 0) do
		emit((op1 = 0xFF-> 0x50: 0x58) | m1);
		return 0;
	end
	m1 := sreg(Oper1);
	if (m1 >= 0) do
		emit((op1 = 0xFF-> 0x06: 0x07) | (m1<<3));
		return 0;
	end
	m1 := indirect(Oper1);
	if (m1 >= 0) do
		emit(op1);
		emit(op2 | m1);
		return  0;
	end
	m1 := inddisp(Oper1);
	if (m1 >= 0) do
		drasm(op1, op2, m1, 0, xtoi(@Oper1::3));
		return 0;
	end
	badops();
end

asm6(op1, op2) do	! Group 6: INT instruction
	var	n;

	if (\isdigit(Oper1::0)) badops();
	n := xtoi(Oper1);
	if (n > 255) bigval();
	ie (n = 3) do
		emit(op1);
	end
	else do
		emit(op2);
		emit(n);
	end
end

! accept +offset
plusoff(offp) do
	if (Token \= PLUS) return 0;
	Token := scan();
	ie (Token \= SYMBOL \/ \isdigit(Text::0)) do
		error("bad offset", Text);
	end
	else do
		offp[0] := xtoi(Text);
	end
	Token := scan();
end

assemble() do var m, c;
	m := Mlist[Op][MTYPE];
	c := Mlist[Op][MCODE];
	Token := scan();
	Wop := 1;
	Off1 := 0;
	Off2 := 0;
	ie (Token = K_WORD) do
		Wop := 1;
		Token := scan();
	end
	else if (Token = K_BYTE) do
		Wop := 0;
		Token := scan();
	end
	if (m = 'b') do
		emit(byte(c));
		xnl();
		return 0;
	end
	if (m = 'w') do
		emitword(word(c));
		xnl();
		return 0;
	end
	if (m = 2 \/ m = 4 \/ m = 5 \/ m = 6) do
		if (Token \= SYMBOL) error("operand expected", Text);
		Oper1 := Text;
		Token := scan();
		plusoff(@Off1);
		ie (m = 2) do
			asm2(byte(c), byte(@c::2));
		end
		else ie (m = 4) do
			ie (c::2)
				asm4(byte(c), byte(@c::2), byte(@c::4), 1);
			else
				asm4(byte(c), 0, 0, 0);
		end
		else ie (m = 5) do
			asm5(byte(c), byte(@c::2));
		end
		else if (m = 6) do
			asm6(byte(c), byte(@c::2));
		end
		xnl();
		return 0;
	end
	if (m = 1 \/ m = 3) do
		if (Token \= SYMBOL) error("operand expected", Text);
		Oper1 := Text;
		Token := scan();
		plusoff(@Off1);
		match(COMMA, "',' expected");
		ie (Token = K_OFFSET) do
			IsOffset := 1;
			Token := scan();
		end
		else do
			IsOffset := 0;
		end
		if (Token \= SYMBOL) error("operand expected", Text);
		Oper2 := Text;
		Token := scan();
		plusoff(@Off2);
		ie (m = 1) asm1(byte(c), byte(@c::2), 0);
		else if (m = 3) asm3(byte(c), byte(@c::2));
		xnl();
		return 0;
	end
end

statement() do
	if (Token = SYMBOL) do
		if (\issym(Text::0)) error("bad label", Text);
		newsym(Text, 0, 1);
		Token := scan();
		match(COLON, "missing ':'");
	end
	ie (Token = K_DB \/ Token = K_DW) do
		defdata();
	end
	else ie (Token = K_EQU) do
		defequ();
	end
	else ie (Token = K_TEXT) do
		deftext();
	end
	else ie (Token = MNEMO) do
		assemble();
	end
	else ie (Token = ENDOFLINE) do
		Token := scan();
	end
	else do
		error("bad statement", 0);
		synch();
	end
end

! report declared but undefined symbols
report_undefd() do var i, name::256;
	for (i=St, SYMBSPACE, SYMENT) do
		if (Symbols[i+SFLGS] & FUNDEFD) do
			t.memcopy(name, @Symbols[i+SNAME]::1,
				Symbols[i+SNAME]::0);
			name::(Symbols[i+SNAME]::0) := 0;
			error("undefined symbol", name);
		end
	end
end

pass() do var out;
	Token := scan();
	while (Token \= ENDOFFILE) statement();
	report_undefd();
	if (Errcount) return;
	if (t.write(Outfile, Code, Ctop) \= Ctop)
		fatal("failed to write text segment", 0);
	if (Infile \= T3X.SYSIN) t.close(Infile);
	if (Outfile \= T3X.SYSOUT) t.close(Outfile);
end

var	In::TEXTLEN, Out::TEXTLEN;

do var ki, ko;
	ki := t.getarg(1, In, TEXTLEN-1);
	ko := t.getarg(2, Out, TEXTLEN-1);
	ie (ki > 0) do
		if (ko < 1) do
			if (ki > 8) ki := 8;
			ko := ki;
			t.memcopy(Out, In, ki+1);
			t.memcopy(@In::ki, ".s86", 5);
			t.memcopy(@Out::ko, ".com", 5);
		end
		Infile := t.open(In, T3X.OREAD);
		if (Infile < 0) fatal("no such file", In);
		Outfile := t.open(Out, T3X.OWRITE);
		if (Outfile < 0) fatal("cannot create file", Out);
	end
	else do
		Infile := T3X.SYSIN;
		Outfile := T3X.SYSOUT;
	end
	init();
	pass();
	if (Errcount) do
		if (ko > 0) t.remove(Out);
		halt 1;
	end
end
