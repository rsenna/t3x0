! Tcode/0 disassembler
! Nils M Holm, 2017,2022
! Public Domain / 0BSD License

use t3x: t;

const	BUFLEN = 512;

var	Infile;
var	Inbuf::BUFLEN, Ip, Ep;
var	More;
var	Pos;

var	ntoa_buf::100;

ntox(x, n) do var i, d;
	d := "0123456789ABCDEF";
	i := 99;
	ntoa_buf::i := 0;
	while (x > 0 \/ i = 99) do
		i := i-1;
		ntoa_buf::i := d::(x mod 16);
		x := x>>4;
	end
	while (i > 99-n*2) do
		i := i-1;
		ntoa_buf::i := '0';
	end
	return @ntoa_buf::i;
end

ntob(x) return ntox(x, 1);
ntow(x) return ntox(x, 2);

str_length(s) return t.memscan(s, 0, 32767);

str_copy(sd, ss) t.memcopy(sd, ss, str_length(ss)+1);

str_append(sd, ss) t.memcopy(@sd::str_length(sd), ss, str_length(ss)+1);

str_equal(s1, s2) return t.memcomp(s1, s2, str_length(s1)+1) = 0;

writes(s) t.write(1, s, str_length(s));

nl() do var b::3;
	writes(t.newline(b));
end

aw(m, s) do
	writes("Error: ");
	writes(ntow(Pos-1));
	writes(": ");
	writes(m);
	if (s) do
		writes(": ");
		writes(s);
	end
	nl();
	halt 1;
end

rdch() do
	if (Ip >= Ep) do
		Ep := t.read(Infile, Inbuf, BUFLEN);
		if (Ep < 1) More := 0;
		Ip := 0;
	end
	Pos := Pos+1;
	Ip := Ip+1;
	return Inbuf::(Ip-1);
end

rdwd() do var w;
	w := rdch();
	return rdch() << 8 | w;
end

rdmagic() do
	if (	rdch() \= 'T' \/
		rdch() \= '3' \/
		rdch() \= 'X' \/
		rdch() \= '0'
	)
		aw("Not a Tcode/0 program", 0);
	rdwd();
	Pos := 0;
end

wri0(s) do var i;
	writes(s);
	for (i=str_length(s), 9) writes("\s");
end

wri1(s) do
	wri0(s);
	writes(ntox(rdwd(), 2));
end

wri1b(s) do
	wri0(s);
	writes(ntox(rdch(), 1));
end

databyte(x) do var s::3;
	ie (32 < x /\ x < 127) do
		s::0 := x;
		s::1 := '\s';
		s::2 := 0;
		writes(s);
	end
	else do
		writes(ntob(x));
	end
end

var	DB::8, Dp;

flushdata() do var i, b::2;
	for (i=0, Dp) do
		writes("\s");
		writes(ntob(DB::i));
	end
	for (i=Dp, 8) writes("\s\s\s");
	writes("\s\s");
	b::1 := 0;
	for (i=0, Dp) do
		ie (DB::i >= 32 /\ DB::i < 127)
			b::0 := DB::i;
		else
			b::0 := '.';
		writes(b);
	end
	Dp := 0;
end

data() do var i;
	if (\Dp) do
		nl();
		writes(ntow(Pos));
		for (i=0, 9) writes("\s");
	end
	DB::Dp := rdch();
	Dp := Dp+1;
	if (Dp > 7) flushdata();
end

wri1br(s) do var k, i;
	wri0(s);
	k := rdch();
	writes(ntow(Pos+k));
	Dp := 0;
	for (i=0, k) data();
	flushdata();
end

skip() do var k;
	wri0("skip");
	k := rdwd();
	writes(ntow(k));
	Dp := 0;
	while (Pos < k) data();
	flushdata();
end

decode() do var i, a, b::10;
	rdmagic();
	i := rdch();
	while (More) do
		writes(ntow(Pos-1));
		writes("\s");
		     ie (i = 0x01) wri0("push");
		else ie (i = 0x02) wri0("clear");
		else ie (i = 0x03) wri0("drop");
		else ie (i = 0x04) wri1("ldval");
		else ie (i = 0x05) wri1("ldaddr");
		else ie (i = 0x06) wri1("ldlref");
		else ie (i = 0x07) wri1("ldglob");
		else ie (i = 0x08) wri1("ldlocl");
		else ie (i = 0x09) wri1("stglob");
		else ie (i = 0x0a) wri1("stlocl");
		else ie (i = 0x0b) wri0("stindr");
		else ie (i = 0x0c) wri0("stindb");
		else ie (i = 0x0d) wri1("incglob");
		else ie (i = 0x0e) wri1("inclocl");
		else ie (i = 0x0f) wri1("incr");
		else ie (i = 0x10) wri1("stack");
		else ie (i = 0x11) wri1("unstack");
		else ie (i = 0x12) wri0("loclvec");
		else ie (i = 0x13) wri1("globvec");
		else ie (i = 0x14) wri0("index");
		else ie (i = 0x15) wri0("deref");
		else ie (i = 0x16) wri0("indxb");
		else ie (i = 0x17) wri0("drefb");
		else ie (i = 0x18) wri1("call");
		else ie (i = 0x19) wri0("calr");
		else ie (i = 0x1a) wri1("jump");
		else ie (i = 0x1b) wri1br("rjump");
		else ie (i = 0x1c) wri1("jmpfalse");
		else ie (i = 0x1d) wri1("jmptrue");
		else ie (i = 0x1e) wri1("for");
		else ie (i = 0x1f) wri1("fordown");
		else ie (i = 0x20) wri0("mkframe");
		else ie (i = 0x21) wri0("delframe");
		else ie (i = 0x22) wri0("ret");
		else ie (i = 0x23) wri1("halt");
		else ie (i = 0x24) wri0("neg");
		else ie (i = 0x25) wri0("inv");
		else ie (i = 0x26) wri0("lognot");
		else ie (i = 0x27) wri0("add");
		else ie (i = 0x28) wri0("sub");
		else ie (i = 0x29) wri0("mul");
		else ie (i = 0x2a) wri0("div");
		else ie (i = 0x2b) wri0("mod");
		else ie (i = 0x2c) wri0("and");
		else ie (i = 0x2d) wri0("or");
		else ie (i = 0x2e) wri0("xor");
		else ie (i = 0x2f) wri0("shl");
		else ie (i = 0x30) wri0("shr");
		else ie (i = 0x31) wri0("eq");
		else ie (i = 0x32) wri0("neq");
		else ie (i = 0x33) wri0("lt");
		else ie (i = 0x34) wri0("gt");
		else ie (i = 0x35) wri0("lte");
		else ie (i = 0x36) wri0("gte");
		else ie (i = 0x37) wri0("umul");
		else ie (i = 0x38) wri0("udiv");
		else ie (i = 0x39) wri0("ult");
		else ie (i = 0x3a) wri0("ugt");
		else ie (i = 0x3b) wri0("ulte");
		else ie (i = 0x3c) wri0("ugte");
		else ie (i = 0x47) skip();
		else ie (i = 0x80) wri1b("libcall");
		else do
			t.memcopy(b, ntob(i), 10);
			aw("unknwon instruction", b);
		end
		nl();
		i := rdch();
	end
end

init() do
	Ip := 0;
	Ep := 0;
	Pos := 0;
	More := 1;
end

do var in::80, k;
	init();
	k := t.getarg(1, in, 14);
	if (k = %1) aw("Missing file name", 0);
	Infile := t.open(in, T3X.OREAD);
	if (Infile < 0) do
		if (k < 77) t.memcopy(@in::k, ".tc", 4);
		Infile := t.open(in, T3X.OREAD);
		if (Infile < 0)
			aw("No file", in);
	end
	decode();
	t.close(in);
end
