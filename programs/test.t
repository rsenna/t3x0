! T3X/0 test suite
! Nils M Holm, 2019, 2022
! Public domain / 0BSD license

! TODO:
! const/struct declarations
! string/char escape sequences
! compound statements

use t3x: t;

var	Errors;
var	Verbose;

var ntoa_buf::100;

ntoa(x) do var i, k;
	if (x = 0) return "0";
	if (x = 0x8000) return "NAN";
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

str_length(s) return t.memscan(s, 0, 32767);

str_equal(a, b) return 0 = t.memcomp(a, b, str_length(a)+1);

writes(s) t.write(T3X.SYSOUT, s, str_length(s));

nl() do var b::3;
	writes(t.newline(b));
end

log(s)	if (Verbose) do
		writes(s);
		nl();
	end

test(s, r, x) do
	log(s);
	if (r \= x) do
		Errors := Errors + 1;
		writes(s);
		writes(" FAILED, got ");
		writes(ntoa(r));
		writes(", expected ");
		writes(ntoa(x));
		nl();
	end
end

testge(s, r, x) do
	ie (r < x)
		test(s, r, x);
	else
		log(s);
end

stest(s, r, x) do
	log(s);
	if (\str_equal(r, x)) do
		Errors := Errors + 1;
		writes(s);
		writes(" FAILED, got \q");
		writes(r);
		writes("\q, expected \q");
		writes(x);
		writes("\q");
		nl();
	end
end

addr() do var x[1];
	test("@x[0]", @x[0], x);
	test("@x[1]", @x[1], x+t.bpw());
	test("@x[5]", @x[5], x+5*t.bpw());
	test("@x[127]", @x[127], x+127*t.bpw());
	test("@x[128]", @x[128], x+128*t.bpw());
	test("@x[129]", @x[129], x+129*t.bpw());
	test("@x[1234]", @x[1234], x+1234*t.bpw());
	test("@x0]", @x[0], x);
	test("@x::1", @x::1, x+1);
	test("@x::5", @x::5, x+5);
	test("@x::127", @x::127, x+127);
	test("@x::128", @x::128, x+128);
	test("@x::129", @x::129, x+129);
	test("@x::1234", @x::1234, x+1234);
end

unop() do
	test("-1",     -1,         %1);
	test("-0",     -0,         %0);
	test("-12345", -12345, %12345);

	test("~0xffff & 0xffff", ~0xffff & 0xffff, 0x0000);
	test("~0xa5a5 & 0xffff", ~0xa5a5 & 0xffff, 0x5a5a);

	test("\\0",       \0,          %1);
	test("\\1",       \1,           0);
	test("\\%1",      \%1,          0);
	test("\\12345",   \12345,       0);
	test("\\12345",   \12345,       0);
	test("\\%12345",  \%12345,      0);
	test("\\%12345",  \%12345,      0);
	test("\\'x'",     \'x',         0);
	test("\\\qfoo\q", \"foo",       0);
	test("\\[1,2,3]", \[1,2,3],     0);
end

mulop() do
	test(" 123 *  99",  123 *  99,  12177);
	test(" 123 * %99",  123 * %99, %12177);
	test("%123 *  99", %123 *  99, %12177);
	test("%123 * %99", %123 * %99,  12177);

	test(" 12345 /  99",  12345 /  99,  124);
	test(" 12345 / %99",  12345 / %99, %124);
	test("%12345 /  99", %12345 /  99, %124);
	test("%12345 / %99", %12345 / %99,  124);

	test(" 12345 mod  99",  12345 mod  99,     69);
	test(" 12345 mod %99",  12345 mod %99,  12345);
	ie (t.bpw() = 2)
		test("%12345 mod  99", %12345 mod  99,     28);
	else
		test("%12345 mod  99", %12345 mod  99,     34);
	test("%12345 mod %99", %12345 mod %99, %12345);

	test(" 12345 ./  99",  12345 ./  99, 124);
	test(" 12345 ./ %99",  12345 ./ %99,   0);
	if (t.bpw() = 2)
		test("%12345 ./  99", %12345 ./  99, 537);
	test("%12345 ./ %99", %12345 ./ %99,   0);
end

addop() do
	test(" 12345 +  9999",  12345 +  9999,  22344);
	test(" 12345 + %9999",  12345 + %9999,   2346);
	test("%12345 +  9999", %12345 +  9999,  -2346);
	test("%12345 + %9999", %12345 + %9999, -22344);

	test(" 12345 -  9999",  12345 -  9999,   2346);
	test(" 12345 - %9999",  12345 - %9999,  22344);
	test("%12345 -  9999", %12345 -  9999, -22344);
	test("%12345 - %9999", %12345 - %9999,  -2346);

	test(" 12345 +     0",  12345 +     0,  12345);
	test("     0 +  9999",      0 +  9999,   9999);
	test(" 12345 -     0",  12345 -     0,  12345);
	test("     0 -  9999",      0 -  9999,  %9999);
end

bitop() do
	test("0x0c & 0x05", 0x0c & 0x05, 0x04);
	test("0x0c | 0x05", 0x0c | 0x05, 0x0d);
	test("0x0c ^ 0x05", 0x0c ^ 0x05, 0x09);
	test("0x0c << 1",   0x0c << 1,   0x18);
	test("0x0c << 2",   0x0c << 2,   0x30);
	test("0x0c >> 1",   0x0c >> 1,   0x06);
	test("0x0c >> 2",   0x0c >> 2,   0x03);

	test("0xff00 & 0xf0f0", 0xff00 & 0xf0f0, 0xf000);
	test("0xff00 | 0xf0f0", 0xff00 | 0xf0f0, 0xfff0);
	test("0xffff ^ 0xf0f0", 0xffff ^ 0xf0f0, 0x0f0f);
	test("0x0ff0 << 4", 0x0ff0 << 4, 0xff00);
	test("0x0ff0 >> 4", 0x0ff0 >> 4, 0x00ff);
end

relop() do
	test(" 5 <   7",  5 <   7, %1);
	test(" 5 <  %7",  5 <  %7,  0);
	test("%5 <   7", %5 <   7, %1);
	test("%5 <  %7", %5 <  %7,  0);
	test(" 7 <   5",  7 <   5,  0);
	test(" 7 <  %5",  7 <  %5,  0);
	test("%7 <   5", %7 <   5, %1);
	test("%7 <  %5", %7 <  %5, %1);
	test(" 5 <   5",  5 <   5,  0);
	test("%5 <  %5", %5 <  %5,  0);

	test(" 5 >   7",  5 >   7,  0);
	test(" 5 >  %7",  5 >  %7, %1);
	test("%5 >   7", %5 >   7,  0);
	test("%5 >  %7", %5 >  %7, %1);
	test(" 7 >   5",  7 >   5, %1);
	test(" 7 >  %5",  7 >  %5, %1);
	test("%7 >   5", %7 >   5,  0);
	test("%7 >  %5", %7 >  %5,  0);
	test(" 5 >   5",  5 >   5,  0);
	test("%5 >  %5", %5 >  %5,  0);

	test(" 5 <=  7",  5 <=  7, %1);
	test(" 5 <= %7",  5 <= %7,  0);
	test("%5 <=  7", %5 <=  7, %1);
	test("%5 <= %7", %5 <= %7,  0);
	test(" 7 <=  5",  7 <=  5,  0);
	test(" 7 <= %5",  7 <= %5,  0);
	test("%7 <=  5", %7 <=  5, %1);
	test("%7 <= %5", %7 <= %5, %1);
	test(" 5 <=  5",  5 <=  5, %1);
	test("%5 <= %5", %5 <= %5, %1);

	test(" 5 >=  7",  5 >=  7,  0);
	test(" 5 >= %7",  5 >= %7, %1);
	test("%5 >=  7", %5 >=  7,  0);
	test("%5 >= %7", %5 >= %7, %1);
	test(" 7 >=  5",  7 >=  5, %1);
	test(" 7 >= %5",  7 >= %5, %1);
	test("%7 >=  5", %7 >=  5,  0);
	test("%7 >= %5", %7 >= %5,  0);
	test(" 5 >=  5",  5 >=  5, %1);
	test("%5 >= %5", %5 >= %5, %1);

	test(" 5 =  7",  5 =  7,  0);
	test(" 5 = %7",  5 = %7,  0);
	test("%5 =  7", %5 =  7,  0);
	test("%5 = %7", %5 = %7,  0);
	test(" 7 =  5",  7 =  5,  0);
	test(" 7 = %5",  7 = %5,  0);
	test("%7 =  5", %7 =  5,  0);
	test("%7 = %5", %7 = %5,  0);
	test(" 5 =  5",  5 =  5, %1);
	test("%5 = %5", %5 = %5, %1);

	test(" 5 \\=  7",  5 \=  7, %1);
	test(" 5 \\= %7",  5 \= %7, %1);
	test("%5 \\=  7", %5 \=  7, %1);
	test("%5 \\= %7", %5 \= %7, %1);
	test(" 7 \\=  5",  7 \=  5, %1);
	test(" 7 \\= %5",  7 \= %5, %1);
	test("%7 \\=  5", %7 \=  5, %1);
	test("%7 \\= %5", %7 \= %5, %1);
	test(" 5 \\=  5",  5 \=  5,  0);
	test("%5 \\= %5", %5 \= %5,  0);

	test(" 5 .<   7",  5 .<   7, %1);
	test(" 5 .<  %7",  5 .<  %7, %1);
	test("%5 .<   7", %5 .<   7,  0);
	test("%5 .<  %7", %5 .<  %7,  0);
	test(" 7 .<   5",  7 .<   5,  0);
	test(" 7 .<  %5",  7 .<  %5, %1);
	test("%7 .<   5", %7 .<   5,  0);
	test("%7 .<  %5", %7 .<  %5, %1);
	test(" 5 .<   5",  5 .<   5,  0);
	test("%5 .<  %5", %5 .<  %5,  0);

	test(" 5 .>   7",  5 .>   7,  0);
	test(" 5 .>  %7",  5 .>  %7,  0);
	test("%5 .>   7", %5 .>   7, %1);
	test("%5 .>  %7", %5 .>  %7, %1);
	test(" 7 .>   5",  7 .>   5, %1);
	test(" 7 .>  %5",  7 .>  %5,  0);
	test("%7 .>   5", %7 .>   5, %1);
	test("%7 .>  %5", %7 .>  %5,  0);
	test(" 5 .>   5",  5 .>   5,  0);
	test("%5 .>  %5", %5 .>  %5,  0);

	test(" 5 .<=  7",  5 .<=  7, %1);
	test(" 5 .<= %7",  5 .<= %7, %1);
	test("%5 .<=  7", %5 .<=  7,  0);
	test("%5 .<= %7", %5 .<= %7,  0);
	test(" 7 .<=  5",  7 .<=  5,  0);
	test(" 7 .<= %5",  7 .<= %5, %1);
	test("%7 .<=  5", %7 .<=  5,  0);
	test("%7 .<= %5", %7 .<= %5, %1);
	test(" 5 .<=  5",  5 .<=  5, %1);
	test("%5 .<= %5", %5 .<= %5, %1);

	test(" 5 .>=  7",  5 .>=  7,  0);
	test(" 5 .>= %7",  5 .>= %7,  0);
	test("%5 .>=  7", %5 .>=  7, %1);
	test("%5 .>= %7", %5 .>= %7, %1);
	test(" 7 .>=  5",  7 .>=  5, %1);
	test(" 7 .>= %5",  7 .>= %5,  0);
	test("%7 .>=  5", %7 .>=  5, %1);
	test("%7 .>= %5", %7 .>= %5,  0);
	test(" 5 .>=  5",  5 .>=  5, %1);
	test("%5 .>= %5", %5 .>= %5, %1);
end

asrelop() do var x;
	x :=  5 <  7; test("x :=  5 <   7",  x, %1);
	x := %5 <  7; test("x := %5 <   7",  x, %1);
	x :=  5 < %7; test("x :=  5 <  %7",  x,  0);
	x := %5 < %7; test("x := %5 <  %7",  x,  0);
	x :=  7 <  5; test("x :=  7 <   5",  x,  0);
	x :=  7 < %5; test("x :=  7 <  %5",  x,  0);
	x := %7 <  5; test("x := %7 <   5",  x, %1);
	x := %7 < %5; test("x := %7 <  %5",  x, %1);
	x :=  5 <  5; test("x :=  5 <   5",  x,  0);
	x := %5 < %5; test("x := %5 <  %5",  x,  0);

	x :=  5 >  7; test("x :=  5 >   7",  x,  0);
	x := %5 >  7; test("x := %5 >   7",  x,  0);
	x :=  5 > %7; test("x :=  5 >  %7",  x, %1);
	x := %5 > %7; test("x := %5 >  %7",  x, %1);
	x :=  7 >  5; test("x :=  7 >   5",  x, %1);
	x :=  7 > %5; test("x :=  7 >  %5",  x, %1);
	x := %7 >  5; test("x := %7 >   5",  x,  0);
	x := %7 > %5; test("x := %7 >  %5",  x,  0);
	x :=  5 >  5; test("x :=  5 >   5",  x,  0);
	x := %5 > %5; test("x := %5 >  %5",  x,  0);

	x :=  5 <=  7; test("x :=  5 <=   7",  x, %1);
	x := %5 <=  7; test("x := %5 <=   7",  x, %1);
	x :=  5 <= %7; test("x :=  5 <=  %7",  x,  0);
	x := %5 <= %7; test("x := %5 <=  %7",  x,  0);
	x :=  7 <=  5; test("x :=  7 <=   5",  x,  0);
	x :=  7 <= %5; test("x :=  7 <=  %5",  x,  0);
	x := %7 <=  5; test("x := %7 <=   5",  x, %1);
	x := %7 <= %5; test("x := %7 <=  %5",  x, %1);
	x :=  5 <=  5; test("x :=  5 <=   5",  x, %1);
	x := %5 <= %5; test("x := %5 <=  %5",  x, %1);

	x :=  5 >=  7; test("x :=  5 >=   7",  x,  0);
	x := %5 >=  7; test("x := %5 >=   7",  x,  0);
	x :=  5 >= %7; test("x :=  5 >=  %7",  x, %1);
	x := %5 >= %7; test("x := %5 >=  %7",  x, %1);
	x :=  7 >=  5; test("x :=  7 >=   5",  x, %1);
	x :=  7 >= %5; test("x :=  7 >=  %5",  x, %1);
	x := %7 >=  5; test("x := %7 >=   5",  x,  0);
	x := %7 >= %5; test("x := %7 >=  %5",  x,  0);
	x :=  5 >=  5; test("x :=  5 >=   5",  x, %1);
	x := %5 >= %5; test("x := %5 >=  %5",  x, %1);

	x :=  5 =  7; test("x :=  5 =   7",  x,  0);
	x := %5 =  7; test("x := %5 =   7",  x,  0);
	x :=  5 = %7; test("x :=  5 =  %7",  x,  0);
	x := %5 = %7; test("x := %5 =  %7",  x,  0);
	x :=  7 =  5; test("x :=  7 =   5",  x,  0);
	x :=  7 = %5; test("x :=  7 =  %5",  x,  0);
	x := %7 =  5; test("x := %7 =   5",  x,  0);
	x := %7 = %5; test("x := %7 =  %5",  x,  0);
	x :=  5 =  5; test("x :=  5 =   5",  x, %1);
	x := %5 = %5; test("x := %5 =  %5",  x, %1);

	x :=  5 \=  7; test("x :=  5 \\=   7",  x, %1);
	x := %5 \=  7; test("x := %5 \\=   7",  x, %1);
	x :=  5 \= %7; test("x :=  5 \\=  %7",  x, %1);
	x := %5 \= %7; test("x := %5 \\=  %7",  x, %1);
	x :=  7 \=  5; test("x :=  7 \\=   5",  x, %1);
	x :=  7 \= %5; test("x :=  7 \\=  %5",  x, %1);
	x := %7 \=  5; test("x := %7 \\=   5",  x, %1);
	x := %7 \= %5; test("x := %7 \\=  %5",  x, %1);
	x :=  5 \=  5; test("x :=  5 \\=   5",  x,  0);
	x := %5 \= %5; test("x := %5 \\=  %5",  x,  0);

	x :=  5 .<  7; test("x :=  5 .<   7",  x, %1);
	x := %5 .<  7; test("x := %5 .<   7",  x,  0);
	x :=  5 .< %7; test("x :=  5 .<  %7",  x, %1);
	x := %5 .< %7; test("x := %5 .<  %7",  x,  0);
	x :=  7 .<  5; test("x :=  7 .<   5",  x,  0);
	x :=  7 .< %5; test("x :=  7 .<  %5",  x, %1);
	x := %7 .<  5; test("x := %7 .<   5",  x,  0);
	x := %7 .< %5; test("x := %7 .<  %5",  x, %1);
	x :=  5 .<  5; test("x :=  5 .<   5",  x,  0);
	x := %5 .< %5; test("x := %5 .<  %5",  x,  0);

	x :=  5 .>  7; test("x :=  5 .>   7",  x,  0);
	x := %5 .>  7; test("x := %5 .>   7",  x, %1);
	x :=  5 .> %7; test("x :=  5 .>  %7",  x,  0);
	x := %5 .> %7; test("x := %5 .>  %7",  x, %1);
	x :=  7 .>  5; test("x :=  7 .>   5",  x, %1);
	x :=  7 .> %5; test("x :=  7 .>  %5",  x,  0);
	x := %7 .>  5; test("x := %7 .>   5",  x, %1);
	x := %7 .> %5; test("x := %7 .>  %5",  x,  0);
	x :=  5 .>  5; test("x :=  5 .>   5",  x,  0);
	x := %5 .> %5; test("x := %5 .>  %5",  x,  0);

	x :=  5 .<=  7; test("x :=  5 .<=   7",  x, %1);
	x := %5 .<=  7; test("x := %5 .<=   7",  x,  0);
	x :=  5 .<= %7; test("x :=  5 .<=  %7",  x, %1);
	x := %5 .<= %7; test("x := %5 .<=  %7",  x,  0);
	x :=  7 .<=  5; test("x :=  7 .<=   5",  x,  0);
	x :=  7 .<= %5; test("x :=  7 .<=  %5",  x, %1);
	x := %7 .<=  5; test("x := %7 .<=   5",  x,  0);
	x := %7 .<= %5; test("x := %7 .<=  %5",  x, %1);
	x :=  5 .<=  5; test("x :=  5 .<=   5",  x, %1);
	x := %5 .<= %5; test("x := %5 .<=  %5",  x, %1);

	x :=  5 .>=  7; test("x :=  5 .>=   7",  x,  0);
	x := %5 .>=  7; test("x := %5 .>=   7",  x, %1);
	x :=  5 .>= %7; test("x :=  5 .>=  %7",  x,  0);
	x := %5 .>= %7; test("x := %5 .>=  %7",  x, %1);
	x :=  7 .>=  5; test("x :=  7 .>=   5",  x, %1);
	x :=  7 .>= %5; test("x :=  7 .>=  %5",  x,  0);
	x := %7 .>=  5; test("x := %7 .>=   5",  x, %1);
	x := %7 .>= %5; test("x := %7 .>=  %5",  x,  0);
	x :=  5 .>=  5; test("x :=  5 .>=   5",  x, %1);
	x := %5 .>= %5; test("x := %5 .>=  %5",  x, %1);
end

var	E;

S() E:= 1;

logop() do
	test(" 0 /\  0",  0 /\  0,  0);
	test(" 0 /\ %1",  0 /\ %1,  0);
	test("%1 /\  0", %1 /\  0,  0);
	test("%1 /\ %1", %1 /\ %1, %1);

	test(" 0 \/  0",  0 \/  0,  0);
	test(" 0 \/ %1",  0 \/ %1, %1);
	test("%1 \/  0", %1 \/  0, %1);
	test("%1 \/ %1", %1 \/ %1, %1);

	E := 0; if ( 0 /\ S()); test(" 0 /\ S()", E, 0);
	E := 0; if (%1 \/ S()); test("%1 \/ S()", E, 0);

	test(" 0-> 1: 2",  0-> 1: 2, 2);
	test("%1-> 1: 2", %1-> 1: 2, 1);
	E := 0; if ( 0-> S(): 1); test(" 0-> S(): 1", E, 0);
	E := 0; if (%1-> 1: S()); test("%1-> 1: S()", E, 0);
end

prec() do var v;
	test("203 mod 89 mod 13", 203 mod 89 mod 13, 12);

	test(" 18 / 3 / 2", 18 / 3 / 2,  3);
	test("  2 - 3 - 4",  2 - 3 - 4, %5);

	test("%1 = 2 <  3", %1 = 2 <  3, %1);
	test("%1 = 2 <= 3", %1 = 2 <= 3, %1);
	test(" 0 = 2 >  3",  0 = 2 >  3, %1);
	test(" 0 = 2 >= 3",  0 = 2 >= 3, %1);
	test("5 \= 2 <  3", 5 \= 2 <  3, %1);
	test("5 \= 2 <= 3", 5 \= 2 <= 3, %1);
	test("5 \= 2 >  3", 5 \= 2 >  3, %1);
	test("5 \= 2 >= 3", 5 \= 2 >= 3, %1);

	test("0 < 2  | 3", 0 < 2  | 3, %1);
	test("0 < 2  & 3", 0 < 2  & 3, %1);
	test("0 < 2  ^ 3", 0 < 2  ^ 3, %1);
	test("0 < 2 << 3", 0 < 2 << 3, %1);
	test("0 < 8 >> 2", 0 < 8 >> 2, %1);

	test("0 <= 2  | 3", 0 <= 2  | 3, %1);
	test("0 <= 2  & 3", 0 <= 2  & 3, %1);
	test("0 <= 2  ^ 3", 0 <= 2  ^ 3, %1);
	test("0 <= 2 << 3", 0 <= 2 << 3, %1);
	test("0 <= 8 >> 2", 0 <= 8 >> 2, %1);

	test("2  | 3 >  0", 2  | 3 >  0, %1);
	test("2  & 3 >  0", 2  & 3 >  0, %1);
	test("2  ^ 3 >  0", 2  ^ 3 >  0, %1);
	test("2 << 3 >  0", 2 << 3 >  0, %1);
	test("8 >> 2 >  0", 8 >> 2 >  0, %1);

	test("2  | 3 >= 0", 2  | 3 >= 0, %1);
	test("2  & 3 >= 0", 2  & 3 >= 0, %1);
	test("2  ^ 3 >= 0", 2  ^ 3 >= 0, %1);
	test("2 << 3 >= 0", 2 << 3 >= 0, %1);
	test("8 >> 2 >= 0", 8 >> 2 >= 0, %1);

	test("2 | 3 + 5", 2 | 3 + 5, 10);
	test("2 & 3 + 5", 2 | 3 + 5, 10);
	test("2 ^ 3 + 5", 2 ^ 3 + 5, 10);

	test("   2 << 3 + 5",    2 << 3 + 5, 512);
	test("1024 >> 3 + 5", 1024 >> 3 + 5,   4);

	test(" 2 + 3 * 4",   2 + 3 * 4,  14);
	test(" 2 - 3 * 4",   2 - 3 * 4, %10);
	test("10 + 12 / 3", 10 + 12 / 3, 14);
	test("10 - 12 / 3", 10 - 12 / 3,  6);

	v := packed [2,3,5,7,11];
	test("v :: 2  = 1", v :: 2  = 1,  0);
	test("v :: 2 \= 1", v :: 2 \= 1, %1);
	test("v :: 2  < 1", v :: 2  < 1,  0);
	test("v :: 2 <= 1", v :: 2 <= 1,  0);
	test("v :: 2  > 1", v :: 2  > 1, %1);
	test("v :: 2 >= 1", v :: 2 >= 1, %1);
	test("v :: 2  | 1", v :: 2  | 1,  5);
	test("v :: 2  & 1", v :: 2  & 1,  1);
	test("v :: 2  ^ 1", v :: 2  ^ 1,  4);
	test("v :: 2 << 1", v :: 2 << 1, 10);
	test("v :: 2 >> 1", v :: 2 >> 1,  2);
	test("v :: 2  + 1", v :: 2  + 1,  6);
	test("v :: 2  - 1", v :: 2  - 1,  4);
	test("v :: 2  * 2", v :: 2  * 2, 10);
	test("v :: 2  / 2", v :: 2  / 2,  2);
end

tables() do var v, i, j, k;
	v := [123, 456, 789];
	test("v[0]", v[0], 123);
	test("v[1]", v[1], 456);
	test("v[2]", v[2], 789);

	v := [[1, 2, 3],
	      [4, 5, 6],
	      [7, 8, 9]];
	test("v[0][0]", v[0][0], 1);
	test("v[0][1]", v[0][1], 2);
	test("v[0][2]", v[0][2], 3);
	test("v[1][0]", v[1][0], 4);
	test("v[1][1]", v[1][1], 5);
	test("v[1][2]", v[1][2], 6);
	test("v[2][0]", v[2][0], 7);
	test("v[2][1]", v[2][1], 8);
	test("v[2][2]", v[2][2], 9);

	v := [[[ 1,  2,  3],
	       [ 4,  5,  6],
	       [ 7,  8,  9]],
	      [[11, 12, 13],
	       [14, 15, 16],
	       [17, 18, 19]],
	      [[21, 22, 23],
	       [24, 25, 26],
	       [27, 28, 29]]];
	test("v[0][0][0]", v[0][0][0], 1);
	test("v[0][0][1]", v[0][0][1], 2);
	test("v[0][0][2]", v[0][0][2], 3);
	test("v[0][1][0]", v[0][1][0], 4);
	test("v[0][1][1]", v[0][1][1], 5);
	test("v[0][1][2]", v[0][1][2], 6);
	test("v[0][2][0]", v[0][2][0], 7);
	test("v[0][2][1]", v[0][2][1], 8);
	test("v[0][2][2]", v[0][2][2], 9);
	test("v[1][0][0]", v[1][0][0], 11);
	test("v[1][0][1]", v[1][0][1], 12);
	test("v[1][0][2]", v[1][0][2], 13);
	test("v[1][1][0]", v[1][1][0], 14);
	test("v[1][1][1]", v[1][1][1], 15);
	test("v[1][1][2]", v[1][1][2], 16);
	test("v[1][2][0]", v[1][2][0], 17);
	test("v[1][2][1]", v[1][2][1], 18);
	test("v[1][2][2]", v[1][2][2], 19);
	test("v[2][0][0]", v[2][0][0], 21);
	test("v[2][0][1]", v[2][0][1], 22);
	test("v[2][0][2]", v[2][0][2], 23);
	test("v[2][1][0]", v[2][1][0], 24);
	test("v[2][1][1]", v[2][1][1], 25);
	test("v[2][1][2]", v[2][1][2], 26);
	test("v[2][2][0]", v[2][2][0], 27);
	test("v[2][2][1]", v[2][2][1], 28);
	test("v[2][2][2]", v[2][2][2], 29);

	for (i=0, 3)
		for (j=0, 3)
			for (k=0, 3) do
				v[i][j][k] := 123;
				test("v[i][j][k]", v[i][j][k], 123);
			end

	v := packed [11, 22, 33, 44, 55];
	test("v::0", v::0, 11);
	test("v::1", v::1, 22);
	test("v::2", v::2, 33);
	test("v::3", v::3, 44);
	test("v::4", v::4, 55);

	v := "abcde";
	test("v::0", v::0, 'a');
	test("v::1", v::1, 'b');
	test("v::2", v::2, 'c');
	test("v::3", v::3, 'd');
	test("v::4", v::4, 'e');
	test("v::4", v::5, 0);

	v := ["abc", "def", "ghi"];
	test("v[0]::0", v[0]::0, 'a');
	test("v[0]::1", v[0]::1, 'b');
	test("v[0]::2", v[0]::2, 'c');
	test("v[1]::0", v[1]::0, 'd');
	test("v[1]::1", v[1]::1, 'e');
	test("v[1]::2", v[1]::2, 'f');
	test("v[2]::0", v[2]::0, 'g');
	test("v[2]::1", v[2]::1, 'h');
	test("v[2]::2", v[2]::2, 'i');

	for (i=0, 3)
		for (j=0, 3) do
			v[i]::j := 'x';
			test("v[i]::j", v[i]::j, 'x');
		end

end

cond() do var x, i;
	x := 0; if (1) x := 1; test("if (1) ...", x, 1);
	x := 0; if (0) x := 1; test("if (0) ...", x, 0);

	x := 0; if (\1) x := 1; test("if (\1) ...", x, 0);
	x := 0; if (\0) x := 1; test("if (\0) ...", x, 1);

	x := 0; ie (1) x := 1; else x := 2; test("ie (1) ...", x, 1);
	x := 0; ie (0) x := 1; else x := 2; test("ie (0) ...", x, 2);

	x := 0; for (i=0, 10) x := x+1;
	test("for (i=0, 10) ...", x, 10);
	x := 0; for (i=10, 0, %1) x := x+1;
	test("for (i=10, 0, %1) ...", x, 10);

	for (i=0, 100, 7); test("for (i=0, 100, 7);", i, 105);
	for (i=100, 0, %7); test("for (i=100, 0, %7);", i, %5);

	x := 0; while (x<10) x := x+1;
	test("while (x<10) ...", x, 10);

	x := 0;
	for (i=0, 10) do
		if (i < 5) loop;
		x := x+1;
	end
	test("for (i=0, 10) ... loop", x, 5);
	x := 0;
	for (i=0, 10) do
		if (i = 7) leave;
		x := x+1;
	end
	test("for (i=0, 10) ... leave", x, 7);

	x := 0;
	i := 0; while (i < 10) do
		i := i+1;
		if (i <= 5) loop;
		x := x+1;
	end
	test("while (i < 10) ... loop", x, 5);
	i := 0; while (i < 10) do
		if (i = 7) leave;
		i := i+1;
	end
	test("while (i < 10) ... leave", i, 7);
end

a0() return;

a1(x) return x;

a2(x, y) return x+y;

a3(x, y, z) return x+y+z;

ack(x, y)
	ie (x = 0) return y+1;
	else ie (x > 0 /\ y = 0) return ack(x-1, 1);
	else return ack(x-1, ack(x, y-1));

r() return E;
s2(a, b) return b;

fa1() do var x; x := 5; return x; end

fa2(x) do var y; y := 7; return y; end

fa3() do var y; y := 11; return y; end

fa4() do var y; end

fa5() do end

fa6() do return; end

fa7() return;

sum(k, v) do var i, n;
	n := 0;
	for (i=0, k) n := n + v[i];
	return n;
end

proc() do var p, q;
	test("a0()", a0(), 0);
	test("a1(12345)", a1(12345), 12345);
	test("a2(123, 456)", a2(123, 456), 579);
	test("a3(123, 456, 789)", a3(123, 456, 789), 1368);

	test("a2(a1(5), a1(7))", a2(a1(5), a1(7)), 12);

	test("ack(3,3)", ack(3,3), 61);

	E := 0; test("s2(s(), r())", s2(s(), r()), 1);

	test("sum(0, [0])", sum(0, [0]), 0);
	test("sum(1, [1])", sum(1, [1]), 1);
	test("sum(5, [1,2,3,4,5])", sum(5, [1,2,3,4,5]), 15);

	test("call a0()", call a0(), 0);
	call test("call a0()", call a0(), 0);

	p := @a0; test("call p()", call p(), 0);
	p := @a1; test("call p(12345)", call p(12345), 12345);
	p := @a2; test("call p(123, 456)", call p(123, 456), 579);
	p := @a3; test("call p(123, 456, 789)", call p(123, 456, 789), 1368);

	p := @a1; q := @a2;
	test("call q(call p(5), call p(7))",
	      call q(call p(5), call p(7)), 12);

	p := @s2; q := @r;
	E := 0; test("call p(s(), call q())", call p(s(), call q()), 1);

	p := @sum;
	test("call p(0, [0])", call p(0, [0]), 0);
	test("call p(1, [1])", call p(1, [1]), 1);
	test("call p(5, [1,2,3,4,5])", call p(5, [1,2,3,4,5]), 15);

	test("fa1()", fa1(), 5);
	test("fa2(0)", fa2(0), 7);
	test("fa3()", fa3(), 11);
	test("fa4()", fa4(), 0);
	test("fa5()", fa5(), 0);
	test("fa6()", fa6(), 0);
	test("fa7()", fa7(), 0);
end

memory() do var v;
!	test("t.bpw()", t.bpw(), 2);
	test("t.memcomp(\qabc\q,\qabc\q,3)", t.memcomp("abc","abc",3),  0);
	test("t.memcomp(\qabd\q,\qabc\q,3)", t.memcomp("abd","abc",3),  1);
	test("t.memcomp(\qabc\q,\qabd\q,3)", t.memcomp("abc","abd",3), %1);
	test("t.memcomp(\qabc\q,\qabd\q,2)", t.memcomp("abc","abd",2),  0);
	v := "0123456789";
	t.memcopy(@v::2, @v::1, 5);
	stest("t.memcopy(@v::2, @v::1, 5)", v, "0112345789");
	v := "0123456789";
	t.memcopy(@v::1, @v::2, 5);
	stest("t.memcopy(@v::2, @v::1, 5)", v, "0234566789");
	v := "0123456789";
	t.memcopy(@v::2, @v::2, 5);
	stest("t.memcopy(@v::2, @v::2, 5)", v, "0123456789");
	v := "0123456789";
	t.memcopy(@v::2, @v::2, 0);
	stest("t.memcopy(@v::1, @v::2, 0)", v, "0123456789");
	v := "0123456789";
	t.memfill(@v::2, '_', 5);
	stest("t.memfill(@v::2, '_', 5)", v, "01_____789");
	v := "0123456789";
	test("t.memscan(v, '5', 10)", t.memscan(v, '5', 10),  5);
	test("t.memscan(v, 'X', 10)", t.memscan(v, 'X', 10), %1);
	test("t.memscan(v, '9', 10)", t.memscan(v, '9', 10),  9);
	test("t.memscan(v, '9',  9)", t.memscan(v, '9',  9), %1);
end

var	Buf::256;

files() do var fd, i, alpha;
	alpha := "abcdefghijklmnopqrstuvwxyz\r\n";
	fd := t.open("test.tmp", T3X.OWRITE);
	testge("t.open(\qtest.tmp\q, OWRITE)", fd, 3);
	for (i=0, 20)
		test("t.write(fd, alpha, 28)",
			t.write(fd, alpha, 28), 28);
	test("t.close(fd)", t.close(fd), 0);

	fd := t.open("test.tmp", T3X.OREAD);
	testge("t.open(\qtest.tmp\q, OREAD)", fd, 3);
	for (i=0, 20) do
		test("t.read(fd, Buf, 28)",
			t.read(fd, Buf, 28), 28);
		test("memcomp(buf, alpha)",
			t.memcomp(Buf, alpha, 28), 0);
	end
	test("t.close(fd)", t.close(fd), 0);

	t.remove("test2.tmp");

	test("t.rename(...)", t.rename("test.tmp", "test2.tmp"), 0);
	test("t.remove(...)", t.remove("test2.tmp"), 0);
	test("t.remove(...)", t.remove("test2.tmp"), %1);

	fd := t.open("test2.tmp", T3X.OWRITE);
	test("t.write(fd, Buf, 0)", t.write(fd, Buf, 0), 0);
	t.close(fd);
	fd := t.open("test2.tmp", T3X.OREAD);
	testge("t.open(\qtest2.tmp\q, OREAD)", fd, 3);
	test("t.read(fd, Buf, 0)", t.read(fd, Buf, 0), 0);
	test("t.read(fd, Buf, 1)", t.read(fd, Buf, 1), 0);
	t.close(fd);
	t.remove("test2.tmp");

	fd := t.create("test.tmp");
	testge("t.create(\qtest.tmp\q);", fd, 3);
	t.write(fd, "01234", 5);
	t.close(fd);
	fd := t.open("test.tmp", T3X.OAPPND);
	testge("t.open(\qtest.tmp\q, T3X.OAPPND);", fd, 3);
	t.write(fd, "56789", 5);
	t.close(fd);
	fd := t.open("test.tmp", T3X.OREAD);
	t.read(fd, Buf, 10);
	t.close(fd);
	test("t.open(\qtest.tmp\q, T3X.OAPPND)",
		t.memcomp(Buf, "0123456789", 10), 0);
end

checkpos(ts, fd, n) do var i;
	log(ts);
	t.read(fd, Buf, 256);
	for (i=0, 256)
		if (Buf::i \= n) do
			test(ts, n, Buf::i);
			leave;
		end
end

! Remove this procedure under CP/M
randio() do var fd, i, j;
	fd := t.open("test.tmp", T3X.OWRITE);
	testge("t.open(\qtest.tmp\q, T3X.OWRITE)", fd, 3);
	t.close(fd);
	fd := t.open("test.tmp", T3X.ORDWR);
	testge("t.open(\qtest.tmp\q, T3X.ORDWR)", fd, 3);
	for (i=0, 256) do
		t.memfill(Buf, i, 256);
		t.write(fd, Buf, 256);
	end
	for (i=0, 256, 16) do
		t.seek(fd, i*256, T3X.SEEK_SET);
		checkpos("t.seek(fd, i*256, T3X.SEEK_SET)", fd, i);
	end
	for (i=1, 257, 16) do
		t.seek(fd, i*256, T3X.SEEK_END);
		checkpos("t.seek(fd, i*256, T3X.SEEK_END)", fd, 256-i);
	end
	for (i=0, 256, 16) do
		t.seek(fd, 0, T3X.SEEK_SET);
		for (j=0, i) t.seek(fd, 256, T3X.SEEK_FWD);
		checkpos("t.seek(fd, 256, T3X.SEEK_FWD)", fd, i);
	end
	for (i=1, 257, 16) do
		t.seek(fd, 0, T3X.SEEK_END);
		for (j=0, i) t.seek(fd, 256, T3X.SEEK_BCK);
		checkpos("t.seek(fd, 256, T3X.SEEK_BCK)", fd, 256-i);
	end
	t.close(fd);
	t.remove("test.tmp");
end

var	abuf::128;

argtest() do var i, k;
	for (i=1, 64) do
		k := t.getarg(i, abuf, 128);
		if (k < 1) leave;
		writes("Argument #");
		writes(ntoa(i));
		writes(" is \q");
		writes(abuf);
		writes("\q, length ");
		writes(ntoa(k));
		nl();
	end
end

do
	Errors := 0;
	Verbose := 0;
	if (t.getarg(1, abuf, 3) > 0 /\ \t.memcomp(abuf, "/V", 3))
		Verbose := 1;
	addr();
	unop();
	mulop();
	addop();
	bitop();
	relop();
	asrelop();
	logop();
	prec();
	tables();
	cond();
	proc();
	memory();
	files();
	randio();	! Remove on CP/M
	argtest();
	ie (Errors) do
		writes(ntoa(Errors));
		writes(" errors");
	end
	else do
		writes("Looks good!");
	end
	nl();
	halt;
end
