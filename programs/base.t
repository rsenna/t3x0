! Numeric base converter
! Nils M Holm, 2023
! Public Domain / 0BSD License

use t3x: t;
use string: str;
use io;

usage() do
	io.fwriteln(T3X.SYSERR, "Usage: base {2,8,10,16} number");
	halt 1;
end

pad(s, k) do var i, n;
	n := str.length(s);
	for (i=n, k) io.writes("\s");
	return s;
end

do var a::100, b, n, i, b4;
	if (t.getarg(2, a, 100) < 1 \/
	    t.getarg(1, a, 100) < 1)
	do
		usage();
	end
	b := str.aton(a, 10, 0);
	if (b < 2 \/ b > 16) usage();
	i := 2;
	b4 := t.bpw() = 2-> 1 : 2;
	io.writes(pad("hex", 4*b4));
	io.writes(pad("dec", 1+5*b4));
	io.writes(pad("oct", 1+6*b4));
	io.writes(pad("bin", 1+16*b4));
	io.nl();
	while (t.getarg(i, a, 100) >= 1) do
		n := str.aton(str.upcase(a), b, 0);
		io.writes(pad(str.ntoa(n, 16), 4*b4));
		io.writes(pad(str.ntoa(n, 10), 1+5*b4));
		io.writes(pad(str.ntoa(n, 8), 1+6*b4));
		io.writes(pad(str.ntoa(n, 2), 1+16*b4));
		io.nl();
		i := i+1;
	end
end
