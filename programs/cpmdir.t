! List a CP/M directory with file sizes
! Nils M Holm, 2023
! Public Domain / 0BSD License

! This program works on CP/M only.

use t3x: t;
use cpm;

const	MAXFILES = 1024;

var	Fcb::CPM.FCB;
var	Buf::128;
var	Files::MAXFILES*11;
var	Ptrs[MAXFILES];

length(s) return t.memscan(s, 0, 32767);

writes(s) t.write(T3X.SYSOUT, s, length(s));

nl() do var b::3;
	writes(t.newline(b));
end

var ntoa_buf::10;
ntoa(x) do var i;
	if (x = 0) return "0";
	ntoa_buf::9 := 0;
	i := 9;
	while (x) do
		i := i-1;
		ntoa_buf::i := x mod 10 + '0';
		x := x / 10;
	end
	return @ntoa_buf::i;
end

sortdir(k) do var i, j, tmp, sw;
	for (i=1, k) do
		sw := 0;
		for (j=0, k-i) do
			if (t.memcomp(Ptrs[j], Ptrs[j+1], 11) > 0) do
				tmp := Ptrs[j];
				Ptrs[j] := Ptrs[j+1];
				Ptrs[j+1] := tmp;
				sw := 1;
			end
		end
		if (\sw) leave;
	end
end

readdir(match) do var k, n;
	cpm.expandfn(match, Fcb);
	cpm.setdma(Buf);
	k := 0;
	n := cpm.search(Fcb);
	while (k < MAXFILES /\ n \= 255) do
		Ptrs[k] := @Files::(k*11);
		t.memcopy(Ptrs[k], @Buf::(1+n*32), 11);
		k := k+1;
		n := cpm.searchnext();
	end
	return k;
end

filesize(i) do
	t.memcopy(@Fcb::CPM.FCB_NAME, Ptrs[i], 11);
	cpm.getfsiz(Fcb);
	! ignoring FCB_RANOVF
	return	(Fcb::CPM.FCB_RANRECL +
		 Fcb::CPM.FCB_RANRECH * 256 + 7) / 8;
end

printfile(i) do var n, j, s;
	n := filesize(i);
	s := ntoa(n);
	for (j = length(s), 4) writes("\s");
	writes(s);
	writes("K\s\s");
	t.write(T3X.SYSOUT, Ptrs[i], 8);
	writes("\s");
	t.write(T3X.SYSOUT, Ptrs[i]+8, 3);
end

const COLS = 3;

printdir(k) do var i, j, m, n;
	ie (k mod COLS)
		n := k / COLS + 1;
	else
		n := k / COLS;
	i := 0;
	for (i=0, n) do
		m := i;
		for (j=0, COLS) do
			if (m < k) do
				printfile(m);
				if (m+n < k) writes("  : ");
			end
			m := m + n;
		end
		nl();
	end
end

upcase(c) return 'a' <= c /\ c <= 'z'-> c-32: c;

expmatch(s, b) do var i, j;
	i := 0;
	if (s::1 = ':') do
		b::0 := upcase(s::0);
		b::1 := ':';
		b := b+2;
		ie (s::2 = 0)
			s := "*.*";
		else
			i := 2;
	end
	for (j=0, 8) do
		     ie (s::i = '*') b::j := '?';
		else ie (s::i = '.') b::j := '\s';
		else ie (s::i = 0)   b::j := '\s';
		else do
			b::j := upcase(s::i);
			i := i+1;
		end
	end
	if (s::i = '*') i := i+1;
	if (s::i = '.') i := i+1;
	b::8 := '.';
	for (j=9, 12) do
		     ie (s::i = '*') b::j := '?';
		else ie (s::i = 0)   b::j := '\s';
		else do
			b::j := upcase(s::i);
			i := i+1;
		end
	end
	b::j := 0;
end

do var arg::15, match::15, k, b::2;
	t.memcopy(match, "????????.???", 13);
	if (t.getarg(1, arg, 15) >= 0)
		expmatch(arg, match);
	k := readdir(match);
	sortdir(k);
	printdir(k);
end
