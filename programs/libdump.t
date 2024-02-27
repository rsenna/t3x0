! Convert library image to table of byte vectors
! Nils M Holm, 2019,2021,2022
! Public domain / 0BSD

use t3x: t;

const	BUFLEN = 128;

var	Inbuf::BUFLEN;

str_length(s) return t.memscan(s, 0, 32767);

writes(fd, s) t.write(fd, s, str_length(s));

nl(fd) do var b::3;
	writes(fd, t.newline(b));
end

aw(s) do
	writes(T3X.SYSERR, s);
	nl(T3X.SYSERR);
	halt 1;
end

wrnib(fd, n) do var c::1;
	ie (n > 9)
		c::0 := n+'a'-10;
	else
		c::0 := n+'0';
	t.write(fd, c, 1);
end

wrbyte(fd, n) do
	wrnib(fd, n>>4);
	wrnib(fd, n&15);
end

copylib(lfd, tfd) do var k, i, j, n, len;
	len := 32767;
	writes(tfd, "\t[ ");
	k := t.read(lfd, Inbuf, BUFLEN);
	if (k > 4) len := Inbuf::3 + (Inbuf::4 << 8);
	!!! len := len-100;	! S86 assembler inserts wrong length
	j := 0;
	while (k > 0) do
		if (j + k > len) k := len - j;
		writes(tfd, "0x");
		wrbyte(tfd, k >> 8);
		wrbyte(tfd, k & 255);
		writes(tfd, ",");
		nl(tfd);
		writes(tfd, "\t  packed [ ");
		nl(tfd);
		writes(tfd, "\t    ");
		n := 0;
		for (i=0, k) do
			n := n+1;
			j := j+1;
			ie (n > 12) do
				nl(tfd);
				writes(tfd, "\t    0x");
				n := 1;
			end
			else do
				writes(tfd, "0x");
			end
			wrbyte(tfd, Inbuf::i);
			if (i < k-1) writes(tfd, ",");
		end
		writes(tfd, " ],");
		nl(tfd);
		writes(tfd, "\t  ");
		if (j >= len) leave;
		k := t.read(lfd, Inbuf, BUFLEN);
	end
	writes(tfd, "0 ];");
	nl(tfd);
end

do var infile::15, outfile::15, sfd, dfd, k;
	if (	t.getarg(1, infile, 15) < 0 \/
		t.getarg(2, outfile, 15) < 0
	)
		aw("Usage: libdump infile outfile");
	sfd := t.open(infile, T3X.OREAD);
	if (sfd < 0) aw("libdump: input file missing");
	dfd := t.open(outfile, T3X.OWRITE);
	if (dfd < 0) aw("libdump: cannot create output file");
	copylib(sfd, dfd);
	t.close(dfd);
	t.close(sfd);
end
