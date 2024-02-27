! Convert file to PCDOS text format
! Nils M Holm, 2019,2021,2022
! Public domain / 0BSD

use t3x: t;

const	BUFLEN = 128;

var	Outbuf::BUFLEN;

str_length(s) return t.memscan(s, 0, 32767);

writes(fd, s) do
	t.write(fd, s, str_length(s));
end

nl(fd) do var b::3;
	writes(fd, t.newline(b));
end

aw(s) do
	writes(T3X.SYSERR, s);
	nl(T3X.SYSERR);
	halt 1;
end

var	Inbuf::BUFLEN;
var	Line::BUFLEN;
var	Cp, Ep, More;

readln(fd) do var i, c;
	i := 0;
	while (1) do
		if (Cp >= Ep) do
			Ep := t.read(fd, Inbuf, BUFLEN);
			if (Ep < 1) do
				More := 0;
				leave;
			end
			Cp := 0;
		end
		c := Inbuf::Cp;
		Cp := Cp+1;
		if (c = '\r') loop;
		if (c = '\n') leave;
		if (i >= BUFLEN-1) leave;
		Line::i := c;
		i := i+1;
	end
	Line::i := 0;
	return i;
end

writeln(fd) do
	writes(fd, Line);
	writes(fd, "\r\n");
end

usage() do
	writes(T3X.SYSOUT, "Usage: dosfile infile outfile");
	nl(T3X.SYSOUT);
	writes(T3X.SYSOUT, "       dosfile infile /r (replace file)");
	nl(T3X.SYSOUT);
	halt 1;
end

var infile::BUFLEN, outfile::BUFLEN;

do var infd, outfd, k, n, ren;
	ren := 0;
	if (t.getarg(1, infile, BUFLEN) < 1) usage();
	if (t.getarg(2, outfile, BUFLEN) < 1) usage();
	if (t.memcomp(outfile, "/r", 3) = 0) do
		t.memcopy(outfile, "dosfile.tmp", 12);
		ren := 1;
	end
	infd := t.open(infile, T3X.OREAD);
	if (infd < 0) aw("No file");
	outfd := t.open(outfile, T3X.OWRITE);
	if (outfd < 0) aw("Cannot create file");
	Cp := 0;
	Ep := 0;
	More := %1;
	n := 0;
	k := readln(infd);
	while (More) do
		writeln(outfd);
		n := n + k + 2;
		k := readln(infd);
	end
	t.close(outfd);
	t.close(infd);
	if (ren) do
		t.remove(infile);
		t.rename("dosfile.tmp", infile);
	end
end
