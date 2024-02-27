! Simple hex dump program
! Nils M Holm, 2023
! Public domain / 0BSD license

use t3x: t;

length(s) return t.memscan(s, 0, 32767);

writes(s) t.write(T3X.SYSOUT, s, length(s));

writec(c) do var b::10;
	b::0 := c;
	t.write(T3X.SYSOUT, b, 1);
end

prhex(nibble) do
	nibble := nibble & 0x0F;
	writec(nibble < 10-> '0' + nibble: 'A' + nibble - 10);
end

prbyte(byte) do
	prhex(byte >> 4);
	prhex(byte);
end

prword(word) do
	prbyte(word >> 8);
	prbyte(word);
end

error(txt) do var b::3;
	writes(txt);
	writes(t.newline(b));
	halt 1;
end

var	Fname::128;

do var i, k, offset, fd, buffer::16;
	if (t.getarg(1, Fname, 128) < 0)
		error("Usage: hexdump file");
	fd := t.open(fname, T3X.OREAD);
	if (fd < 0) error("Cannot open file");
	offset := 0;
	k := t.read(fd, buffer, 16);
	while (k > 0) do
		prword(offset);
		writes(": ");
		for(i = 0, k) do
			prbyte(buffer::i);
			writec(' ');
		end
		for (i=k, 16) writes("\s\s\s");
		for(i = 0, k) do
			ie (buffer::i > 31 /\ buffer::i < 127)
				writec(buffer::i);
			else
				writec('.');
		end
		writes(t.newline(buffer));
		offset := offset + 16;
		k := t.read(fd, buffer, 16);
	end
	t.close(fd);
end
