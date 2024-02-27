! T3X/0 module: simple I/O functions
! Nils M Holm, 1999-2002,2019,2022
! In the public domain / 0BSD License

! needs: t3x, string

module io;

 public fwrites(fd, s) t3x.write(fd, s, string.length(s));

 public writes(s) fwrites(T3X.SYSOUT, s);

 public fnl(fd) do var b::3;
	fwrites(fd, t3x.newline(b));
 end

 public nl() fnl(T3X.SYSOUT);

 public fwriteln(fd, s) do
	fwrites(fd, s);
	fnl(fd);
 end

 public writeln(s) fwriteln(T3X.SYSOUT, s);

end
