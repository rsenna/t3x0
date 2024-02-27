! Hello, World! using just the core module
! Nils M Holm
! Public domain / 0BSD license

use t3x: t;

do var b::3;
	t.write(T3X.SYSOUT, "Hello, World!", 13);
	t.write(T3X.SYSOUT, t.newline(b), b::1-> 2: 1);
end
