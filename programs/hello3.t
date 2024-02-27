! Extended Hello, World!
! Nils M Holm
! Public domain / 0BSD license

use t3x: t;

do var k, buf::20;
	t.write(T3X.SYSOUT, "Enter your name: ", 17);
	k := t.read(T3X.SYSIN, buf, 20);
	ie (k < 2) do
		t.write(T3X.SYSOUT, "Goodbye!\r\n", 10);
	end
	else do
		t.write(T3X.SYSOUT, "Hello, ", 7);
		t.write(T3X.SYSOUT, buf, k-1);
		t.write(T3X.SYSOUT, "!\r\n", 3);
	end
end
