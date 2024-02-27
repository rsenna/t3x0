! Draw a Mandelbrot Set using ASCII characters.
! With Unicode support
! Nils M Holm, 2003, 2022, 2023
! Public Domain / 0BSD License

! UAPFEL U  print Unicode block characters (default: ASCII)
! UAPFEL P  print IBM PC block characters (default: interactive)
! UAPFEL B  print batch output (default: interactive)

use t3x: t;

const	SCALE = 100;	! Denominator for fixed-point computations,
			! greater values => higher resolution,
			! must be a multiple of 10,
			! must be 100 on the 16-bit machines

struct  FILL = UPPER, LOWER, FULL, EMPTY;

var	Unicode, IBMPC;
var	Batch;
var	S_full, S_upper, S_lower, S_empty;

length(s) return t.memscan(s, 0, 32767);

writes(s) t.write(T3X.SYSOUT, s, length(s));

prch(c)
	     ie (c = UPPER) writes(S_upper);
	else ie (c = LOWER) writes(S_lower);
	else ie (c = EMPTY) writes(S_empty);
	else                writes(S_full);

print(s) do var i;
	for (i=0, 79) prch(s::i);
end

! Compute the color of a point c
f(x, y) do
	var	cr, ci, zr, zi, ir;
	var	i;

	zr := 0;		! z = 0+0i
	zi := 0;
	cr := x*SCALE/25;	! c = x+yi
	ci := y*SCALE/20;
	for (i=0, 70) do
		! z(n+1) = z(n) * z(n) + c
		ir := zr*zr/SCALE - zi*zi/SCALE;
		zi := zr*zi/SCALE + zi*zr/SCALE + ci;
		zr := ir + cr;
		! if |z| > 2+2i, c is not a member of the Mandelbrot set M
		if (	zi > 2*SCALE \/ zr > 2*SCALE \/
			zi < -2*SCALE\/ zr < -2*SCALE
		)
			return 0;
	end
	! |z| <= 2+2i after 100 iterations, so c is probably in M
	return 1;
end

do
	var	a::10, k;
	var	x, y, r;
	var	line::79;
	var	even;
	var	nl::3;

	S_full := "M";
	S_upper := "\q";
	S_lower := "m";
	S_empty := "\s";
	Unicode := 0;
	IBMPC := 0;
	Batch := 0;
	k := t.getarg(1, a, 10);
	if (k > 0) do
	     	     ie (t.memscan(a, 'U', k) >= 0) Unicode := %1;
		else ie (t.memscan(a, 'P', k) >= 0) IBMPC := %1;
		else if (t.memscan(a, 'B', k) >= 0) Batch := %1;
	end
	if (Unicode) do
		S_upper := packed [ 0xe2, 0x96, 0x80, 0 ];
		S_lower := packed [ 0xe2, 0x96, 0x84, 0 ];
		S_full  := packed [ 0xe2, 0x96, 0x88, 0 ];
	end
	if (IBMPC) do
		S_upper := packed [ 223, 0 ];
		S_lower := packed [ 220, 0 ];
		S_full  := packed [ 219, 0 ];
	end
	t.newline(nl);
	even := 0;
	for (y=-24, 24) do
		for (x=-59, 20) do
			r := \f(x,y);
			ie (even)
				line::(x+59) :=
					line::(x+59) = UPPER ->
						r-> FULL: UPPER:
						r-> LOWER: EMPTY;
			else
				line::(x+59) := r-> UPPER: EMPTY;
			if (\Batch) prch(line::(x+59));
		end
		ie (Batch /\ even) do
			print(line);
			writes(nl);
		end
		else ie (even) do
			writes(nl);
		end
		else do
			writes("\r");
		end
		even := \even;
	end
end
