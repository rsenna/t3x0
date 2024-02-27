! Draw a Mandelbrot Set using ASCII characters.
! Nils M Holm, 2003, 2022
! Public Domain / 0BSD License

use t3x: t;

const	SCALE = 100;	! Denominator for fixed-point computations,
			! greater values => higher resolution,
			! must be a multiple of 10,
			! must be 100 on the 16-bit machines

! ASCII renditions of filled rectangles:
const	FULL	= 'M',	! upper and lower half filled
	UPPER	= '"',	! upper half filled
	LOWER	= 'm',	! lower half filled
	EMPTY	= '\s';	! empty rectangle

! IBM PC extended ASCII rendition:
! const	FULL = 219, UPPER = 223, LOWER = 220, EMPTY = '\s';

! Compute the color of a point c
f(x, y) do
	var	cr, ci, zr, zi, ir;
	var	i;

	zr := 0;		! z = 0+0i
	zi := 0;
	cr := x*SCALE/25;	! c = x+yi
	ci := y*SCALE/20;
	! no change beyond 70 iterations
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
	var	x, y, r;	! coordinates, point color
	var	line::79;	! line buffer
	var	even;		! even line flag
	var	nl::3, k;	! newline sequence buffer, length

	t.newline(nl);
	for (k=0, 3) if (\nl::k) leave;
	even := 0;
	for (y=-24, 24) do			! lines
		for (x=-59, 20) do		! columns
			r := \f(x,y);		! r = point color
			! When drawing an even line, merge the color
			! of r into the current rectangle:
			ie (even)
				line::(x+59) :=
					line::(x+59)->
						! odd point was on
						r-> FULL: UPPER:
						! odd point was off
						r-> LOWER: EMPTY;
			! Odd line: just memorize the color
			else
				line::(x+59) := r;
		end
		! Even line completed, print it
		if (even) do
			t.write(T3X.SYSOUT, line, 79);
			t.write(T3X.SYSOUT, nl, k);
		end
		even := \even;
	end
end
