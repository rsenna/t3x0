! Draw a Postscript Mandelbrot Set
! Nils M Holm, 2003, 2022
! Public Domain / 0BSD License

use t3x: t;
use string: str;
use strfmt: fmt;
use io;

const	SCALE = 100;	! Denominator for fixed-point computations,
			! greater values => higher resolution,
			! must be a multiple of 10,
			! must be 100 on the 16-bit machines

color(i) return i <  5-> "":
		i <  8-> "0.4 0.0 0.0":
		i < 12-> "0.6 0.0 0.0":
		i < 16-> "0.8 0.0 0.0":
		i < 100-> "1.0 0.0 0.0": "0 0 0";

! Compute the color of a point c
f(x, y) do
	var	cr, ci, zr, zi, ir;
	var	i;

	zr := 0;		! z = 0+0i
	zi := 0;
	cr := x*SCALE/40;	! c = x+yi
	ci := y*SCALE/40;
	for (i=0, 100) do
		! z(n+1) = z(n) * z(n) + c
		ir := zr*zr/SCALE - zi*zi/SCALE;
		zi := zr*zi/SCALE + zi*zr/SCALE + ci;
		zr := ir + cr;
		! if |z| > 2+2i, return color of (x,y)
		if (	zi > 2*SCALE \/ zr > 2*SCALE \/
			zi < -2*SCALE\/ zr < -2*SCALE
		)
			return color(i);
	end
	! |z| <= 2+2i after 100 iterations, return black
	return "0 0 0";
end

intro() do
	io.writeln("%!PS-Adobe-3.0");
	io.writeln("%%Creator: NMH");
	io.writeln("%%DocumentMedia: Plain 420 420 white ()");
	io.writeln("%%LanguageLevel: 2");
	io.writeln("%%BoundingBox: 0 0 420 420");
	io.writeln("%%EndComments");
	io.writeln("/box {");
	io.writeln(" /dy exch def");
	io.writeln(" /dx exch def");
	io.writeln(" /y exch def");
	io.writeln(" /x exch def");
	io.writeln(" x y moveto");
	io.writeln(" dx y lineto");
	io.writeln(" dx dy lineto");
	io.writeln(" x dy lineto");
	io.writeln(" x y lineto");
	io.writeln(" closepath");
	io.writeln(" fill");
	io.writeln("} def");
end

do var x, y, b::100, s;
	intro();
	for (y=-50, 50) do
		io.nl();
		for (x=- 80, 30) do
			s := f(x, y);
			if (s::0) do
				io.writes(fmt.format(b, "%s setrgbcolor",
						[(s)]));
				io.nl();
				io.writes(fmt.format(b, "%d %d %d %d box",
						[(320+x*4, 200+y*4,
						  320+x*4+4, 200+y*4+4)]));
				io.nl();
			end
		end
	end
end
