! T3X/0 module: string formatter
! Nils M Holm, 1999-2002,2019,2022
! In the public domain / 0BSD License

! needs: t3x, string

module strfmt;

 ! Template format:
 ! %[min][:F][U][LR]CDSX%
 ! Fillchar
 ! Unsigned
 ! Left, Right
 ! Character, Decimal, String, heXadecimal, %

 const	MAXPAD = 256;	! max. padding characters

 var	nbuf::MAXPAD+1;

 public nformat(n, s, tmpl, args) do
	const	LEFT=%1, NONE=0, RIGHT=1;	! adjust modes
	var	adj;		! adjustment type, see above
	var	op, ap;
	var	i, j, k;
	var	min;		! minimum field size
	var	p, x;
	var	unsigned;	! unsigned flag for %D (1=unsigned)
	var	fc;		! fill character (for padding)

	i := 0;		! template index
	j := 0;		! buffer index
	ap := 0;	! argument index
	while (tmpl::i) do
		ie (tmpl::i = '%') do
			! set defaults
			adj := NONE;
			min := %1;	! variable length
			unsigned := 0;
			fc := '\s';
			i := i+1;
			! accept min field length
			while ('0' <= tmpl::i /\ tmpl::i <= '9') do
				if (min = %1) min := 0;
				min := min*10 + tmpl::i - '0';
				i := i+1;
			end
			! accept fill character
			if (tmpl::i = ':') do
				fc := tmpl::(i+1);
				i := i+2;
			end
			! accept unsigned flag
			if (tmpl::i = 'u' \/ tmpl::i = 'U') do
				unsigned := 1;
				i := i+1;
			end
			! accept adjustment type
			ie (tmpl::i = 'l' \/ tmpl::i = 'L') do
				adj := LEFT;
				i := i+1;
			end
			else if (tmpl::i = 'r' \/ tmpl::i = 'R') do
				adj := RIGHT;
				i := i+1;
			end
			x := tmpl::i;	! get type
			if ('a' <= x /\ x <= 'z') x := x+('A'-'a');
			op := ap;
			ie (x = 'C') do
				nbuf::0 := args[ap];
				nbuf::1 := 0;
				p := nbuf;
				ap := ap+1;
			end
			else ie (x = 'D') do
				p := string.ntoa(args[ap],
					unsigned-> 10: %10);
				if (adj = NONE) adj := RIGHT;
				ap := ap+1;
			end
			else ie (x = 'S') do
				p := args[ap];
				ap := ap+1;
				if (adj = NONE) adj := LEFT;
			end
			else ie (x = 'X') do
				p := string.ntoa(args[ap],
					unsigned-> 16: %16);
				ap := ap+1;
				if (adj = NONE) adj := RIGHT;
			end
			! default: copy character after %
			else if (j .< n-1) do
				s::j := tmpl::i;
				j := j+1;
			end
			if (op \= ap) do	! op=ap means invalid type
				k := string.length(p);
				ie (min <= k) do
					! no padding required
					if (j+k .>= n-1) k := n-j-1;
					if (k) t3x.memcopy(@s::j, p, k);
					j := j+k;
				end
				else do
					if (j+min .>= n-1) min := n-j-1;
					if (min) t3x.memfill(@s::j, fc, min);
					if (j+k .>= n-1) k := n-j-1;
					ie (k /\ adj = LEFT)
						t3x.memcopy(@s::j, p, k);
					else if (k)
						t3x.memcopy(@s::(j+min-k),
								p, k);
					j := j+min;
				end
			end
		end
		else if (j .< n-1) do
			! no %-sequence: copy one char
			s::j := tmpl::i;
			j := j+1;
		end
		if (tmpl::i) i := i+1;
	end
	s::j := 0;
	return s;
 end

 public format(s, tmpl, args) return nformat(~0, s, tmpl, args);

end
