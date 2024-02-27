! T3X/0 module: string parser
! Nils M Holm, 1999-2002,2019,2022,2023
! In the public domain / 0BSD License

! needs: t3x, string

module strparse;

 ! Template format:
 ! %[len][:delimiter]CDSWX%
 ! Character, Decimal, String, White, heXadecimal, %
 ! %[group] (literal []s)
 ! %W = %[\s\t]

 var	cbuf::256;

 public parse(s, tmpl, args) do
	var	i, j, k, x;
	var	n, m;
	var	ap;
	var	len,		! max. length to parse
		dlm;		! delimiter

	i := 0;		! template index
	j := 0;		! string (source) index
	k := string.length(s);
	ap := 0;	! argument index
	while (tmpl::i /\ j < k) do
		ie (tmpl::i = '%') do
			i := i+1;
			len := %1;	! no limit on length
			dlm := %1;	! no delimiter
			! accept length
			while ('0' <= tmpl::i /\ tmpl::i <= '9') do
				if (len = %1) len := 0;
				len := len*10 + tmpl::i - '0';
				i := i+1;
			end
			! accept delimiter
			if (tmpl::i = ':') do
				dlm := tmpl::(i+1);
				i := i+2;
			end
			x := tmpl::i;	! get type
			if ('a' <= x /\ x <= 'z') x := x+('A'-'a');
			ie (x = 'C') do
				args[ap][0] := s::j;
				ap := ap+1;
				j := j+1;
			end
			else ie (x = 'D') do
				n := len=%1-> 0: len;
				args[ap][0] := string.aton(@s::j, 10, @n);
				ap := ap+1;
				j := j+n;
			end
			else ie (x = 'S') do
				if (len = %1) do
					! no limit: copy up to next tmpl char
					if (dlm = %1) dlm := tmpl::(i+1);
					len := t3x.memscan(@s::j, dlm, k-j+1);
					If (len < 1) len := k-j+1;
				end
				if (len > k-j+1) len := k-j+1;
				t3x.memcopy(args[ap], @s::j, len);
				args[ap]::len := 0;
				ap := ap+1;
				j := j+len;
			end
			else ie (x = 'X') do
				n := len=%1-> 0: len;
				args[ap][0] := string.aton(@s::j, 16, @n);
				ap := ap+1;
				j := j+n;
			end
			else ie (x = 'W' \/ x = '[') do
				ie (x = 'W') do
					cbuf::0 := '\s';
					cbuf::1 := '\t';
					m := 2;
				end
				else do
					m := 0;
					i := i+1;
					while (tmpl::i /\ tmpl::i \= ']') do
						cbuf::m := tmpl::i;
						i := i+1;
						m := m+1;
					end
				end
				while (	j < k /\
					t3x.memscan(cbuf, s::j, m) >= 0
				)
					j := j+1;
			end
			else do
				! default: match character after %
				if (tmpl::i \= s::j) leave;
				j := j+1;
			end
		end
		else do
			! default: match literal character
			if (tmpl::i \= s::j) leave;
			j := j+1;
		end
		if (tmpl::i) i := i+1;
	end
	return ap;
 end

end
