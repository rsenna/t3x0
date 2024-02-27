! T3X/0 module: string functions
! Nils M Holm, 1999-2002,2019,2022
! In the public domain / 0BSD License

! needs: t3x

module string;

 public const	MAXLEN = 32767;

 public length(s) return t3x.memscan(s, 0, MAXLEN);

 public copy(a, b) do
	t3x.memcopy(a, b, length(b)+1);
	return a;
 end

 public ncopy(n, a, b) do var k;
	if (n < 0) return;
	k := length(b) + 1;
	t3x.memcopy(a, b, n < k-> n: k);
	a::(n-1) := 0;
	return a;
 end

 public comp(a, b) do var k1, k2;
	k1 := length(a)+1;
	k2 := length(b)+1;
	return t3x.memcomp(a, b, k1>k2->k1:k2);
 end

 public scan(s, c) return t3x.memscan(s, c, length(s));

 public rscan(s, c) do var k, i;
	k := length(s);
	for (i=k-1, %1, %1) if (s::i = c) return i;
	return %1;
 end

 public find(a, b) do var ka, kb, i;
	ka := length(a);
	kb := length(b);
	for (i=0, ka-kb+1)
		if (\t3x.memcomp(@a::i, b, kb))
			return i;
	return %1;
 end

 chcase(s, l, h, d) do var i;
	i := 0;
	while (s::i) do
		if (l <= s::i /\ s::i <= h)
			s::i := s::i + d;
		i := i+1;
	end
	return s;
 end

 public upcase(s)   return chcase(s, 'a', 'z', %32);
 public downcase(s) return chcase(s, 'A', 'Z',  32);

 _digits() return "0123456789ABCDEF";

 var buf::66; ! 64-bit binary + sign

 public ntoa(n, r) do
	var	digits;
 	var	i, s;

	digits := _digits();
	buf::65 := 0;
	i := 65;
	s := 0;
	if (r < 0) do
		r := -r;
		if (n < 0) do
			n := -n;
			s := 1;
		end
	end
	while (1) do
		i := i-1;
		buf::i := digits::(n mod r);
		n := n ./ r;
		if (\n) leave;
	end
	if (s) do
		i := i-1;
		buf::i := '-';
	end
	return @buf::i;
 end

 public aton(s, r, lp) do
	var	digits;
	var	g, i, j;
	var	v;
	var	lim;

	digits := _digits();
	r := r mod 17;
	i := 0;
	v := 0;
	g := 1;
	if (s::i = '-' \/ s::i = '%') do
		g := %1;
		i := i+1;
	end
	lim := lp /\ lp[0]-> lp[0]: MAXLEN;
	for (i=i, lim) do
		j := t3x.memscan(digits, s::i, r);
		if (j < 0) leave;
		v := v*r + j;
	end
	if (lp) lp[0] := i;
	return v*g;
 end

end
