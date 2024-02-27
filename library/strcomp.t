! T3X/0 module: string compatibility module
! Nils M Holm, 1999-2002,2019,2022
! In the public domain / 0BSD License

! needs: string

module strcomp;

 ! Implement some functions of previous T3X versions

 public numtostr(s, n, r)
	return string.copy(s, string.ntoa(n, r));

 public strtonum(s, r, lp) return string.aton(s, r, lp);

 public xlate(s, c1, c2) do var i;
	i := 0;
	while (s::i) do
		if (s::i = c1) s::i := c2;
		i := i+1;
	end
	return s;
 end

end
