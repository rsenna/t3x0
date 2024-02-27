! T3X/0 compiler: symbol table dump functions
! Nils M Holm, 2022
! Public Domain / 0BSD License

! This module is for use in the T3X/0 compiler only.

! needs: the T3X/0 compiler

module symdump;

 pad(s, n) do var i, k;
	k := str_length(s);
	for (i=k, n) writes("\s");
 end

 wrpad(s, n) do
	writes(s);
	pad(s, n);
 end

 prflag(v, f, s) writes(v&f-> s: "-");

 public dumpsym(y) do var f;
	writes("; ");
	writes(ntox(y));
	writes("\s");
	writes(ntox(y[SFLAGS]));
	writes("\s");
	f := y[SFLAGS];
	prflag(f, EXTN, "X");
	prflag(f, ALIA, "A");
	prflag(f, PUBL, "P");
	prflag(f, MODU, "M");
	prflag(f, FUNC, "F");
	prflag(f, FORW, "W");
	prflag(f, VECT, "V");
	prflag(f, CNST, "C");
	prflag(f, GLOB, "G");
	writes("\s");
	writes(ntox(y[SVALUE]));
	writes("\s");
	writes(y[SNAME]);
	if (f & (FUNC|FORW|EXTN)) do
		writes(" (");
		writes(ntoa(f >> ARSHIFT));
		writes(")");
	end
	nl();
 end

 public dumpsyms(y0, yn) do var i, f;
	if (y0 = yn) return;
	nl();
	writes("; ");
	wrpad("ADDR", 1+t.bpw()*2);
	wrpad("FLGS", 11+t.bpw()*2);
	wrpad("VALU", 1+t.bpw()*2);
	wrpad("NAME", 1+t.bpw()*2);
	nl();
	for (i=y0, yn, SYM) dumpsym(@Syms[i]);
 end

end
