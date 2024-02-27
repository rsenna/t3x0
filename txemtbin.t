! T3X/0 compiler: binary emitter module
! Nils M Holm, 2022, 2023
! Public Domain / 0BSD License

module txemit;

 public findlab(id) return Labels[id];

 public emits(v);

 public totext();

 public todata();

 public demit(v) emit(v);

 public demitw(v) emitw(v);

 public demitaddr(v) emitw(v);

 public dskip(a) gen(CG_SKIP, a);

 hex(c)	ie (numeric(c))
		return c-'0';
	else
		return c-'a'+10;

 byte(s) return 16*hex(s::0) + hex(s::1);

 public rgen(s, v) do var n;
	while (s::0) do
		ie (s::0 = ',') do
			ie (s::1 = 'w')
				emitw(v);
			else ie (s::1 = 'l')
				emit(v);
			else ie (s::1 = 'h')
				emit(v+1);
			else ie (s::1 = 'r')
				emit(v-Tp-1);
			else ie (s::1 = 'R')
				emitw(v-Tp-2);
			else ie (s::1 = '>') do
				n := byte(s+4) << 8 | byte(s+2);
				emitw(n-Tp-2);
				s := s+4;
			end
			else
				oops("rgen", 0);
		end
		else do
			emit(byte(s));
		end
		s := s+2;
	end
 end

 public droplab() do
	commit();
	return Tp;
 end

 public dropdlab() do
	commit();
	return Tp;
 end

 public resolve(id) do
	commit();
	Labels[id] := Tp;
 end

 public resolve_data(id) resolve(id);

 public globaddr() do var l, i, g;
	if (Gp >= Gtop) do
		l := newlab();
		gen(CG_RJUMP, findlab(l));
		commit();
		Gp := Tp;
		for (i=0, CG.GPOOL_SIZE) emitw(0);
		Gtop := Tp;
		resolve(l);
	end
	g := Gp;
	Gp := Gp+2;
	return g;
 end

 public emitlib() do var i, j, k, lib;
	lib := cg.library();
	i := 0;
	while (lib[i]) do
		k := lib[i];
		i := i+1;
		for (j=0, k) emit(lib[i]::j);
		i := i+1;
	end
 end

end
