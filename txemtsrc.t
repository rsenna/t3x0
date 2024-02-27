! T3X/0 compiler: source code emitter module
! Nils M Holm, 2022, 2023
! Public Domain / 0BSD License

module txemit;

 var	Text;

 public findlab(id) return id;

 public emits(s) do
	while (s::0) do
		emit(s::0);
		s := s+1;
	end
 end

 public totext() do
	if (Text) return;
	emits(cg.textseg());
	Text := %1;
 end

 public todata() do
	if (\Text) return;
	emits(cg.dataseg());
	Text := 0;
 end

 public demit(v) do
	todata();
	emits(cg.defbyte());
	ie (alphabetic(v)) do
		emit('\'');
		emit(v);
		emit('\'');
	end
	else do
		emits(ntoa(v));
	end
	emit('\n');
 end

 public demitw(v) do
	todata();
	emits(cg.defword());
	emits(ntoa(v));
	emit('\n');
 end

 public demitaddr(v) do
	todata();
	emits(cg.defaddr());
	emits(ntoa(v));
	emit('\n');
 end

 public dskip(a);

 public rgen(p, v) do var s, n, i, j;
	i := 0;
	while (p[i]) do
		s := p[i];
		j := 0;
		while (s::j) do
			ie (s::j = '!') do
				j := j+1;
				ie (s::j = 'w')
					emits(ntoa(v));
				else ie (s::j = 'n')
					emits(v);
				else
					oops("rgen", 0);
			end
			else do
				emit(s::j);
			end
			j := j+1;
		end
		emit('\n');
		i := i+1;
	end
 end

 prlab(id) do
	emit('L');
	emits(ntoa(id));
	emit(':');
	emit('\n');
 end

 public droplab() do var k;
	commit();
	totext();
	k := newlab();
	prlab(k);
	return k;
 end

 public dropdlab() do var k;
	commit();
	todata();
	k := newlab();
	prlab(k);
	return k;
 end

 public resolve(id) do
	commit();
	prlab(id);
 end

 public resolve_data(id) do
	todata();
	prlab(id);
 end

 public globaddr() do var k;
	k := dropdlab();
	emits(cg.defglob());
	return k;
 end

 public emitlib() do var i, j, lib, s;
	lib := cg.library();
	i := 0;
	while (lib[i]) do
		s := lib[i];
		j := 0;
		while (s[j]) do
			emits(s[j]);
			emit('\n');
			j := j+1;
		end
		i := i+1;
	end
 end

 do
	Text := %1;
 end

end
