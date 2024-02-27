! Nested block escape regression tests
! Nils M Holm, 2023
! Public Domain / 0BSD License

use t3x: t;

f1() do
	return;
end

f2() do var x;
	return;
end

f3() do
	do return; end
end

f4() do var x;
	do return; end
end

f5() do
	do var y;
		return;
	end
end

f6() do var x;
	do var y;
		return;
	end
end

g1(a) do
	return;
end

g2(a) do var x;
	return;
end

g3(a) do
	do return; end
end

g4(a) do var x;
	do return; end
end

g5(a) do
	do var y;
		return;
	end
end

g6(a) do var x;
	do var y;
		return;
	end
end

h1() do
	while (1) do
		leave;
	end
end

h2() do var x;
	while (1) do
		leave;
	end
end

h3() do
	while (1) do var y;
		leave;
	end
end

h4() do var x;
	while (1) do var y;
		leave;
	end
end

r1() do var x;
	while (1) leave;
end

r2() if (0) do var x;
            end

do var x;
	f1();
	f2();
	f3();
	f4();
	f5();
	f6();
	g1(0);
	g2(0);
	g3(0);
	g4(0);
	g5(0);
	g6(0);
	h1();
	h2();
	h3();
	h4();
	r1();
	r2();
	t.write(T3X.SYSOUT, "Looks good!\r\n", 13);
end
