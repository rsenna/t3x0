! Draw some random squares using Curses.
! Nils M Holm, 2023
! Public Domain / 0BSD License

! This program works on Unix only.
! Compile with
! tx0 -e curses.c -e -lcurses curses

use t3x;

extern	addch(1),
	clear(0),
	delay(1),
	endwin(0),
	initscr(0),
	move(2),
	refresh(0),
	usleep(1);

lfsr(k) do var s, x;
	s := [ 123 ];
	x := s[0] >> 1 ^ s[0] & 1 << 14;
	s[0] := s[0] >> 1 | x;
	return s[0] mod k;
end

box(x, y, w, h, c) do var i, j, tones;
	tones := [ ':', '/', '#' ];
	c := tones[c];
	for (i=y, y+h) do
		move(y+i, x);
		for (j=0, w) addch(c);
	end
end

do var i;
	initscr();
	clear();
	refresh();
	for (i=0, 1000) do
		box(lfsr(70), lfsr(20), lfsr(40)+1, lfsr(6)+4, lfsr(3));
		refresh();
		usleep(10000);
	end
	clear();
	refresh();
	endwin();
end
