/*
 * Interface file: Unix Console -> Curses
 * Nils M Holm, 2024
 * Public Domain / 0BSD License
 */

#include <curses.h>
#include <unistd.h>

void t3x_setup(int p) {
	initscr();
	raw();
	noecho();
	scrollok(stdscr, TRUE);
}

void t3x_shutdown(void) {
	echo();
	noraw();
	endwin();
}

void t3x_clrscr(void) {
	clear();
}

void t3x_clreol(void) {
	clrtoeol();
}

void t3x_move(int x, int y) {
	move(x, y);
}

void t3x_scroll(void) {
	scroll(stdscr);
	refresh();
}

void t3x_rscroll(void) {
	wscrl(stdscr, -1);
	refresh();
}

void t3x_refresh(void) {
	refresh();
}

int t3x_getkey(void) {
	int	c;

	c = getch();
	return c == '\n'? '\r': c;
}

int t3x_pollkey(void) {
	int	c;

	nodelay(stdscr, TRUE);
	c = getch();
	nodelay(stdscr, FALSE);
	return c == ERR? 0: c == '\n'? '\r': c;
}

void t3x_wrch(int c) {
	addch(c);
}

void t3x_writes(char *s) {
	addstr(s);
}

int	Mdelay = 1000;

void t3x_calibrate(int m) {
	Mdelay = m;
}

void t3x_wait(int hs) {
	refresh();
	usleep(hs * Mdelay);
}
