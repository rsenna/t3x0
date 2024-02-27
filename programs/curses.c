/*
 * Interface file for curses.t
 * Nils M Holm, 2023
 * Public Domain / 0BSD License
 */

#include <curses.h>
#include <unistd.h>

void t3x_initscr(void) {
	initscr();
}

void t3x_endwin(void) {
	endwin();
}

void t3x_clear(void) {
	clear();
}

void t3x_move(int x, int y) {
	move(y, x);
}

void t3x_refresh(void) {
	refresh();
}

void t3x_addch(int c) {
	addch(c);
}

void t3x_usleep(int ms) {
	usleep(ms);
}
