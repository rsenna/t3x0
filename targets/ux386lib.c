/*
 * T3X/0 core module for generic Unix on the 386
 * Nils M Holm, 2022, 2023
 * Public Domain / 0BSD License
 */

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>

#define BPW	4
#define cell	int

int	Argc;
char	**Argv;

cell t3x_bpw(void) { return BPW; }

char *t3x_newline(char *s) {
	s[0] = '\n';
	s[1] = 0;
	return s;
}

cell t3x_memcomp(cell n, void *d, void *s) {
	return memcmp(s, d, n);
}

cell t3x_memcopy(cell n, void *d, void *s) {
	memmove(s, d, n);
	return 0;
}

cell t3x_memfill(cell n, cell c, void *d) {
	memset(d, c, n);
	return 0;
}

cell t3x_memscan(cell n, cell c, char *s) {
	cell	i;

	for (i=0; i<n; i++)
		if (s[i] == c) return i;
	return -1;
}

cell t3x_getarg(cell k, char *s, cell n) {
	int	j, m;

	k--;
	if (n >= Argc) return -1;
	j = strlen(Argv[n]);
	m = j>=k? k: j;
	memcpy(s, Argv[n], m);
	s[m] = 0;
	return m;
}

cell t3x_create(char *s) {
	return creat(s, 0644);
}

cell t3x_open(cell mode, char *s) {
	int	r;

	if (1 == mode)
		r = creat(s, 0644);
	else if (3 == mode)
		r = open(s, O_WRONLY);
	else
		r = open(s, mode);
	if (3 == mode) lseek(r, 0L, SEEK_END);
	return r;
}

cell t3x_close(cell fd) {
	return close(fd);
}

cell t3x_read(cell k, char *b, cell fd) {
	return read(fd, b, k);
}

cell t3x_write(cell k, char *b, cell fd) {
	return write(fd, b, k);
}

cell t3x_seek(cell how, unsigned cell where, cell fd) {
	long	w;
	int	h;

	switch (how) {
		case 0:	w = where; h = SEEK_SET; break;
		case 1: w = where; h = SEEK_CUR; break;
		case 2: w = where; w = -w; h = SEEK_END; break;
		case 3: w = where; w = -w; h = SEEK_CUR; break;
		default: return 0xffff;
	}
	return lseek(fd, w, h) < 0? -1: 0;
}

cell t3x_rename(char *new, char *old) {
	return rename(old, new);
}

cell t3x_remove(char *s) {
	return remove(s);
}

cell t3x_trunc(cell fd) {
	return ftruncate(fd, lseek(fd, 0, SEEK_CUR));
}

cell	*Sem;

void handle_break(int dummy) {
	*Sem = 1;
	signal(SIGINT, handle_break);
}

cell t3x_break(cell *sem) {
	if (0 == sem) {
		signal(SIGINT, SIG_DFL);
	}
	else if ((cell *) 1 == sem) {
		/* ignore */
	}
	else {
		Sem = sem;
		*Sem = 0;
		signal(SIGINT, handle_break);
	}
	return 0;
}

void start(void);

int main(int argc, char **argv) {
	Argc = argc;
	Argv = argv;
	start();
}
