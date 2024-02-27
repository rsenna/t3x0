/*
 * Ad-hoc 16-bit Tcode/0 virtual machine
 * Nils M Holm, 2017,2022,2023,2024
 * Public domain / 0BSD license
 */

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>

#ifdef unix
 #include <unistd.h>
#endif

#undef DEBUG

/*
 * Define EXTRA to enable t3x.trunc and t3x.break.
 */
#define EXTRA

#ifdef EXTRA
 #include <signal.h>
#endif

#define MEMSIZE	0xfffe

#define byte	unsigned char
#define cell	unsigned int

byte	*M;
cell	Red;

char	**Args;
int	Narg;

void writes(char *s) { write(1, s, strlen(s)); }
void wlog(char *s) { write(2, s, strlen(s)); }

void fail(char *s) {
	wlog("tcvm: ");
	wlog(s);
	wlog("\n");
	exit(1);
}

void load(char *s) {
	int	fd, k;
	char	b[6];
	char	p[100];

	k = strlen(s);
	if (k > 96) fail("file name too long");
	strcpy(p, s);
	if (k < 4 || strcmp(&s[k-3], ".tc"))
		strcat(p, ".tc");
	fd = open(p, O_RDONLY);
	if (fd < 0) fail("could not open program");
#ifdef __TURBOC__
	setmode(fd, O_BINARY);
#endif
	read(fd, b, 6);
	if (memcmp(b, "T3X0", 4)) fail("not a tcvm program");
	M = malloc(MEMSIZE);
	if (NULL == M) fail("not enough memory");
	Red = read(fd, M, MEMSIZE);
	close(fd);
}

cell	A, F, I, P;

cell w(cell a) { return  M[a+0] | (M[a+1] << 8); }

void Sw(cell a, cell w) {
	M[a+0] =  w        & 255;
	M[a+1] = (w >> 8)  & 255;
}

void push(cell x) {
	P -= 2;
	Sw(P, x);
}

cell pop(void) {
	P += 2;
	return w(P-2);
}
#ifdef __TURBOC__                                                               
 int S(cell x) { return x; }                                                    
#else                                                                           
 int S(cell x) { return x > 32767? x-65536: x; }
#endif

#define a()	w(I+1)
#define a2()	w(I+3)

cell memscan(cell p, cell c, cell k) {
	cell	i;

	for (i=0; i<k; i++)
		if (M[p+i] == c)
			return i;
	return 0xffff;
}

int getarg(int n, char *s, int k) {
	int	j, m;

	n++;
	k--;
	if (n >= Narg) return -1;
	j = strlen(Args[n]);
	m = j>=k? k: j;
	memcpy(s, Args[n], m);
	s[m] = 0;
	return m;
}

cell t3xopen(char *s, int mode) {
	int	r;

	if (1 == mode)
		r = creat(s, 0644);
	else if (3 == mode)
		r = open(s, O_WRONLY);
	else
		r = open(s, w(P+2));
#ifdef __TURBOC__
	if (r > 0) setmode(r, O_BINARY);
#endif
	if (3 == mode) lseek(r, 0L, SEEK_END);
	return r;
}

cell t3xseek(cell fd, cell where, cell how) {
	long	w;
	int	h;

	switch (how) {
		case 0:	w = where; h = SEEK_SET; break;
		case 1: w = where; h = SEEK_CUR; break;
		case 2: w = where; w = -w; h = SEEK_END; break;
		case 3: w = where; w = -w; h = SEEK_CUR; break;
		default: return 0xffff;
	}
	return lseek(fd, w, h) < 0? 0xffff: 0;
}

cell t3xtrunc(cell fd) {
#ifndef EXTRA
	fail("t3x.trunc not implemented");
	return 0;
#else
 #ifdef unix
	return ftruncate(fd, lseek(fd, 0, SEEK_CUR));
 #endif
 #ifdef __TURBOC__
	return write(fd, "", 0);
 #endif
#endif
}

char	*Sem;

void handle_break(int dummy) {
	Sem[0] = Sem[1] = -1;
#ifdef EXTRA
 #ifdef unix
	signal(SIGINT, handle_break);
 #endif
#endif
}

cell t3xbreak(cell sem) {
#ifndef EXTRA
	fail("t3x.break not implemented");
#else
 #ifdef unix
	if (0 == sem) {
		signal(SIGINT, SIG_DFL);
	}
	else if (1 == sem) {
		/* ignore */
	}
	else {
		Sem = (char *) &M[sem];
		Sem[0] = Sem[1] = 0;
		signal(SIGINT, handle_break);
	}
 #endif
 #ifdef __TURBOC__
	fail("t3x.break not implemented");
 #endif
#endif
	return 0;
}

cell libcall(cell n) {
	cell	r;

#ifdef DEBUG
	printf("LIBCALL(%d): %x %x %x %x\n", n, w(P+6), w(P+4), w(P+2), w(P));
#endif
	switch (n) {
	case  0: r = 2; break;
	case  1: strcpy((char *) &M[w(P+2)], "\n"); r = w(P+2); break;
	case  2: r = memcmp(&M[w(P+6)], &M[w(P+4)], w(P+2)); break;
	case  3: memmove(&M[w(P+6)], &M[w(P+4)], w(P+2)); r = 0; break;
	case  4: memset(&M[w(P+6)], w(P+4), w(P+2)); r = 0; break;
	case  5: r = memscan(w(P+6), w(P+4), w(P+2)); break;
	case  6: r = getarg(w(P+6), (char *) &M[w(P+4)], w(P+2)); break;
	case  7: r = creat((char *) &M[w(P+2)], 0644); break;
	case  8: r = t3xopen((char *) &M[w(P+4)], w(P+2)); break;
	case  9: r = close(w(P+2)); break;
	case 10: r = read(w(P+6), &M[w(P+4)], w(P+2)); break;
	case 11: r = write(w(P+6), &M[w(P+4)], w(P+2)); break;
	case 12: r = t3xseek(w(P+6), w(P+4), w(P+2)); break;
	case 13: r = rename((char *) &M[w(P+4)], (char *)&M[w(P+2)]); break;
	case 14: r = remove((char *) &M[w(P+2)]); break;
	case 15: r = t3xtrunc(w(P+2)); break;
	case 16: r = t3xbreak(w(P+2)); break;
	default: fail("bad library call"); r = 0; break;
	}
	return r & 0xffff;
}

void run(void) {
	cell	t;

	P = MEMSIZE;
	F = MEMSIZE;
	for (I = 0;; I++) {
#ifdef DEBUG
	printf("F=%04x P=%04x I=%04x %2x a=%04x A=%04x S0=%04x\n",
		F, P, I, M[I], a(), A, w(P));
#endif
	if (P < Red) fail("stack overflow");
	switch (M[I]) {
	case 0x01: push(A); break;
	case 0x02: A = 0; break;
	case 0x03: P += 2; break;
	case 0x04:
	case 0x05: A = a(); I += 2; break;
	case 0x06: A = F+a() & 0xffff; I += 2; break;
	case 0x07: A = w(a()); I += 2; break;
	case 0x08: A = w(F+a() & 0xffff); I += 2; break;
	case 0x09: Sw(a(), A); I += 2; break;
	case 0x0a: Sw(F+a() & 0xffff, A); I += 2; break;
	case 0x0b: Sw(pop(), A); break;
	case 0x0c: M[pop()] = (byte) A; break;
	case 0x0d: t = a(); Sw(t, w(t)+1 & 0xffff); I += 2; break;
	case 0x0e: t = a(); Sw(F+t & 0xffff, w(F+t & 0xffff)+1 & 0xffff);
		   I += 2; break;
	case 0x0f: A += S(a()); A &= 0xffff; I += 2; break;
	case 0x10:
	case 0x11: P += S(a()); P &= 0xffff; I += 2; break;
	case 0x12: push(P); break;
	case 0x13: Sw(a(), P); I += 2; break;
	case 0x14: A = (A<<1) + pop() & 0xffff; break;
	case 0x15: A = w(A); break;
	case 0x16: A = A + pop() & 0xffff; break;
	case 0x17: A = M[A]; break;
	case 0x18: push(I+2); I = a()-1; break;
	case 0x19: push(I); I = A-1; break;
	case 0x47:
	case 0x1a: I = a()-1; break;
	case 0x1b: I += M[I+1]+1; break;
	case 0x1c: if (0 == A) I = a()-1; else I += 2; break;
	case 0x1d: if (0 != A) I = a()-1; else I += 2; break;
	case 0x1e: if (S(pop()) >= S(A)) I = a()-1; else I += 2; break;
	case 0x1f: if (S(pop()) <= S(A)) I = a()-1; else I += 2; break;
	case 0x20: push(F); F = P; break;
	case 0x21: F = pop(); break;
	case 0x22: I = pop(); break;
	case 0x23: exit(a()); break;
	case 0x24: A = ~A+1 & 0xffff; break;
	case 0x25: A = ~A & 0xffff; break;
	case 0x26: A = 0==A? 0xffff: 0; break;
	case 0x27: A = pop() + A & 0xffff; break;
	case 0x28: A = pop() - A & 0xffff; break;
	case 0x29: A = S(pop()) * S(A) & 0xffff; break;
	case 0x2a: A = S(pop()) / S(A) & 0xffff; break;
	case 0x2b: A = pop() % A & 0xffff; break;
	case 0x2c: A = pop() & A; break;
	case 0x2d: A = pop() | A; break;
	case 0x2e: A = pop() ^ A; break;
	case 0x2f: A = (pop() << A) & 0xffff; break;
	case 0x30: A = pop() >> A; break;
	case 0x31: A = pop() == A? 0xffff: 0; break;
	case 0x32: A = pop() != A? 0xffff: 0; break;
	case 0x33: A = S(pop()) < S(A)? 0xffff: 0; break;
	case 0x34: A = S(pop()) > S(A)? 0xffff: 0; break;
	case 0x35: A = S(pop()) <= S(A)? 0xffff: 0; break;
	case 0x36: A = S(pop()) >= S(A)? 0xffff: 0; break;
	case 0x37: A = pop() * A & 0xffff; break;
	case 0x38: A = pop() / A & 0xffff; break;
	case 0x39: A = pop() < A? 0xffff: 0; break;
	case 0x3a: A = pop() > A? 0xffff: 0; break;
	case 0x3b: A = pop() <= A? 0xffff: 0; break;
	case 0x3c: A = pop() >= A? 0xffff: 0; break;
	case 0x80: A = libcall(M[I+1]); I = pop(); break;
	default:   fail("invalid opcode"); break;
	}}
}

int main(int argc, char **argv) {
	if (argc < 2) fail("usage: tcvm program [args]");
	Args = argv;
	Narg = argc;
	load(argv[1]);
	run();
	return EXIT_SUCCESS;
}
