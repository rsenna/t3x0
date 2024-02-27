! T3X core module for generic Unix on the 386
! Nils M Holm, 2022, 2023
! Public Domain / 0BSD license

module t3x;

 public const	SYSIN = 0,
		SYSOUT = 1,
		SYSERR = 2;

 public const	OREAD = 0,
		OWRITE = 1,
		ORDWR = 2,
		OAPPND = 3;

 public const	SEEK_SET = 0,
		SEEK_FWD = 1,
		SEEK_END = 2,
		SEEK_BCK = 3;

 public extern	bpw(0),
		newline(1),
		memcomp(3),
		memcopy(3),
		memfill(3),
		memscan(3),
		getarg(3),
		create(1),
		open(2),
		close(1),
		read(3),
		write(3),
		seek(3),
		rename(2),
		remove(1),
		trunc(1),
		break(1);

end
