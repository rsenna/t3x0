! T3X core module for the Tcode/0 TCVM
! Nils M Holm, 2022
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

 public inline	bpw(0) = [ 0x80, 0x00 ],
		newline(1) = [ 0x80, 0x01 ],
		memcomp(3) = [ 0x80, 0x02 ],
		memcopy(3) = [ 0x80, 0x03 ],
		memfill(3) = [ 0x80, 0x04 ],
		memscan(3) = [ 0x80, 0x05 ],
		getarg(3) = [ 0x80, 0x06 ],
		create(1) = [ 0x80, 0x07 ],
		open(2) = [ 0x80, 0x08 ],
		close(1) = [ 0x80, 0x09 ],
		read(3) = [ 0x80, 0x0a ],
		write(3) = [ 0x80, 0x0b ],
		seek(3) = [ 0x80, 0x0c ],
		rename(2) = [ 0x80, 0x0d ],
		remove(1) = [ 0x80, 0x0e ],
		trunc(1) = [ 0x80, 0x0f ],
		break(1) = [ 0x80, 0x10 ];

end
