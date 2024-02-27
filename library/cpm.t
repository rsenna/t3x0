! T3X/0 module: CP/M 2.2 BDOS Functions
! Nils M Holm, 2023
! In the public domain / 0BSD License

module cpm;

 public const	FCB         = 36,
		FCB_DISK    =  0,
		FCB_NAME    =  1,
		FCB_TYPE    =  9,
		FCB_ROBIT   =  9,
		FCB_SYSBIT  = 10,
		FCB_CHANGED = 11,
		FCB_EXTENT  = 12,
		FCB_RECORDS = 15,
		FCB_BLOCKS  = 16,
		FCB_SEQREC  = 32,
		FCB_RANRECL = 33,
		FCB_RANRECH = 34,
		RCB_RANOVFL = 35;

 public inline expandfn(2) = [ 0xc3, 0x4d, 0x01 ]; ! jp 0x014d

 public sysreset()	return t3x.bdos(0, 0);
 public conin()		return t3x.bdos(1, 0);
 public conout(c)	return t3x.bdos(2, c);
 public readin()	return t3x.bdos(3, 0);
 public punout(c)	return t3x.bdos(4, c);
 public lstout(c)	return t3x.bdos(5, c);
 public conio(c)	return t3x.bdos(6, c);
 public getiob()	return t3x.bdos(7, 0);
 public setiob(iob)	return t3x.bdos(8, iob);
 public prints(s)	return t3x.bdos(9, s);
 public readcons(buf)	return t3x.bdos(10, buf);
 public constat()	return t3x.bdos(11, 0);
 public getver()	return t3x.bdos(12, 0);
 public dskreset()	return t3x.bdos(13, 0);
 public seldsk(dsk)	return t3x.bdos(14, dsk);
 public open(fcb_)	return t3x.bdos(15, fcb_);
 public close(fcb_)	return t3x.bdos(16, fcb_);
 public search(fcb_)	return t3x.bdos(17, fcb_);
 public searchnext()	return t3x.bdos(18, 0);
 public erase(fcb_)	return t3x.bdos(19, fcb_);
 public readseq(fcb_)	return t3x.bdos(20, fcb_);
 public writeseq(fcb_)	return t3x.bdos(21, fcb_);
 public create(fcb_)	return t3x.bdos(22, fcb_);
 public rename(fcb_)	return t3x.bdos(23, fcb_);
 public getlogvec()	return t3x.bdoshl(24, 0);
 public getcurdsk()	return t3x.bdos(25, 0);
 public setdma(dma)	return t3x.bdos(26, dma);
 public getalvec()	return t3x.bdoshl(27, 0);
 public setdskro()	return t3x.bdos(28, 0);
 public getrodsks()	return t3x.bdoshl(29, 0);
 public setfat(fcb_)	return t3x.bdos(30, fcb_);
 public getdpb()	return t3x.bdoshl(31, 0);
 public getsetusr(n)	return t3x.bdos(32, n);
 public readran(fcb_)	return t3x.bdos(33, fcb_);
 public writeran(fcb_)	return t3x.bdos(34, fcb_);
 public getfsiz(fcb_)	return t3x.bdos(35, fcb_);
 public setranrec(fcb_)	return t3x.bdos(36, fcb_);
 public resetdsks(map)	return t3x.bdos(37, map);
 public writeranz(fcb_)	return t3x.bdos(40, fcb_);

end
