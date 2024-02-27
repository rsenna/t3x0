! T3X/0 module: CP/NC BDOS Functions
! Nils M Holm, 2023
! In the public domain / 0BSD License

! Some of these override functions of CPM.T

module cpnc;

 public const IOCONMASK = 0x03,
	      IOAUXMASK = 0x0C,
	      IOLSTMASK = 0xC0,
	      IOCONTTY  = 0x00,
	      IOCONCRT  = 0x01,
	      IOCONLPT  = 0x02,
	      IOAUXTTY  = 0x00,
	      IOAUXCRT  = 0x04,
	      IOAUXLPT  = 0x08,
	      IOLSTTTY  = 0x00,
	      IOLSTCRT  = 0x40,
	      IOLSTLPT  = 0x80;

 public auxin()		return t3x.bdos(3, 0);
 public auxout(c)	return t3x.bdos(4, c);
 public auxinst()	return t3x.bdos(7, 0);
 public auxoutst()	return t3x.bdos(8, 0);
 public dskfree(dsk)	return t3x.bdos(46, dsk);
 public absread(rec)	return t3x.bdos(192, rec);
 public abswrite(rec)	return t3x.bdos(193, rec);
 public setabsdsk(dsk)	return t3x.bdos(194, dsk);

end
