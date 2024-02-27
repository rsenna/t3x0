! T3X module: I/O streams
! Nils M Holm, 1997,1998,2000,2002,2019,2022,2023
! Public Domain / 0BSD License

! needs: t3x

module iostream;

 public struct	IOS =	IOS_FD,		! file descriptor
			IOS_BUF,	! ptr to buffer
			IOS_LEN,	! 
			IOS_PTR,	!
			IOS_LIM,	!
			IOS_FLAGS;	!

 public const	FREAD	= 0x0001,	! read-only
		FWRITE	= 0x0002,	! write-only
		FRDWR	= FREAD|FWRITE,	! read/write
		FKILLCR	= 0x0004,	! remove CRs from input
		FADDCR	= 0x0008,	! add CR before LF in output
		FTRANS	= FKILLCR|FADDCR;

 const		FEOF	= 0x0100,	! EOF detected
		FLASTW	= 0x0200;	! last access was of type 'write'

 ! seek modes
 public const	SEEK_SET = T3X.SEEK_SET,	! from beginning
		SEEK_FWD = T3X.SEEK_FWD,	! relative/forward
		SEEK_END = T3X.SEEK_END,	! from end (backward)
		SEEK_BCK = T3X.SEEK_BCK;	! relative/backward

 iosreset(ios_) do
	ios_[IOS_PTR] := 0;
	ios_[IOS_LIM] := 0;
 end

 public create(ios_, fd, bufp, len, mode) do
	ios_[IOS_FD] := fd;
	ios_[IOS_BUF] := bufp;
	ios_[IOS_FLAGS] := mode;
	ios_[IOS_LEN] := len;
	iosreset(ios_);
	return ios_;
 end

 public open(ios_, name, fl) do var fd, mode, ifl;
	ifl := fl & FRDWR;
	mode := ifl = FREAD-> T3X.OREAD:
		ifl = FWRITE-> T3X.OWRITE:
		ifl = FREAD|FWRITE-> T3X.ORDWR: %1;
	if (mode < 0) return %1;
	fd := t3x.open(name, mode);
	if (fd < 0) return %1;
	ios_[IOS_FD] := fd;
	iosreset(ios_);
	return ios_;
 end

 public flush(ios_) do var k;
	if (ios_[IOS_FLAGS] & (FWRITE|FLASTW) = FWRITE|FLASTW /\ ios_[IOS_PTR])
	do
		k := t3x.write(ios_[IOS_FD], ios_[IOS_BUF], ios_[IOS_PTR]);
		if (k \= ios_[IOS_PTR]) return %1;
	end
	iosreset(ios_);
 end

 public close(ios_) do
	if (flush(ios_) = %1) return %1;
	t3x.close(ios_[IOS_FD]);
	ios_[IOS_FLAGS] := 0;
 end

 written(ios_) ios_[IOS_FLAGS] := ios_[IOS_FLAGS] |  FLASTW;
 readd(ios_)   ios_[IOS_FLAGS] := ios_[IOS_FLAGS] & ~FLASTW;

 public wrch(ios_, ch) do
	written(ios_);
	if (ios_[IOS_FLAGS] & FADDCR /\ ch = '\n') wrch(ios_, '\r');
	if (ios_[IOS_PTR] >= ios_[IOS_LEN] /\ flush(ios_) = %1) return %1;
	ios_[IOS_BUF]::ios_[IOS_PTR] := ch;
	ios_[IOS_PTR] := ios_[IOS_PTR]+1;
	return ch;
 end

 public write(ios_, buf, len) do var i;
	written(ios_);
	i := 0;
	while (i < len) do
		if (ios_[IOS_PTR] >= ios_[IOS_LEN] /\ flush(ios_) = %1)
			return %1;
		if (ios_[IOS_FLAGS] & FADDCR) do
			if (buf::i = '\n') do
				ios_[IOS_BUF]::ios_[IOS_PTR] := '\r';
				ios_[IOS_PTR] := ios_[IOS_PTR]+1;
				if (	ios_[IOS_PTR] >= ios_[IOS_LEN] /\
					flush(ios_) = %1
				)
					return %1;
			end
		end
		ios_[IOS_BUF]::ios_[IOS_PTR] := buf::i;
		ios_[IOS_PTR] := ios_[IOS_PTR]+1;
		i := i+1;
	end
	return i;
 end

 public writes(ios_, str) do var k;
	k := t3x.memscan(str, 0, 32767);
	if (k .> 32766) return %1;
	return write(ios_, str, k);
 end

 more(ios_) do var k;
	if (ios_[IOS_FLAGS] & FREAD) do
		k := t3x.read(ios_[IOS_FD], ios_[IOS_BUF], ios_[IOS_LEN]);
		if (k <= 0) do
			ios_[IOS_FLAGS] := ios_[IOS_FLAGS] | FEOF;
			if (k < 0) return %1;
		end
		ios_[IOS_PTR] := 0;
		ios_[IOS_LIM] := k;
		return k;
	end
 end

 public rdch(ios_) do var c;
	readd(ios_);
	while (1) do
		if (ios_[IOS_FLAGS] & FEOF) return %1;
		if (ios_[IOS_PTR] >= ios_[IOS_LIM] /\ more(ios_) < 1) return %1;
		c := ios_[IOS_BUF]::ios_[IOS_PTR];
		ios_[IOS_PTR] := ios_[IOS_PTR]+1;
		ie (ios_[IOS_FLAGS] & FKILLCR)
			if (c \= '\r') leave;
		else
			leave;
	end
	return c;
 end

 doread(ios_, buf, len, ckln) do var i;
	readd(ios_);
	i := 0;
	while (i < len) do
		if (ios_[IOS_PTR] >= ios_[IOS_LIM] /\ more(ios_) < 1) leave;
		ie (ios_[IOS_FLAGS] & FKILLCR) do
			if (ios_[IOS_BUF]::ios_[IOS_PTR] \= '\r') do
				buf::i := ios_[IOS_BUF]::ios_[IOS_PTR];
				i := i+1;
			end
		end
		else do
			buf::i := ios_[IOS_BUF]::ios_[IOS_PTR];
			i := i+1;
		end
		ios_[IOS_PTR] := ios_[IOS_PTR]+1;
		if (ckln /\ buf::(i-1) = '\n') leave;
	end
	if (ckln) buf::i := 0;
	return i;
 end

 public read(ios_, buf, len) return doread(ios_, buf, len, 0);

 public reads(ios_, buf, len) return doread(ios_, buf, len, 1);

 public move(ios_, off, how) do var delta;
	ie (ios_[IOS_FLAGS] & FLASTW) do
		if (flush(ios_) = %1) return %1;
	end
	else ie (how = SEEK_FWD) do
		delta := ios_[IOS_LIM] - ios_[IOS_PTR];
		if (flush(ios_) = %1) return %1;
		off := off - delta;
	end
	else ie (how = SEEK_BCK) do
		delta := ios_[IOS_LIM] - ios_[IOS_PTR];
		if (flush(ios_) = %1) return %1;
		off := off + delta;
	end
	else do
		if (flush(ios_) = %1) return %1;
	end
	return t3x.seek(ios_[IOS_FD], off, how);
 end

 public eof(ios_) return (ios_[IOS_FLAGS] & FEOF) \= 0;

 public reset(ios_) ios_[IOS_FLAGS] := ios_[IOS_FLAGS] & ~FEOF;

end
