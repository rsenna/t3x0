! T3X/0 module: dynamic memory
! Nils M Holm, 1997-2002,2022
! In the public domain / 0BSD License

! needs: t3x

module memory;

 var	Arena;		! ptr to user-supplied pool
 var	Freezone;	! ptr to first free block in Arena
 var	FreeBit;	! free block indicator bit (set=free)
 var	BytesPerWord;	! cache for T.BPW()

 public init(pool, size) do
	BytesPerWord := t3x.bpw();
	FreeBit := 0x8000;
	if (BytesPerWord >= 4) FreeBit := FreeBit << 16; ! 0x80000000
	Arena := pool;
	size := size/BytesPerWord;
	Arena[0] := size-1 | FreeBit;
	Arena[size-1] := 0;
	Freezone := 0;
 end

 public walk(blk, sizep, statp) do
	ie (blk)
		blk := @blk[(blk[%1] & ~FreeBit) - 1];
	else
		blk := Arena;
	if (sizep) sizep[0] := ((blk[0] & ~FreeBit) - 1) * BytesPerWord;
	if (statp) statp[0] := (blk[0] & FreeBit) \= 0;
	if (\blk[0]) return 0;
	return @blk[1];
 end

 public alloc(size) do var ap, k, i;
	! round up, add status word
	size := (size+BytesPerWord-1)/BytesPerWord + 1;
	while (1) do
		ap := Freezone;
		while (Arena[ap]) do
			k := Arena[ap];
			if (k & FreeBit) do
				k := k & ~FreeBit;
				! free block fits exactly:
				! just clear its FreeBit
				ie (size = k) do
					Freezone := ap;
					Arena[ap] := k;
					return @Arena[ap+1];
				end
				! free block larger than requested:
				! split and allocate first part
				else if (size < k) do
					Freezone := ap;
					Arena[ap] := size;
					Arena[ap+size] := k-size | FreeBit;
					return @Arena[ap+1];
				end
			end
			ap := ap + (k & ~FreeBit);
		end
		if (Freezone = 0) return 0;	! already searched whole pool
		Freezone := 0;
	end
 end

 public free(blk) do
	var	ap, k;
	var	head, tail;
	var	badblk;

	badblk := "memory.free(): bad block\r\n";
	if (blk .< Arena \/ blk[%1] & FreeBit) do
		t3x.write(T3X.SYSERR, badblk, 26);
		halt 1;
	end
	! first free the block
	blk[%1] := blk[%1] | FreeBit;
	ap := 0;
	head := %1;
	! then collect leading free blocks
	while (@Arena[ap] .< blk) do
		if (Arena[ap] = 0) do
			t3x.write(T3X.SYSERR, badblk, 26);
			halt 1;
		end
		ie (Arena[ap] & FreeBit) do
			if (head = %1) head := ap;
			if (ap .< Freezone) Freezone := ap;
		end
		else do
			head := %1;
		end
		tail := ap;
		ap := ap + (Arena[ap] & ~FreeBit);
	end
	! and trailing free blocks
	while (Arena[ap]) do
		if (\(Arena[ap] & FreeBit)) leave;
		tail := ap;
		ap := ap + (Arena[ap] & ~FreeBit);
	end
	tail := tail + (Arena[tail] & ~FreeBit);
	! connect all continous free space found around blk
	Arena[head] := tail-head | FreeBit;
 end

end
