; T3X/0 runtime library for CP/M on the Z80
; Nils M Holm, 2019,2020,2023
; Public Domain / 0BSD license

include cpmdefs.asm

	org	100h	; zmac
;	cseg		; m80, zsm4

rtlib:	jp	start

	; The following definitions MUST start at 0103h
	; and MUST be kept in this order!

	defw	rtlen		; 0103 module length for mklib.t
	defb	'T3X'		; 0105

	jp	mul15s		; 0108
	jp	div15s		; 010b
	jp	mod16		; 010e
	jp	mul16		; 0111
	jp	div16		; 0114
	jp	cmp15s		; 0117
	jp	cmp16		; 011a
	jp	t_bpw		; 011d
	jp	t_newline	; 0120
	jp	t_memcomp	; 0123
	jp	t_memcopy	; 0126
	jp	t_memfill	; 0129
	jp	t_memscan	; 012c
	jp	t_getarg	; 012f
	jp	t_open		; 0132
	jp	t_close		; 0135
	jp	t_read		; 0138
	jp	t_write		; 013b
	jp	t_rename	; 013e
	jp	t_remove	; 0141
	jp	t_bdos		; 0144
	jp	t_bdoshl	; 0147
	jp	(hl)		; 014a
	nop
	nop
	jp	t_expandfn	; 014d

fcb3:	defw	0	; FCBs and buffers for file descriptors 3,
fcb4:	defw	0	; 4,
fcb5:	defw	0	; and 5
tbuf:	defw    0	; temporary buffer

; Call frame layout
;
; +-----------------+
; | argument 1      | <-- IX+2+2N
; +-----------------+
; | ...             |
; +-----------------+
; | argument N      | <-- IX+4
; +-----------------+
; | saved frame     |
; +-----------------+
; | return address  | <-- SP, IX
; +-----------------+

; 16x16 unsigned multiplication, HL = HL*DE.
; Algorithm from Rodney Zaks, "Programming the Z80".

mul16:	ld   a,l	; transfer HL to CA
	ld   c,h
	ld   b,16	; 16 bits to multiply
	ld   hl,0
mul0:	srl  c		; shift CA right, get low bit
	rra
	jr   nc,mul1	; zero fell out, do not add
	add  hl,de	; else add DE
mul1:	ex   de,hl	; DE = DE*2
	add  hl,hl
	ex   de,hl
	djnz mul0
	ret

; 16/16 unsigned division, HL = HL div DE, DE = HL rem DE.
; Algorithm from Rodney Zaks, "Programming the Z80".

div16:	ld   a,h	; transfer HL to AC
	ld   c,l
	ld   hl,0	; intermediate result
	ld   b,16	; 16 bits to divide
div0:	rl   c		; get AC high bit, rotate in result bit
	rla
	adc  hl,hl	; HL = HL*2, never sets C
	sbc  hl,de	; trial subtract and test DE > HL
	jr   nc,div1
	add  hl,de	; DE > HL, restore HL
div1:	ccf		; result bit
	djnz div0
	rl   c		; rotate in last result bit
	rla
	ld   d,a
	ld   e,c
	ex   de,hl
	ret

; extract common sign from two mul/div factors (HL, DE);
; return CY=0, if signs are equal and otherwise CY=1
sign:	ld   a,h
	xor  d
	rla		; sign to carry
	ret

; strip signs from HL and DE
strip:	bit  7,d
	jr   z,sb0
	ld   a,d
	cpl
	ld   d,a
	ld   a,e
	cpl
	ld   e,a
	inc  de
sb0:	bit  7,h
	ret  z
neghl:	ld   a,h
	cpl
	ld   h,a
	ld   a,l
	cpl
	ld   l,a
	inc  hl
	ret

; 15x15 signed multiplication
mul15s:	call sign
	push af
	call strip
	call mul16
	pop  af
	ret  nc
	jr   neghl

; 15/15 signed division
div15s:	ex   de,hl
	call sign
	push af
	call strip
	call div16
	pop  af
	ret  nc
	jr   neghl

; 15/15 unsigned remainder
mod16:	ex   de,hl
	call div16
	ex   de,hl
	ret

; signed comparison HL-DE, set Z and CY flags,
; where CY indicates that HL < DE
cmp15s:	xor  a
	sbc  hl,de
	ret  z
	jp   m,cs1
	or   a
	ret
cs1:	scf
	ret

cmp16:	xor  a
	sbc  hl,de
	ret

; T.BPW()
; Return bytes per word on target machine.
t_bpw:
	ld   hl,2
	ret

; T.NEWLINE(B)
; Fill B with newline sequence.
t_newline:
	push ix
	ld   ix,0
	add  ix,sp
	ld   h,(ix+5)	; HL = B
	ld   l,(ix+4)
	push hl
	ld   (hl),0Dh	; B::0 = 0Dh
	inc  hl
	ld   (hl),0Ah	; B::1 = 0Ah
	inc  hl
	ld   (hl),0	; B::2 = 0
	pop  hl		; return B
	pop  ix
	ret

; T.MEMCOMP(R1, R2, N)
; Compare regions R1 and R2 of size N,
; return difference between first differing
; pair of bytes; return 0 if R1=R2
t_memcomp:
	push ix
	ld   ix,0
	add  ix,sp
	ld   h,(ix+9)	; fetch args
	ld   l,(ix+8)	; R1
	ld   d,(ix+7)	; R2
	ld   e,(ix+6)
	ld   b,(ix+5)	; N
	ld   c,(ix+4)

	ld   a,b	; nothing to compare?
	or   c
	jr   z,mc0a	; always succeed

mc0:	ld   a,(de)	; (hl) = (de)?
	cp   (hl)
	jr   nz,mc1	; no? return difference
	inc  de		; else proceed with next char pair
	inc  hl
	dec  bc
	ld   a,b
	or   c
	jr   nz,mc0	; repeat until BC=0

mc0a:	ld   hl,0	; regions are equal
	jr   mc2

mc1:	ld   e,a	; return (hl)-(de)
	ld   l,(hl)
	ld   h,0
	ld   d,0
	xor  a
	sbc  hl,de

mc2:	pop  ix
	ret

; T.MEMCOPY(DEST, SRC, N)
; Copy N bytes from SRC to DEST;
; regions may overlap.
t_memcopy:
	push ix
	ld   ix,0
	add  ix,sp
	ld   d,(ix+9)	; fetch args, DEST
	ld   e,(ix+8)
	ld   h,(ix+7)	; SRC
	ld   l,(ix+6)
	ld   b,(ix+5)	; N
	ld   c,(ix+4)

	ld   a,b
	or   c
	jr   z,mcp1

	push hl		; is (hl) >= (de)?
	xor  a
	sbc  hl,de
	pop  hl
	jp   z,mcp1	; (hl) = (de)
	jr   nc,mcp0

	add  hl,bc	; (hl) > (de), copy from end
	ex   de,hl
	add  hl,bc
	ex   de,hl
	dec  hl
	dec  de
	lddr
	ld   hl,0
	pop  ix
	ret

mcp0:	ldir		; (hl) < (de), copy from beginning

mcp1:	ld   hl,0
	pop  ix
	ret

; T.MEMFILL(R, C, N)
; Fill N bytes starting at R with C.
t_memfill:
	push ix
	ld   ix,0
	add  ix,sp
	ld   h,(ix+9)	; fetch args
	ld   l,(ix+8)	; R
	ld   e,(ix+6)	; C, we use only the LSB
	ld   b,(ix+5)	; N
	ld   c,(ix+4)

	ld   (hl),e	; place C in R::0
	ld   d,h	; and then copy it forward BC-1 times
	ld   e,l
	inc  de
	dec  bc
	ldir

	ld   hl,0
	pop  ix
	ret

; T.MEMSCAN(R, C, N)
; Find first byte C in region R of size N;
; return offset of the byte or -1, if C is not in R.
t_memscan:
	push ix
	ld   ix,0
	add  ix,sp
	ld   h,(ix+9)	; fetch args
	ld   l,(ix+8)	; R
	ld   a,(ix+6)	; C, we use only the LSB
	ld   b,(ix+5)	; N
	ld   c,(ix+4)

	inc  bc		; search one byte past region
	cpir

	ld   a,b	; if search went past region, fail
	or   c
	jp   z,fail

	ld   d,(ix+9)	; get offset: R::HL-R
	ld   e,(ix+8)
	xor  a
	sbc  hl,de
	dec  hl

	pop  ix
	ret

; Skip to next non-blank token after the one pointed to by HL;
; stop after B characters

skipto:	ld   a,(hl)	; first skip over any non-blanks
	cp   32
	jr   z,sk0
	inc  hl
	djnz skipto
	ret

sk0:	ld   a,(hl)	; then skip over blanks
	cp   32
	ret  nz
	inc  hl
	djnz sk0
	ret

; T.GETARG(A, BUF, N)
; Extract up to N-1 characters from A'th command line argument,
; store the characters in BUF and append a delimiting NUL char.
; Return the number of characters extracted (excluding the NUL).

t_getarg:
	push ix
	ld   ix,0
	add  ix,sp
	ld   a,(ix+9)	; A>255? then fail
	or   a
	jp   nz,fail

	ld   c,(ix+8)	; A
	ld   a,(COMTAIL)	; B = max chars to search/extract
	or   a			; COMTAIL empty?
	jp   z,fail		; then fail

	ld   b,a
	ld   hl,COMTAIL+1

ga0:	call skipto	; skip to next argument

	ld   a,b	; end of COMTAIL reached?
	or   a
	jp   z,fail	; then fail

	dec  c		; got the desired one?
	jr   z,ga1	; then extract it

	jr   ga0	; proceed with next argument

	; now extract the argument

ga1:	ld   d,(ix+7)	; DE = BUF
	ld   e,(ix+6)
	ld   c,(ix+4)	; C = max. chars to extract
	dec  c

ga2:	ld   a,(hl)	; get one char from argument
	cp   32		; is it a blank?
	jr   z,ga3	; then that's it

	ld   (de),a
	inc  hl
	inc  de
	dec  c		; decrement maximum
	jr   z,ga3	; maximum zero? then that's it

	djnz ga2

ga3:	xor  a		; append NUL
	ld   (de),a
	ld   h,(ix+7)
	ld   l,(ix+6)
	xor  a
	ex   de,hl
	sbc  hl,de

	pop  ix
	ret

; Convert character in A to upper case.
upcase:	cp  'a'
	ret c
	cp  'z'+1
	ret nc
	sub 'a'-'A'
	ret

; Expand NUL-terminated filename in string at HL
; to FCB at DE, zap the rest of the FCB.
; Return CY=1 if file name is invalid.

expandfn:
	push hl
	inc  hl		; is there a drive name?
	ld   a,(hl)
	dec  hl
	cp   ':'
	jr   z,exdrv	; yes? use it

	xor  a		; else use default disk
	jr   exnam

exdrv:	ld   a,(hl)	; extract drive ID
	call upcase
	inc  hl
	inc  hl
	sub  'A'-1

exnam:	ld   (de),a	; store drive ID
	inc  de

	ld   b,8	; extract up to 8 file name characters
cpnam:	ld   a,(hl)
	or   a		; NUL? fill the rest with spaces
	jr   z,filsp
	cp   '.'	; '.'? fill the rest with spaces
	jr   z,filsp
	inc  hl
	call upcase	; else store the extracted char
	ld   (de),a
	inc  de
	djnz cpnam
	jr   exdot	; 8 characters extracted, expect '.'

filsp:	ld   a,32	; fill with B blanks (B left over from loop above)
fill8:	ld   (de),a
	inc  de
	djnz fill8

exdot:	ld   a,(hl)	; expect '.' or end of name
	or   a
	jr   z,notyp	; end of name, fill type with blanks
	cp   '.'
	jr   nz,exfail	; no '.'? give up
	inc  hl		; skip over dot and extract type
	jr   extyp

notyp:	ld   b,3	; no type given, fill with 3 blanks
	jr   typsp

extyp:	ld   b,3	; extract up to 3 file type characters
cptyp:	ld   a,(hl)
	or   a		; NUL? fill the rest with spaces
	jr   z,typsp
	inc  hl
	call upcase	; else store the extracted char
	ld   (de),a
	inc  de
	djnz cptyp
	jr   exdone	; 3 chars extracted, done

typsp:	ld   a,32	; fill with B blanks (B left over from loop above)
fill3:	ld   (de),a
	inc  de
	djnz fill3

	ld   a,(hl)	; trailing characters?
	or   a
	jr   nz,exfail	; then give up

exdone:	ld   h,d
	ld   l,e
	ld   b,24	; zap rest of FCB
exz:	ld   (hl),0
	inc  hl
	djnz exz
	pop  hl
	or   a
	ret

	; give up

exfail:	pop  hl
	scf
	ret

; T.EXPANDFN(S, B)
t_expandfn:
	push ix
	ld   ix,0
	add  ix,sp

	ld   h,(ix+7)	; S
	ld   l,(ix+6)
	ld   d,(ix+5)   ; B
	ld   e,(ix+4)

	push de
	call expandfn
	pop  hl
	jp   c,fail

	pop  ix
	ret

; T.OPEN(NAME, MODE)
; Open file NAME.
; If MODE=0 open existing file for reading.
; If MODE=1 erase and create file.

; Internal file descriptor layout:
;
;            +36    +37    +38
; +---...---+------+------+---...----+
; |   FCB   | PTR  | TYPE |  BUFFER  |
; +---...---+------+------+---...----+
;  36 bytes  1 byte 1 byte 128 bytes
;
; In an unused descriptor, the first byte
; of the FCB is set to FFh.

PTR	equ	36
TYPE	equ	37
BUFR	equ	38

handle:	defb	0	; file descriptor id
file:	defw	0	; pointer to file descriptor

t_open:
	push ix
	ld   ix,0
	add  ix,sp

	; do not bother with a loop, just try
	; the available descriptors in sequence

	ld   hl,handle	; try descriptor 3
	ld   (hl),3
	ld   de,(fcb3)
	ld   a,(de)
	inc  a
	jr   z,op0	; unused? open it

	inc  (hl)	; try descriptor 4
	ld   de,(fcb4)
	ld   a,(de)
	inc  a
	jr   z,op0	; unused? open it

	inc  (hl)	; try descriptor 5
	ld   de,(fcb5)
	ld   a,(de)
	inc  a
	jr   z,op0	; unused? open it

	jp   fail	; no descriptors unused

op0:	ld   a,(ix+5)
	or   a
	jp   nz,fail	; MODE > 255, fail
	ld   a,(ix+4)
	or   a		; MODE = 0, open for reading
	jr   z,oprd

	cp   2		; MODE > 1 not supported
	jp   nc,fail

	; else open for writing

opwr:	ld   h,(ix+7)	; file name
	ld   l,(ix+6)
	push de
	call expandfn	; expand to FCB format
	pop  de
	jp   c,fail
	push ix		; save stack frame and FCB
	push de
	ld   c,BDERASE	; erase file first
	call BDOS
	pop  de
	push de
	ld   c,BDCREATE	; then create it
	call BDOS
	pop  de		; restore FCB and stack frame
	pop  ix
	inc  a		; creation failed?
	jp   z,fail
	jr   op2

	; open for reading

oprd:	ld   h,(ix+7)	; file name
	ld   l,(ix+6)
	push de
	call expandfn
	pop  de
	jp   c,fail
	push ix
	push de
	ld   c,BDOPEN	; open existing file
	call BDOS
	pop  de
	pop  ix
	inc  a		; failed?
	jp   z,fail

	; open or create done

op2:	ld   (file),de	; memoize FCB
	ld   hl,PTR	; HL = PTR
	add  hl,de
	ld   a,(ix+4)	; file type
	or   a
	jr   z,op3

	xor  a		; when writing file, set PTR to 0
	jr   op4

op3:	ld   a,128	; when reading, set PTR to end of buffer

op4:	ld   (hl),a	; set PTR
	inc  hl
	ld   a,(ix+4)	; also set TYPE
	ld   (hl),a

	call clear	; clear buffer

	ld   a,(handle)	; return handle
	ld   l,a
	ld   h,0
	pop  ix
	ret

; T.CLOSE(FD)
; Close file descriptor.

t_close:
	push ix
	ld   ix,0
	add  ix,sp

	ld   a,(ix+5)	; FD>255? then fail
	or   a
	jp   nz,fail

	ld   a,(ix+4)	; find descriptor
	ld   de,(fcb3)
	cp   3
	jr   z,cl0

	ld   de,(fcb4)
	cp   4
	jr   z,cl0

	ld   de,(fcb5)
	cp   5
	jr   z,cl0

	jp   fail	; none of 3,4,5, so fail

cl0:	push de
	pop  iy
	ld   a,(de)	; if not open, fail
	cp   255
	jp   z,fail
	ld   a,(iy+TYPE); TYPE = 0? (file read-only)
	or   a
	jr   z,cl1	; then just close it

	ld   a,(iy+PTR)	; unwritten data in buffer?
	or   a
	jr   z,cl1	; no? then just close it

	ld   (file),de	; flush unwritten data
	call flush
	or   a
	jp   nz,fail

cl1:	push de		; FCB
	ld   c,BDCLOSE	; now close the file
	call BDOS
	pop  hl		; FCB
	inc  a		; close failed?
	jp   z,fail

	ld   (hl),0FFh	; mark descriptor unused
	ld   hl,0

	pop  ix
	ret

; Read up to 128 characters from the console
; (used by T.READ)

crlf:	defb 13,10,'$'

rdcon:
	ld   b,(ix+5)	; number N of chars to read
	ld   c,(ix+4)

	ld   a,b	; no bytes to read?
	or   c
	jr   nz,rc1
	ld   hl,0	; then succeed trivially
	jr   rc0

rc1:	ld   a,b	; N > 255?
	or   a
	jp   nz,fail	; then fail

	ld   a,c	; N > 128?
	cp   81h
	jp   nc,fail	; still fail

	ld   iy,(tbuf)	; use TBUF for reading
	ld   (iy+0),a	; TBUF::0 = A

	push ix		; save register during BDOS call
	push iy

	ld   c,BDREADCONS	; read
	ld   de,(tbuf)
	call BDOS
	ld   c,BDPRINTS	; print CR,LF
	ld   de,crlf
	call BDOS

	pop  iy		; restore registers
	pop  ix

	ld   a,(iy+1)	; did we read anything?
	or   a
	jr   nz,rc0a	; no? return single LF

	ld   hl,0
	ld   d,(ix+7)	; BUF
	ld   e,(ix+6)
	jr   addlf

rc0a:	ld   c,a	; else move TBUF to BUF argument of T.READ
	ld   b,0
	push bc		; remember number of chars

	ld   hl,(tbuf)	; move to data
	inc  hl
	inc  hl
	ld   d,(ix+7)	; BUF
	ld   e,(ix+6)
	ldir

	pop  hl		; number of chars read
	push hl

	ld   b,(ix+5)	; number N of chars to read
	ld   c,(ix+4)

	xor  a
	sbc  hl,bc
	pop  hl
	jr   z,rc0

addlf:	ld   a,10
	ld   (de),a
	inc  hl

rc0:	pop  ix
	ret

; T.READ(FD, BUF, N)
; Read up to N bytes from FD into BUF.

t_read:	push ix
	ld   ix,0
	add  ix,sp

	ld   a,(ix+9)	; FD > 255?
	or   a
	jp   nz,fail	; then fail

	ld   a,(ix+8)	; FD = 0?
	or   a
	jp   z,rdcon	; then read the console

	ld   de,(fcb3)	; else try the file descriptors
	cp   3
	jp   z,rdfile

	ld   de,(fcb4)
	cp   4
	jp   z,rdfile

	ld   de,(fcb5)
	cp   5
	jp   z,rdfile

	jp   fail

rdfile:	ld   b,(ix+5)	; number of bytes to read
	ld   c,(ix+4)

	ld   a,b	; no bytes to read?
	or   c
	jr   z,rf2	; then succeed trivially

	ld   (file),de	; remember FD

	ld   hl,BUFR	; HL = BUFFER
	add  hl,de

	ld   iy,(file)	; DE = @BUFFER::PTR
	ld   e,(iy+PTR)
	ld   d,0
	add  hl,de
	ex   de,hl

	ld   h,(ix+7)	; HL = BUF argument of T.WRITE
	ld   l,(ix+6)

rf0:	ld   iy,(file)	; PTR = 128?
	ld   a,(iy+PTR)
	cp   128
	jr   nz,rf1

	call more	; buffer empty, read more
	or   a		; and exit if read failed
	jr   nz,rf2

	call resetptr

rf1:	ex   de,hl	; transfer byte from BUFFER to BUF
	ldi	
	ex   de,hl
	inc  (iy+PTR)	; increment PTR

	ld   a,b	; still bytes to read?
	or   c
	jr   nz,rf0	; yes? then loop

rf2:	ld   h,(ix+5)	; return number of bytes actually read
	ld   l,(ix+4)
	xor  a
	sbc  hl,bc
	pop  ix
	ret

; Re-fill buffer of read-only file descriptor
more:	push hl		; save registers
	push de
	push bc
	push ix

	ld   c,BDSETDMA	; set DMA address to BUFFER
	ld   de,(file)
	ld   hl,BUFR
	add  hl,de
	ex   de,hl
	call BDOS

	ld   c,BDREADSEQ	; read new record
	ld   de,(file)
	call BDOS

	pop  ix		; restore registers
	pop  bc
	pop  de
	pop  hl
	ret

; reset pointers in READ/WRITE operations
; FILE = file descriptor
; Output:
; PTR = 0
; IY = pointer to file descriptor
; DE = pointer to buffer

resetptr:
	ld   iy,(file)	; reset PTR
	xor  a
	ld   (iy+PTR),0

	ld   de,(file)	; reset DE to @BUFFER:0
	push hl
	ld   hl,BUFR
	add  hl,de
	ex   de,hl
	pop  hl

	ret

; clear buffer of a file descriptor at (file)
; (fill with SUB chars)
clear:	ld   de,(file)
	ld   hl,BUFR	; move to BUFFER
	add  hl,de
	ld   d,h
	ld   e,l
	inc  de		; fill with SUB
	ld   bc,127
	ld   (hl),1Ah
	ldir
	ret

; Flush buffer of write-only file descriptor
flush:	push hl		; save registers
	push de
	push bc
	push ix

	ld   c,BDSETDMA	; set DMA address to BUFFER
	ld   de,(file)
	ld   hl,BUFR
	add  hl,de
	ex   de,hl
	call BDOS

	ld   c,BDWRITESEQ	; write the buffer
	ld   de,(file)
	call BDOS
	push af		; save result

	call clear	; clear the buffer

	pop  af		; restore result

	pop  ix		; also restore other registers
	pop  bc
	pop  de
	pop  hl
	ret

; Write buffer to file (used by T.WRITE)

wrfile:	ld   b,(ix+5)	; number of bytes to write
	ld   c,(ix+4)

	ld   a,b	; nothing to write?
	or   c
	jr   z,wf2	; then succeed trivially

	ld   (file),de	; remember FD

	ld   hl,BUFR	; HL = BUFFER
	add  hl,de

	ld   iy,(file)	; DE = @BUFFER::PTR
	ld   e,(iy+PTR)
	ld   d,0
	add  hl,de
	ex   de,hl

	ld   h,(ix+7)	; HL = BUF argument of T.WRITE
	ld   l,(ix+6)

wf0:	ld   iy,(file)	; PTR = 128?
	ld   a,(iy+PTR)
	cp   128
	jr   nz,wf1

	call flush	; buffer full, flush
	or   a		; and test for write failure
	jr   nz,fail

	call resetptr

wf1:	ldi		; transfer byte from BUF to BUFFER
	inc  (iy+PTR)	; increment PTR

	ld   a,b	; still bytes to write?
	or   c
	jr   nz,wf0	; yes? then loop

wf2:	ld   h,(ix+5)	; all bytes written successfully
	ld   l,(ix+4)
	pop  ix
	ret

; Common exit point in case of failure

fail:	ld   hl,0FFFFh
	pop  ix
	ret

; Write characters to the console
; (used by T.WRITE)

wrcon:	ld   h,(ix+7)	; BUF
	ld   l,(ix+6)
	ld   b,(ix+5)	; number of chars to write
	ld   c,(ix+4)

	push bc		; save count

	ld   a,b	; nothing to write?
	or   c
	jr   z,wc1	; then succeed trivially

wc0:	ld   a,(hl)	; fetch next char
	push hl
	push bc
	ld   c,BDCONOUT	; write it
	ld   e,a
	call BDOS
	pop  bc
	pop  hl

	inc  hl		; advance to next char
	dec  bc

	ld   a,b	; more to write?
	or   c
	jr   nz,wc0	; yes? then loop

wc1:	pop  hl		; return saved count
	pop  ix
	ret

; T.WRITE(FD, BUF, N)
; WRITE N bytes from BUF to FD.

t_write:
	push ix
	ld   ix,0
	add  ix,sp

	ld   a,(ix+9)	; FD > 255?
	or   a
	jr   nz,fail	; then fail

	ld   a,(ix+8)	; writing to T3X.SYSOUT (console)?
	cp   1
	jr   z,wrcon

	cp   2		; T3X.SYSERR is also console
	jr   z,wrcon

	cp   3		; otherwise try the FDs
	ld   de,(fcb3)
	jp   z,wrfile

	cp   4
	ld   de,(fcb4)
	jp   z,wrfile

	cp   5
	ld   de,(fcb5)
	jp   z,wrfile

	jr   fail	; no FD? then fail

; T.RENAME(OLD, NEW)
; Rename file OLD as NEW.
; Fail if OLD does not exist or NEW does exist.
; Return 0=success or -1=failure.

t_rename:
	push ix
	ld   ix,0
	add  ix,sp

	ld   h,(ix+7)	; expand OLD name to TBUF
	ld   l,(ix+6)
	ld   de,(tbuf)
	call expandfn
	jp   c,fail

	ld   hl,(tbuf)	; expand NEW name to TBUF+16
	ld   de,16
	add  hl,de
	ex   de,hl
	ld   h,(ix+5)
	ld   l,(ix+4)
	call expandfn
	jp   c,fail

	ld   hl,(tbuf)	; does the NEW name exist?
	ld   de,16
	add  hl,de
	ex   de,hl
	ld   c,BDOPEN
	call BDOS
	inc  a
	jp   nz,fail

	ld   c,BDRENAME
	ld   de,(tbuf)
	call BDOS
	inc  a
	jp   z,fail

	ld   hl,0
	pop  ix
	ret

; T.REMOVE(NAME)

t_remove:
	push ix
	ld   ix,0
	add  ix,sp

	ld   h,(ix+5)	; expand NAME name to TBUF
	ld   l,(ix+4)
	ld   de,(tbuf)
	call expandfn
	jp   c,fail

	ld   de,(tbuf)
	ld   c,BDERASE
	call BDOS
	inc  a
	jp   z,fail

	ld   hl,0
	pop  ix
	ret

; Perform a BDOS call with
;     D = (ix+7)
; A = E = (ix+6)
;     C = (ix+8)
; Intended to be called by T.BDOS and T.BDOSHL

bdoscall:
	push ix
	ld   ix,0
	add  ix,sp

	ld   d,(ix+7)
	ld   e,(ix+6)
	ld   a,e

	ld   c,(ix+8)
	call BDOS

	pop  ix
	ret

; T.BDOS(C, ADE)
; Do a BDOS call with A=ADE & 255, DE=ADE, and C=C
; Return the value of A.
t_bdos:
	call bdoscall
	ld   l,a
	ld   h,0
	ret

; T.BDOSHL(C, ADE)
; Do a BDOS call with A=ADE & 255, DE=ADE, and C=C
; Return the value of HL.
t_bdoshl:
	call bdoscall	; This is NOT a tail call, because BDOSCALL
	ret		; expects the additional value on the stack!

	; MAIN PROGRAM ENTRY POINT

start:
	ld   hl,(BDOS+1)	; set up local stack
	ld   l,0
	ld   sp,hl

	ld   hl,-166	; allocate file descriptor #3
	add  hl,sp
	ld   sp,hl
	ld   (hl),0FFh
	ld   (fcb3),hl

	ld   hl,-166	; FD #4
	add  hl,sp
	ld   sp,hl
	ld   (hl),0FFh
	ld   (fcb4),hl

	ld   hl,-166	; FD #5
	add  hl,sp
	ld   sp,hl
	ld   (hl),0FFh
	ld   (fcb5),hl

	ld   hl,-130	; set up temporary buffer
	add  hl,sp
	ld   sp,hl
	ld   (tbuf),hl

rtlen	equ  $-rtlib

	; T3X code starts here ...

	end
