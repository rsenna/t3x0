! T3X/0 module: character functions
! Nils M Holm, 1999,2000,2002,2003,2019,2022
! In the public domain / 0BSD License

module char;

 var	Cmap;		! pointer to character property map

 const	C_ALPHA = 0x0001,
	C_UPPER = 0x0002,
	C_DIGIT = 0x0004,
	C_SPACE = 0x0008,
	C_CNTRL = 0x0010;

 const	A = C_ALPHA,
	U = C_UPPER,
	D = C_DIGIT,
	S = C_SPACE,
	C = C_CNTRL;

 public ascii(ch) return (ch & ~127)-> 0: -1;

 public alpha(ch) do
	if (ch & ~127) return 0;
	return (Cmap::ch & C_ALPHA)-> -1: 0;
 end

 public upper(ch) do
	if (ch & ~127) return 0;
	return (Cmap::ch & C_UPPER)-> -1: 0;
 end

 public lower(ch) do
	if (ch & ~127) return 0;
	return Cmap::ch = C_ALPHA;
 end

 public digit(ch) do
	if (ch & ~127) return 0;
	return (Cmap::ch & C_DIGIT)-> -1: 0;
 end

 public space(ch) do
	if (ch & ~127) return 0;
	return (Cmap::ch & C_SPACE)-> -1: 0;
 end

 public cntrl(ch) do
	if (ch & ~127) return 0;
	return (Cmap::ch & C_CNTRL)-> -1: 0;
 end

 public lcase(ch) do
	if (ch & ~127) return ch;
	if (Cmap::ch & C_UPPER) return ch+('a'-'A');
	return ch;
 end

 public ucase(ch) do
	if (ch & ~127) return ch;
	if (\(Cmap::ch & C_ALPHA) \/ Cmap::ch & C_UPPER) return ch;
	return ch+('A'-'a');
 end

 public value(ch) do
	if (ch & ~127) return %1;
	if (Cmap::ch & C_DIGIT) return ch-'0';
	return %1;
 end

 do
	Cmap := packed [
	C,	C,	C,	C,	C,	C,	C,	C,
	C,	C|S,	C|S,	C|S,	C|S,	C|S,	C,	C,
	C,	C,	C,	C,	C,	C,	C,	C,
	C,	C,	C,	C,	C,	C,	C,	C,
	S,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	D,	D,	D,	D,	D,	D,	D,	D,
	D,	D,	0,	0,	0,	0,	0,	0,
	0,	A|U,	A|U,	A|U,	A|U,	A|U,	A|U,	A|U,
	A|U,	A|U,	A|U,	A|U,	A|U,	A|U,	A|U,	A|U,
	A|U,	A|U,	A|U,	A|U,	A|U,	A|U,	A|U,	A|U,
	A|U,	A|U,	A|U,	0,	0,	0,	0,	0,
	0,	A,	A,	A,	A,	A,	A,	A,
	A,	A,	A,	A,	A,	A,	A,	A,
	A,	A,	A,	A,	A,	A,	A,	A,
	A,	A,	A,	0,	0,	0,	0,	C ];
 end

end
