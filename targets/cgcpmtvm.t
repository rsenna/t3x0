! T3X/0 cross back-end CP/M on Z80 -> TCVM
! Nils M Holm, 2022, 2023
! In the Public Domain / 0BSD License

module cg;

 public const	BPW = 2;

 public const	GPOOL_SIZE = 7;

 public const	BUFLEN = 128;

 public const	SYMTBL_SIZE = 400;
 public const	LABEL_SIZE = 1400;
 public const	NLIST_SIZE = 3000;

 public const	LOCAL_LIMIT = 32760;

 public binary() return %1;

 public suffix() return ".tc";

 public modpath() return [ "", 0 ];

 public header() do
	emit('T');
	emit('3');
	emit('X');
	emit('0');
	emitw(0);
	return 0;
 end

 public codefrags() return [
	[ CG_NULL,	""	],
	[ CG_PUSH,	"01"	],
	[ CG_CLEAR,	"02"	],
	[ CG_DROP,	"03"	],
	[ CG_LDVAL,	"04,w"	],
	[ CG_LDADDR,	"05,w"	],
	[ CG_LDLREF,	"06,w"	],
	[ CG_LDGLOB,	"07,w"	],
	[ CG_LDLOCL,	"08,w"	],
	[ CG_STGLOB,	"09,w"	],
	[ CG_STLOCL,	"0a,w"	],
	[ CG_STINDR,	"0b"	],
	[ CG_STINDB,	"0c"	],
	[ CG_INCGLOB,	"0d,w"	],
	[ CG_INCLOCL,	"0e,w"	],
	[ CG_INCR,	"0f,w"	],
	[ CG_STACK,	"10,w"	],
	[ CG_UNSTACK,	"11,w"	],
	[ CG_LOCLVEC,	"12"	],
	[ CG_GLOBVEC,	"13,w"	],
	[ CG_INDEX,	"14"	],
	[ CG_DEREF,	"15"	],
	[ CG_INDXB,	"16"	],
	[ CG_DREFB,	"17"	],
	[ CG_CALL,	"18,w"	],
	[ CG_CALR,	"19"	],
	[ CG_JUMP,	"1a,w"	],
	[ CG_RJUMP,	"1b,r"	],
	[ CG_JMPFALSE,	"1c,w"	],
	[ CG_JMPTRUE,	"1d,w"	],
	[ CG_FOR,	"1e,w"	],
	[ CG_FORDOWN,	"1f,w"	],
	[ CG_MKFRAME,	"20"	],
	[ CG_DELFRAME,	"21"	],
	[ CG_RET,	"22"	],
	[ CG_HALT,	"23,w"	],
	[ CG_NEG,	"24"	],
	[ CG_INV,	"25"	],
	[ CG_LOGNOT,	"26"	],
	[ CG_ADD,	"27"	],
	[ CG_SUB,	"28"	],
	[ CG_MUL,	"29"	],
	[ CG_DIV,	"2a"	],
	[ CG_MOD,	"2b"	],
	[ CG_AND,	"2c"	],
	[ CG_OR,	"2d"	],
	[ CG_XOR,	"2e"	],
	[ CG_SHL,	"2f"	],
	[ CG_SHR,	"30"	],
	[ CG_EQ,	"31"	],
	[ CG_NE,	"32"	],
	[ CG_LT,	"33"	],
	[ CG_GT,	"34"	],
	[ CG_LE,	"35"	],
	[ CG_GE,	"36"	],
	[ CG_UMUL,	"37"	],
	[ CG_UDIV,	"38"	],
	[ CG_ULT,	"39"	],
	[ CG_UGT,	"3a"	],
	[ CG_ULE,	"3b"	],
	[ CG_UGE,	"3c"	],
	[ CG_JMPEQ,	""	],
	[ CG_JMPNE,	""	],
	[ CG_JMPLT,	""	],
	[ CG_JMPGT,	""	],
	[ CG_JMPLE,	""	],
	[ CG_JMPGE,	""	],
	[ CG_JMPULT,	""	],
	[ CG_JMPUGT,	""	],
	[ CG_JMPULE,	""	],
	[ CG_JMPUGE,	""	],
	[ CG_SKIP,	"47,w"	],
	[ %1,		""	] ];

 public optimizations() return [
	[ CG_LOGNOT,	0,	CG_JMPFALSE,	CG_JMPTRUE	],
	[ CG_DROP,	0,	CG_PUSH,	CG_NULL		],
	[ %1,		%1,	%1,		%1		],
	[ CG_LDVAL,	0,	CG_INDEX,	CG_DROP		],
	[ CG_LDVAL,	0,	CG_INDXB,	CG_DROP		],
	[ CG_LDVAL,	0,	CG_ADD,		CG_DROP		],
	%1 ];

 public library() return [ 0 ];

end
