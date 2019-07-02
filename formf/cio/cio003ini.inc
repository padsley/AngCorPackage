C CIO003INI.INC - Initialization of common CIO003 containing the
C charactertype definition table.

	DATA	C_TABLE
	1/	9*0					! 00-08
	1,	C_SEPA					!    09 tab
	1,	22*0					! 10-31
	1,	C_SEPA					!    32	space
	1,	C_COMM					!    33 excl.point
	1,	C_DELM					!    34 double quote
	1,	1*0					!    35
	1,	C_CTRL					!    36 dollarsign
	1,	2*0					! 37-38
	1,	C_DELM					!    39 single quote
	1,	2*0					! 40-41
	1,	C_CTRL					!    42	asterisk
	1,	1*0					!    43
	1,	C_SEPA					!    44 comma
	1,	2*0					! 45-46
	1,	C_QUAL					!    47 slash
	1,	11*0					! 48-58
	1,	1*0					!    59 semicolon
	1,	C_FLOW					!    60 backarrow
	1,	C_CTRL					!    61 equal sign
	1,	C_FLOW					!    62 forw.arrow
	1,	C_CTRL					!    63 questionmark
	1,	30*0					! 64-93
	1,	C_FLOW					!    94 uparrow
	1,	161*0					! 95-255
	1/
