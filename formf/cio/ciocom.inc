C Miscellaneous parameters

	IMPLICIT	NONE

	INTEGER		INBSIZ, PRBLEN, PRBSIZ, LUNIDF, LSTKSZ, LUNODF

      PARAMETER (INBSIZ = 100)          ! Internal input buffer size.
      PARAMETER (PRBLEN = 6)            ! Nr of strings which can be stored in PRBUF.
      PARAMETER (PRBSIZ = 80)           ! Max. size of such strings.
      PARAMETER (LUNIDF = 5)            ! Default input unit nr.
      PARAMETER (LUNODF = 6)            ! Default output unit nr.
      PARAMETER (LSTKSZ = 10)           ! Alternate file lun stack size.

C CIO001 - Common with miscellaneous variables

      INTEGER           KSPLOW          ! argument number for lowlim
      INTEGER           KSPHIG          ! argument number for hilim
      INTEGER           KSPDEF          ! argument number for default
      INTEGER           KSPCNV          ! 0/1 for don't/do upcase input

	INTEGER		KSPCTL(4)
	INTEGER		KSPBCK, KSPFWD, KSPUP, KSPDOL

	EQUIVALENCE	(KSPBCK, KSPCTL(1))
	EQUIVALENCE	(KSPFWD, KSPCTL(2))
	EQUIVALENCE	(KSPUP,  KSPCTL(3))
	EQUIVALENCE	(KSPDOL, KSPCTL(4))

      INTEGER           LUNI            ! current LUN for input
      INTEGER           LUNIS           ! copy of LUN, negative if obtained with CIOLUNGET
      INTEGER           LUNO            ! current LUN for output

      INTEGER           MODI            ! 1 if LUNI assigned to a terminal device, else 0.
      INTEGER           MODO            ! 1 if LUNO assigned to a terminal device, else 0.

      INTEGER           LUNSTK(LSTKSZ)  ! LUN stack used to save LUNIS for nested alternate input streams.
      INTEGER           LUNSTP          ! LUN stackpointer

      INTEGER           INPTR           ! current position in input buffer.
      INTEGER           INBUFL          ! # of characters in input buffer.
      INTEGER           INBUFC          ! same, excluding trailing comments and blanks.
      INTEGER           IPBPTR          ! position of last character in the prompt buffer.
      LOGICAL           LCONTIN         ! Continuation flag; true of last non-blank char in inbuf is the continuation character.
      INTEGER           LUNPRF          ! LUN for a temp file storing prompts if the prompt buffer overflows.
      INTEGER           ITPTR1          ! current item start position in input buffer
      INTEGER           ITPTR2          ! current item end position in input buffer

	COMMON /CIO001/
	1  KSPLOW,KSPHIG,KSPDEF,KSPCNV
	1 ,KSPBCK,KSPFWD,KSPUP,KSPDOL
	1 ,MODI,MODO,LUNI,LUNIS,LUNO,LUNSTP,LUNSTK
	1 ,INPTR,INBUFL,INBUFC,IPBPTR,LCONTIN,LUNPRF
	1 ,ITPTR1,ITPTR2

C CIO002 - common for character variables.

      CHARACTER INBUF*(INBSIZ)          ! Internal input buffer
      CHARACTER CWORK*(INBSIZ)          ! Work string.

      CHARACTER PRBUF(PRBLEN)*(PRBSIZ)  ! Buffer for long prompts
      CHARACTER CSPRDX*1                ! Number radix from specification string (I, O, Z)

	COMMON /CIO002/ INBUF,CWORK,PRBUF,CSPRDX

C CIO003 - common with table defining the various special charactertypes
C for SPANC,SCANC.
C This common is initialized by the INCLUDE file CIO003INI.INC

C Parameters of the form S_XXXX are strings  containing all characters
C of a particular type.

C C_SEPA removed because the construction used for S_SEPA doesn't work and none of
C the S_xxxx parameters seems to be used.
C G77 doesn't allow intrinsics in a PARAMETER statement.
C pak 18-09-2006

      CHARACTER*(*) S_COMM, S_SEPA, S_DELM, S_FLOW, S_CHAR, S_CONT
      CHARACTER*(*) S_ALTF, S_QUAL

      PARAMETER (S_COMM  = '!')         ! Comment character(s)
      PARAMETER (S_SEPA = ' ,'//CHAR(9))! Item separators (incl TAB)
      PARAMETER (S_DELM = '''"')        ! String delimiters
      PARAMETER (S_FLOW = '<>^$')       ! Flow control characters
      PARAMETER (S_CHAR = '=*?<>^$')    ! All control char's
      PARAMETER (S_CONT = '-')          ! Continuation character
      PARAMETER (S_ALTF = '@')          ! Alternate file spec character
      PARAMETER (S_QUAL = '/')          ! Qualifier prefix character.

C Parameters of the form C_XXXX are constants representing a code for
C each type of characters, to be used in SPANC and SCANC instructions.

	INTEGER C_COMM, C_SEPA, C_DELM, C_FLOW, C_CTRL, C_QUAL

	PARAMETER	(C_COMM	= 1 )		! Comment character(s)
	PARAMETER	(C_SEPA	= 2 )		! Item separator(s)
	PARAMETER	(C_DELM	= 4 )		! String delimiter(s)
	PARAMETER	(C_FLOW	= 8 )		! Flow control character(s)
	PARAMETER	(C_CTRL	= 16)		! Other control character(s)
	PARAMETER	(C_QUAL	= 32)		! Qualifier prefix character(s)

      BYTE      C_TABLE(0:255)          ! Specification table.
	COMMON	/CIO003/ C_TABLE

C Definitions concerning the qualifier mechanism.

	INTEGER QUASIZ, QUAMAX

	PARAMETER (QUASIZ	= 12)	! Max length of a qualifier
	PARAMETER (QUAMAX	= 32)	! Max nr of qualifiers which can
					! be defined at the same time.

C CIO004 - common with variables concerning qualifiers.
C CIO005 - common with character variables concerning qualifiers.

	INTEGER Q_NR			! Total nr of qualifiers defined.

	CHARACTER*(QUASIZ) Q_NAMES (QUAMAX)	! Qualifier names


C Data type of the variable associated with the qualifier.
C 1 = logical, 2 = integer, 3 = real, 4 = string.

	INTEGER*1	Q_DATATYPE (QUAMAX)

C Flag that stores whether the short or long calling format was used
C for the functions CIOQt or CIOQtX.
C True = long format.
C In earlier versions this was encoded as a negative Q_DATATYPE.

	LOGICAL*1	Q_XCALL (QUAMAX)

C Pointer to variable associated with the qualifier.
C For strings Q_PTR_DEST is the address of the string proper,

	INTEGER		Q_PTR_DEST (QUAMAX)

C For strings there are two additional pointers:
C Q_PTR_LEN points to the variable receiveing the actual string length;
C Q_PTR_FLAG points to variable receiveing the qualifier format flag.

	INTEGER		Q_PTR_LEN (QUAMAX)
	INTEGER		Q_PTR_FLAG (QUAMAX)

C Default values for logical, real and integer qualifiers

	LOGICAL		Q_L_DEFAULT (QUAMAX)
	REAL		Q_F_DEFAULT (QUAMAX)
	INTEGER		Q_I_DEFAULT (QUAMAX)

C for logical: yes value, no value
C for integer: low limit, high limit
C for real:	 low limit, high limit

	LOGICAL*4	Q_L_YESVALUE (QUAMAX)
	LOGICAL*4	Q_L_NOVALUE (QUAMAX)

	INTEGER		Q_I_LOLIM (QUAMAX)
      INTEGER		Q_I_HILIM (QUAMAX)

      REAL		Q_F_LOLIM (QUAMAX)
      REAL		Q_F_HILIM (QUAMAX)

	COMMON /CIO004/ Q_NR, Q_DATATYPE,
     + Q_PTR_DEST, Q_PTR_LEN,
     + Q_L_DEFAULT, Q_F_DEFAULT, Q_I_DEFAULT,
     + Q_L_YESVALUE, Q_L_NOVALUE,
     + Q_I_LOLIM, Q_I_HILIM,
     + Q_F_LOLIM, Q_F_HILIM

	COMMON /CIO005/ Q_NAMES

C Common with Logical Unit Number pool.
C The choice for the first and last LUNs was made as follows:
C - the VMS Fortran function LIB$GET_LUN that was used in the original
C   version of CIOLIB used the range 100-119 for VAX and 	100-299 on
C   Alpha. Since most programs using CIOLIB were originally written on
C   a VAX, 20 LUN's should satisfy most needs.
C - the highest LUN in the Linux g77 implementation is 99.
C - by starting as high as possible in this available LUN range, we
C   hopefully avoid conflict with LUN usage in existing programs.

	INTEGER LUNPOOLMIN, LUNPOOLMAX
	PARAMETER (LUNPOOLMIN = 80)
	PARAMETER (LUNPOOLMAX = 99)
	! Element K in LUNPOOL is false/true for a free/inuse LUN.
	LOGICAL*1 LUNPOOL(LUNPOOLMIN:LUNPOOLMAX)

	COMMON /CIO006/ LUNPOOL
