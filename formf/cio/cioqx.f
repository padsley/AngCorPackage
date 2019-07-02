C++
C TITLE: CIOQENTRY
C FACILITY:  CIO  - conversational I/O package.
C ABSTRACT: Store a qualifier name in the qualifier table and set
C	Q_NR to the entry's index.
C PROCEDURES CALLED:
C	CIOMSG
C AUTHOR: Peter A. Kroon, KVI, Groningen, NL.
C CREATION DATE: 21-sep-2006
C MODIFIED BY:
C--


	SUBROUTINE CIOQENTRY (QUALNAME)

	INCLUDE		'ciocom.inc'
	CHARACTER*(*)	QUALNAME
	INTEGER			K

	! Once only initialize

	CALL CIO1ST

	! If qualifier already defined before, redefine it,
	! otherwise add it. Abort if qualifier table full.

	DO K = 1, Q_NR
		IF (Q_NAMES(K).EQ.QUALNAME) GOTO 100	! found, redefine
	ENDDO

	! Qualifier not found, add it.
	K = Q_NR + 1
	IF (K.GT.QUAMAX) CALL CIOMSG (1, 'too many qualifiers defined')

	! Store qualifier string and pointer to associated variable
	! set global Q_NR to the qualifier index
  100	Q_NAMES (K) = QUALNAME
	Q_NR = K
	END
	
C++
C ENTRY: CIOQL, CIOQLX
C ABSTRACT: Define a qualifier of type /[NO]QUALIFIER
C FUNCTIONAL DESCRIPTION:
C CALLING SEQUENCE:
C	CALL CIOQL ( QUALNAME, IQUAL)
C	CALL CIOQLX (QUALNAME, IQUAL, IYES, INO)
C INPUTS:
C	QUALNAME	qualifier string
C
C	Following input arguments must either all be present or all be omitted.
C	If they are omitted they default to .true. and .false. respectively
C	and IQUAL is initialized to .true..
C	If they are present, IQUAL is initially not affected.
C
C	IYES	the value assigned to IQUAL if the positive form of the
C			qualifier is encountered in any input to CIO.
C	INO		the value assigned to IQUAL if the negative form of the
C			qualifier is encountered in any input to CIO.
C OUTPUTS:
C	IQUAL	the associated variable which receives the value of IYES or
C			INO on the occurance of respectively the positive or the
C			negative form of the qualifier.
C			IQUAL is initialized to .false. only if the IYES and INO
C			arguments are omitted.
C COMMENT:
C	IYES, INO and IQUAL may be of type LOGICAL, INTEGER or REAL,
C	however, they must be all of the same type.
C	However, this relies on the fact that these types have the same
C	size in memory, and that the compiler accepts that different
C	calls to the same function may have different argument types.
C--

	SUBROUTINE	CIOQL  (QUALNAME, LQUAL)

	INCLUDE		'ciocom.inc'

	CHARACTER*(*)	QUALNAME
	LOGICAL*4		LQUAL, LYES, LNO
	LOGICAL			XCALL

	XCALL = .false.
	GOTO 50

	ENTRY	CIOQLX (QUALNAME, LQUAL, LYES, LNO)

	XCALL = .true.
	GOTO 50

	! Store qualifier name or lookup if already defined
   50	CALL CIOQENTRY (QUALNAME)

  	Q_DATATYPE (Q_NR) = 1							! logical qualifier i.e. /QUAL or /NOQUAL form
	Q_XCALL    (Q_NR) = XCALL
	Q_PTR_DEST (Q_NR) = LOC (LQUAL)

	IF (.NOT.XCALL) THEN					! if only 2 arguments specified
	  Q_L_YESVALUE (Q_NR) = .true.			! store .true. as yes-value
	  Q_L_NOVALUE  (Q_NR) = .false.		! store .false. as no-value.
	  LQUAL = .false.						! and init assoc.var. to false.
	ELSE									! if all arguments specified
	  Q_L_YESVALUE (Q_NR) = LYES 				! store yes-value
	  Q_L_NOVALUE  (Q_NR) = LNO  				! store no-value.
	ENDIF

  	RETURN
	END
	
C++
C ENTRY: CIOQI, CIOQIX
C ABSTRACT: Define a qualifier of type /QUALIFIER[=integer value]
C FUNCTIONAL DESCRIPTION:
C CALLING SEQUENCE:
C	CALL CIOQI ( QUALNAME, IQUAL ,ILOLIM, IHILIM)
C	CALL CIOQIX (QUALNAME, IQUAL ,ILOLIM, IHILIM, IDEF)
C INPUTS:
C	QUALNAME	qualifier string
C	ILOLIM		inclusive lower limit for the value assigned to the qualifier.
C	IHILIM		inclusive upper limit for the value assigned to the qualifier.
C				NOTE: these limits are checked only if ILOLIM .ne. IHILIM.
C
C	IDEFAULT	optional argument specifying a default value to be assigned
C				to IQUAL if the qualifier is encountered without a value
C				assigned to it (i.e. the form /QUALIFIER).
C				If omitted, a value must be assigned to the qualifier
C				i.e. only the form /QUALIFIER=integer is allowed.
C OUTPUTS:
C	IQUAL		the associated variable which receives the value assigned
C				to the qualifier, or the value of IDEFAULT in case the form
C				/QUALIFIER is encountered.
C--

	SUBROUTINE	CIOQI  (QUALNAME, IQUAL, ILOLIM, IHILIM)

	INCLUDE		'ciocom.inc'
	CHARACTER*(*)	QUALNAME
	INTEGER			IQUAL, ILOLIM, IHILIM, IDEFAULT
	LOGICAL			XCALL

	XCALL = .false.
	GOTO 50

	ENTRY	CIOQIX (QUALNAME, IQUAL, ILOLIM, IHILIM, IDEFAULT)
	XCALL = .true.
	GOTO 50

	! Store qualifier name or lookup if already defined
   50 CALL CIOQENTRY (QUALNAME)

	! Store address of the associated variable and its type.
  	Q_DATATYPE (Q_NR) = 2							! /QUALIFIER=integer
	Q_XCALL    (Q_NR) = XCALL
	Q_PTR_DEST (Q_NR) = LOC (IQUAL)
	Q_I_LOLIM  (Q_NR) = ILOLIM
	Q_I_HILIM  (Q_NR) = IHILIM

	! If default is specified, store its value
	IF (XCALL) Q_I_DEFAULT (Q_NR) = IDEFAULT

  	RETURN
	END

C++
C ENTRY: CIOQF, CIOQFX
C ABSTRACT: Define a qualifier of type /QUALIFIER[=real value]
C FUNCTIONAL DESCRIPTION:
C CALLING SEQUENCE:
C	CALL CIOQF  (QUALNAME, AQUAL, ALOLIM, AHILIM)
C	CALL CIOQFX (QUALNAME, AQUAL, ALOLIM, AHILIM, ADEFAULT
C	arguments have a meaning similar to entry CIOQI.
C--

	SUBROUTINE	CIOQF (QUALNAME, AQUAL, ALOLIM, AHILIM)

	INCLUDE		'ciocom.inc'
	CHARACTER*(*)	QUALNAME
	REAL			AQUAL, ALOLIM, AHILIM, ADEFAULT
	LOGICAL			XCALL

	XCALL = .false.
	GOTO 50

	ENTRY	CIOQFX (QUALNAME, AQUAL, ALOLIM, AHILIM, ADEFAULT)
	XCALL = .true.
	GOTO 50

	! Store qualifier name or lookup if already defined
   50	CALL CIOQENTRY (QUALNAME)

  	Q_DATATYPE (Q_NR) = 3			! /QUALIFIER=real
	Q_XCALL    (Q_NR) = XCALL
	Q_PTR_DEST (Q_NR) = LOC (AQUAL)
	Q_F_LOLIM  (Q_NR) = ALOLIM
	Q_F_HILIM  (Q_NR) = AHILIM

	! If default is specified, store its value
	IF (XCALL) Q_F_DEFAULT (Q_NR) = ADEFAULT

  	RETURN
	END
	
C++
C ENTRY: CIOQS
C ABSTRACT: Define a qualifier of type /QUALIFIER[=string]
C FUNCTIONAL DESCRIPTION:
C CALLING SEQUENCE:
C	CALL CIOQS  (QUALNAME, LENQUAL, SQUAL)
C	CALL CIOQSX (QUALNAME, LENQUAL, SQUAL, IFLAG)
C INPUTS:
C	QUALNAME	qualifier string
C OUTPUTS:
C	LENSQUAL	integer variable which, on occurance of the qualifier,
C				receives the length of the string assigned to SQUAL.
C	SQUAL		string variable which, on occurance of the qualifier,
C				receives the string assigned to SQUAL.
C	IFLAG		optional argument receiving an indication of the format
C				in which the qualifier was entered:
C				-1 if the qualifier was not encountered,
C				 0 if the /QUALIFIER form was encountered,
C				+1 if the /QUALIFIER=string form was encountered.
C				If omitted, a string must be assigned to the qualifier
C				i.e. only the form /QUALIFIER=string is allowed.
C				Note that if IFLAG = -1 or 0 the contents of SQUAL and
C				LENSQUAL are unaffected.
C--

	SUBROUTINE	CIOQS  (QUALNAME, LENSQUAL, SQUAL)

	INCLUDE		'ciocom.inc'
	CHARACTER*(*)	QUALNAME, SQUAL
	INTEGER			LENSQUAL, IFLAG
	LOGICAL			XCALL

	XCALL = .false.
	GOTO 50

	ENTRY	CIOQSX (QUALNAME, LENSQUAL, SQUAL, IFLAG)
	XCALL = .true.
	GOTO 50

	! Store qualifier name or lookup if already defined
   50	CALL CIOQENTRY (QUALNAME)

  	Q_DATATYPE (Q_NR) = 4			! /QUALIFIER=string
	Q_XCALL    (Q_NR) = XCALL
      Q_PTR_DEST (Q_NR) = LOC (SQUAL)
      Q_PTR_LEN  (Q_NR) = LOC (LENSQUAL)

	! If all arguments given set format flag to 'qualifier not specified'.
	! and store pointer to format indicator.

	IF (XCALL) THEN
	  IFLAG = -1
	  Q_PTR_FLAG (Q_NR) = LOC(IFLAG)
	ENDIF

  	RETURN
	END
