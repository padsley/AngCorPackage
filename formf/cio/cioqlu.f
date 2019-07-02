	SUBROUTINE CIOQLU (I1, I2, *, *, *)
C++
C TITLE:  CIOQLU
C FACILITY:  CIO  - conversational I/O package.
C ABSTRACT: Test if item is qualifier and if so process it.
C AUTHOR: Peter A. Kroon, KVI, Groningen, NL.
C PROCEDURES CALLED:
C	CIOLUP, CIOERL, CIO_MOVL, STR__UPCASE, STR__COPY_DX
C CREATION DATE:  13-may-1981
C MODIFIED BY:
C FUNCTIONAL DESCRIPTION:
C	Check if item INBUF(I1:I2) is a qualifier, i.e. a slash followed by
C	at least one character. If not take alternate return 1.
C	If a qualifier, search its definition in the qualifier list.
C	If no qualifiers allowed (defined) complain, and also take return 2.
C	If not found or ambiguous, complain, and take alternate return 3.
C	If qualifier is legal, update its associated variable(s). Then return.
C CALLING SEQUENCE:
C	CALL CIOQLU (I1, I2, *, *, *)
C INPUT PARAMETERS:
C	I1, I2	pointers to begin and end of item in INBUF to be examined.
C ALTERNATE RETURNS:
C	normal	item is a qualifier and has successfully been processed.
C	1	item is not a qualifier,
C	2	item is a qualifier, but there are no qualifiers allowed,
C	3	item is a qualifier, but is unknown or ambiguous, a value
C		is required, missing or not allowed or a negative form is
C		given but not allowed.
C--

	INCLUDE		'ciocom.inc'
	LOGICAL*1	LNEGATE, LVALUE
	CHARACTER*(QUASIZ+2)	CTEMP	! allow for 'NO' prefix
	INTEGER		ILOLIM, IHILIM, ITEMP
	REAL		ALOLIM, AHILIM, ATEMP
	INTEGER		I1, I2, J1, J2, K, K1, K2, M
	LOGICAL		XCALL

C Check whether the item in INBUF(I1:I2) is a qualifier.
C If not take alternate return 1.
C If ok, copy item pointers to local variables, since we may modify them.

	IF (I2.LE.I1 .OR. INBUF(I1:I1).NE.'/') RETURN 1
	J1 = I1
	J2 = I2

C It is a qualifier. See if any qualifiers defined.

	IF (Q_NR.EQ.0) THEN
	  CALL CIOERL ('no qualifiers allowed')
	  RETURN 2
	ENDIF

C Check whether the /QUALIFIER or /QUALIFIER=value form.

	K = INDEX (INBUF(J1:J2), '=')	! locate the equal sign
	IF (K.EQ.0 .OR. K.EQ.J2-J1+1) THEN
		! no equal sign or nothing behind equal sign
	  LVALUE = .FALSE.
	ELSE				! equal sign present
	  LVALUE = .TRUE.
	  K1 = J1 + K			! K1 and K2 mark the value field
	  K2 = J2
	  J2 = K1 - 2			! Adjust the qualifier end pointer
	ENDIF

C Distinquish between /QUALIFIER AND /NOQUALIFIER form

	CALL STR__UPCASE (CTEMP, INBUF(J1:J1+2))
	IF (CTEMP(1:3) .EQ. '/NO') THEN
	  LNEGATE = .TRUE.
	  CTEMP = INBUF(J1+3:J2)	! extract part after /NO.
	ELSE
	  LNEGATE = .FALSE.
	  CTEMP = INBUF(J1+1:J2)	! extract part after /.
	ENDIF

C Lookup qualifier (with possible NO removed) in qualifier definition list

	CALL CIOLUP (CTEMP, Q_NAMES, Q_NR, M)

C Check result

	IF (M) 910, 920, 100	! unknown / ambiguous / found

C Qualifier is known. Get flag that indicates whether original call
C had the short or extended argument list.
C Then dispatch according to its datatype

  100	XCALL = Q_XCALL (M)
	GOTO (110, 120, 130, 140), Q_DATATYPE(M)

C Logical qualifier ( /QUAL or /NOQUAL).

  110	CONTINUE
	IF (LVALUE) GOTO 930				! A value is never allowed here
	IF (LNEGATE) THEN				! if /NO... form
	  CALL CIO_COPY_L (Q_L_NOVALUE(M),  %VAL(Q_PTR_DEST(M)))	! store no-value.
	ELSE						! if /... form
	  CALL CIO_COPY_L (Q_L_YESVALUE(M), %VAL(Q_PTR_DEST(M)))	! store yes-value.
	ENDIF
	RETURN

C Qualifier with an integer value assignement.

  120	CONTINUE
	IF (LNEGATE) GOTO 950							! Don't allow /NO form
	IF (.NOT.LVALUE) THEN
		! no value given, use default, but complain if it was
		! not specified.
		IF (.NOT.XCALL) GOTO 940	! error, value required
		CALL CIO_COPY_I (Q_I_DEFAULT(M), %VAL(Q_PTR_DEST(M)))
	ELSE
		! Value given, decode it, check against limits and store
		! in associated variable.
		READ (INBUF(K1:K2), 9120, ERR=960) ITEMP
C9120		FORMAT (I<K2-K1+1>)
 9120		FORMAT (I20)
		ILOLIM = Q_I_LOLIM(M)
		IHILIM = Q_I_HILIM(M)
		IF (ILOLIM.NE.IHILIM .and.
     +		(ITEMP.LT.ILOLIM .OR.ITEMP.GT.IHILIM)) GOTO 970
		CALL CIO_COPY_I (ITEMP, %VAL(Q_PTR_DEST(M)))
	ENDIF
	RETURN

C Qualifier with a real value assignement.

  130	CONTINUE
	IF (LNEGATE) GOTO 950							! Don't allow /NO form
	IF (.NOT.LVALUE) THEN
		! no value given, use default, but complain if it was
		! not specified.
		IF (.NOT.XCALL) GOTO 940	! error, value required
		CALL CIO_COPY_F (Q_F_DEFAULT(M), %VAL(Q_PTR_DEST(M)))	! use default
	ELSE
		! Value given, decode it, check against limits and store
		! in associated variable.
		READ (INBUF (K1:K2), 9130, ERR=960) ATEMP
C9130		FORMAT (F<K2-K1+1>.0)
 9130		FORMAT (F20.0)
		ALOLIM = Q_F_LOLIM(M)
		AHILIM = Q_F_HILIM(M)
		IF (ALOLIM.NE.AHILIM .and.
     +    	(ATEMP.LT.ALOLIM .OR.ATEMP.GT.AHILIM)) GOTO 970
		CALL CIO_COPY_F (ATEMP, %VAL(Q_PTR_DEST(M)))		! If ok, store value.
	ENDIF
	RETURN

C Qualifier with a string assignement.

  140	CONTINUE
	IF (LNEGATE) GOTO 950								! Don't allow /NO form
	IF (.NOT.LVALUE) THEN
		! No string value given. Error if value IS required.
		IF (.NOT.XCALL) GOTO 940
		! Else set flag to qualifier-without-value
	    CALL CIO_COPY_I (0, %VAL(Q_PTR_FLAG(M)))
	ELSE
		! String given. Copy it to associated string variable,
		! and copy length to assocated length variable.
		CALL CIO_COPY_S (INBUF(K1:K2), K2-K1+1, %VAL(Q_PTR_DEST(M)))
	  	CALL CIO_COPY_I (K2-K1+1, %VAL(Q_PTR_LEN(M)))
		! and if indicator flag was given
		! then set it to indicate! qualifier-with-value.
	  	IF (XCALL) CALL CIO_COPY_I (1, %VAL(Q_PTR_FLAG(M)))
	ENDIF
	RETURN

C Various error messages.

  910	CALL CIOERL ('unknown qualifier')
	GOTO 990
  920	CALL CIOERL ('ambiguous qualifier')
	GOTO 990
  930	CALL CIOERL ('no value allowed on qualifier')
	GOTO 990
  940	CALL CIOERL ('value required on qualifier')
	GOTO 990
  950	CALL CIOERL ('qualifier has no negative form')
	GOTO 990
  960	CALL CIOERL ('bad syntax for qualifier value')
	GOTO 990
  970	CALL CIOERL ('illegal qualifier value')
	GOTO 990

  990	WRITE (LUNO,9990)
 9990	FORMAT(T8,'please re-enter or type <return> to proceed'/' _',$)
	RETURN 3

	END
