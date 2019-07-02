	SUBROUTINE CIOQCO
C++
C TITLE:  CIOQCO
C FACILITY:  CIO  - conversational I/O package.
C ABSTRACT: Complete qualifier scan.
C PROCEDURES CALLED:
C	CIO1ST, CIOGBF, CIOLIT, CIOQLU, CIOQSH
C AUTHOR: Peter A. Kroon, KVI, Groningen, NL.
C CREATION DATE: 13-may-1981
C MODIFIED BY:
C FUNCTIONAL DESCRIPTION:
C	Extract remaining qualifiers from the
C	input buffer until a non-qualifier is encountered.
C	If an undefined or ambiguous qualifier is found, display a message,
C	and proceed as usual (abort if batch, continue if interactive).
C	On errors try to recover.
C	Note that HELP and ? are intercepted in a slightly different
C	from the normal way.
C CALLING SEQUENCE:
C	CALL CIOQCO
C--

	INCLUDE		'ciocom.inc'
	LOGICAL		LQUOTE, LQUALERR
	CHARACTER*4	CTEMP
	INTEGER		INPTR_TMP, I1, I2

	CALL CIO1ST			! once only initialize.

C Extract items until non-qualifier or end-of-line found.

	LQUALERR = .FALSE.
  250	INPTR_TMP = INPTR			! Use temp INBUF pointer
	CALL CIOLIT (INPTR_TMP, I1, I2, LQUOTE, *299)	! Locate next item.
	ITPTR1 = I1				! Store pointers for CIOERL
	ITPTR2 = I2
	CALL CIOQLU (I1, I2, *270, *255, *260)	! See if item qualifier,
  255	LQUALERR = .FALSE.			! if yes, lookup and update
						! associated variable,
						! complain and return if
						! ambiguous or unknown qual.
	INPTR = INPTR_TMP			! On succes update pointer and
	GOTO 250				! loop to try next item.

C Item is a qualifier but some error occurred (unknown, ambiguous,
C value required, missing, not allowed or outside limits).

  260	LQUALERR = .TRUE.			! remember
	CALL CIOGBF				! get input, don't prompt
	GOTO 250				! get next item

C Item is not a qualifier. Only if we just had a qualifier error
C see if the item is a single question mark and if so show the legal
C qualifiers, else return and leave the item for later interpretation.

  270	IF (LQUALERR) THEN
	  IF (INBUF(I1:I1).EQ.'?') THEN
	    CALL CIOQSH
	    GOTO 260
	  ELSE
	    CALL STR__UPCASE (CTEMP, INBUF(I1:I1+3))
	    IF (CTEMP.EQ.'HELP') THEN
	      INPTR = INPTR_TMP	! move pointer beyond 'HELP'
	      CALL CIOHLP
	      GOTO 260
	    ENDIF
	  ENDIF
	ENDIF

  299	RETURN
	END
