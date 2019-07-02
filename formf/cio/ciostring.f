C++
C TITLE:  CIOSTRING
C FACILITY:  CIO  - conversational I/O package.
C ABSTRACT:
C	F77 implementation of some string functions that were
C	available as Run-Time-Libaray functions on OpenVMS.
C PROCEDURES CALLED:
C	none
C AUTHOR: Peter A. Kroon, KVI, Groningen, NL.
C CREATION DATE:  27-sep-2006
C MODIFIED BY:
C
C CALLING SEQUENCE:
C	See the corresponding descriptions of:
C	LIB$SCANC, LIB$SPANC, STR$UPCASE
C	in the OpenVMS documentation set.
C--

	SUBROUTINE STR__UPCASE (DESTINATION, SOURCE)

	IMPLICIT NONE
	CHARACTER*(*) DESTINATION, SOURCE

	INTEGER LENSRC, LENDST, K, IC

	LENSRC = LEN (SOURCE)
	LENDST = LEN (DESTINATION)

      DO K = 1, MIN (LENSRC, LENDST)
        IC = ICHAR (SOURCE (K:K))
        IF (IC.GE.ICHAR('a') .and. IC.LE.ICHAR('z')) THEN
                IC = IC - ICHAR('a') + ICHAR('A')
        ENDIF
		DESTINATION(K:K) = CHAR(IC)
	END DO

	IF (LENDST.GT.LENSRC)	DESTINATION (LENSRC+1:) = ''

	END

C ---------------------------------------------------------------------

	FUNCTION LIB__SCANC (SOURCE, TABLE, MASK)

	IMPLICIT		NONE
      INTEGER           LIB__SCANC
	CHARACTER(*)	SOURCE
      BYTE              TABLE(0:255)
      BYTE              MASK

      INTEGER           K, IC

      LIB__SCANC = 0    ! assume not found
	IF (LEN(SOURCE).EQ.0) RETURN
	DO K = 1, LEN(SOURCE)
		IC = ICHAR (SOURCE(K:K))
        IF (IAND(MASK,TABLE(ICHAR(SOURCE(K:K)))) .NE. 0) THEN
                LIB__SCANC = K
			RETURN
		ENDIF
	END DO
	RETURN
	END

C ---------------------------------------------------------------------

	FUNCTION LIB__SPANC (SOURCE, TABLE, MASK)

	IMPLICIT		NONE
      INTEGER           LIB__SPANC
	CHARACTER(*)	SOURCE
      BYTE              TABLE(0:255)
      BYTE              MASK

      INTEGER           K, IC

	LIB__SPANC = 0
	IF (LEN(SOURCE).EQ.0) RETURN
	DO K = 1, LEN(SOURCE)
		IC = ICHAR (SOURCE(K:K))
        IF (IAND(MASK,TABLE(ICHAR(SOURCE(K:K)))) .EQ. 0) THEN
			LIB__SPANC = K
			RETURN
		ENDIF
	END DO
	RETURN
	END

C ---------------------------------------------------------------------

