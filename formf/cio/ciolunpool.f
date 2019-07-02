C++
C TITLE:  CIOLUNGET
C FACILITY:  CIO  - conversational I/O package.
C ABSTRACT: Allocate a LUN from the LUN pool
C ENVIRONMENT:  FORTRAN-77.
C PROCEDURES CALLED:
C	none
C AUTHOR: Peter A. Kroon, KVI, Groningen, NL.
C CREATION DATE:  26-sep-2006
C MODIFIED BY:
C FUNCTIONAL DESCRIPTION:
C	Returns a free LUN from the LUN pool an marks that LUN as used.
C CALLING SEQUENCE:
C	CALL CIOLUNGET (LUN, STATUS)
C INPUT PARAMETERS:
C	none
C OUTPUT PARAMETERS:
C	LUN		integer receiving the allocated LUN.
C	STATUS	integer receiving status:
C			1 = LUN succesfully allocated
C			0 = no more LUNs available in pool
C--

	SUBROUTINE CIOLUNGET (LUN, STATUS)

	INCLUDE 'ciocom.inc'
	INTEGER LUN, STATUS
	INTEGER K

	DO K = LUNPOOLMIN, LUNPOOLMAX
		IF (.not.LUNPOOL(K)) THEN
			LUN = K
			STATUS = 1
			RETURN
		ENDIF
	END DO
	STATUS = 0	! no free lun found
	RETURN
	END

C++
C TITLE:  CIOLUNFREE
C FACILITY:  CIO  - conversational I/O package.
C ABSTRACT: Return a LUN to the LUN pool
C ENVIRONMENT:  FORTRAN-77.
C PROCEDURES CALLED:
C	none
C AUTHOR: Peter A. Kroon, KVI, Groningen, NL.
C CREATION DATE:  26-sep-2006
C MODIFIED BY:
C FUNCTIONAL DESCRIPTION:
C	Returns a free LUN from the LUN pool an marks that LUN as used.
C CALLING SEQUENCE:
C	CALL CIOLUNFREE (LUN, STATUS)
C INPUT PARAMETERS:
C	LUN		integer containing the LUN to be freed.
C OUTPUT PARAMETERS:
C	STATUS	integer receiving status:
C			1 = LUN succesfully freed.
C			0 = LUN was already free, or LUN out of allowable range.
C--

	SUBROUTINE CIOLUNFREE (LUN, STATUS)

	INCLUDE 'ciocom.inc'
	INTEGER LUN, STATUS
	INTEGER K

	STATUS = 0	! assume failure
	IF (LUN.lt.LUNPOOLMIN .OR. LUN.GT.LUNPOOLMAX) RETURN
	IF (.NOT.LUNPOOL(LUN)) RETURN ! was already free
	LUNPOOL(LUN) = .FALSE.								! make it free
	STATUS = 1											! and return success
	RETURN
	END
