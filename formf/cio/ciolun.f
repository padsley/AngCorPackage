	SUBROUTINE CIOLUN (LI,LO)
C++
C TITLE:  CIOLUN
C FACILITY:  CIO  - conversational I/O package.
C ABSTRACT: Change logical unit numbers for zero level I/O.
C ENVIRONMENT:  FORTRAN-77.
C PROCEDURES CALLED:
C	CIO1ST, CIOERR
C AUTHOR: Peter A. Kroon, KVI, Groningen, NL.
C CREATION DATE:  21-oct-1980.
C MODIFIED BY:
C FUNCTIONAL DESCRIPTION:
C	Assigns the logical unit numbers in the call to the internal
C	logical unit numbers for the zero level input device.
C CALLING SEQUENCE:
C	CALL CIOLUN (LI,LO)
C INPUT PARAMETERS:
C	LI	the new logical unit nr for CIO zero level input.
C	LO	the new logical unit nr for CIO output of prompts
C		and messages.
C--

	INCLUDE 'ciocom.inc'

	INTEGER	LI, LO

	CALL CIO1ST
	IF (LUNSTP.NE.0) THEN
	  CALL CIOERR(1, 'Call to CIOLUN illegal while alt input active')
	ELSE
	  LUNI = LI
	  LUNIS = LI
	  LUNO = LO
	ENDIF
	RETURN
	END
