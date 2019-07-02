
	SUBROUTINE CIOMSG(ISEVER,MESSAG)
C++
C TITLE:  CIOMSG
C FACILITY:  CIO  - conversational I/O package.
C ABSTRACT: Procedure to display informative, warning and error
C	messages.
C ENVIRONMENT:  FORTRAN-77.
C	Calls VAX/VMS specific routine LIB$STOP for program abort
C	with traceback.
C PROCEDURES CALLED:
C AUTHOR: Peter A. Kroon, KVI, Groningen, NL.
C CREATION DATE:  21-oct-1980.
C MODIFIED BY:
C PAK 24-nov-1980.	Allow ISEVER = 2.
C PAK 22-feb-1985.	Change condition code for LIB$STOP to SS$_ABORT
C			instead of simply zero.
C PAK 10-sep-1985.	Modify outputformat of MESSAG to prevent output
C			statement overflow.
C FUNCTIONAL DESCRIPTION:
C	The string specified by MESSAG is displayed.
C	If CIO is currently reading from an alternate device, indicated
C	by LUNSTP.ne.0, the alternate device level is also displayed.
C	Whether the routine returns to the main program depends on
C	ISEVER as follows:
C	ISEVER = 0: return to calling program.
C	ISEVER = 1: abort program.
C	ISEVER = 2: if inputdevice is terminal, return, else abort.
C CALLING SEQUENCE: (only for internal use by CIO)
C	CALL CIOMSG (ISEVER,MESSAG)
C INPUT PARAMETERS:
C	ISEVER	severity of message. See above.
C	MESSAG	character string to be displayed.
C--

	INCLUDE 'ciocom.inc'

	CHARACTER*(*)	MESSAG
	INTEGER			ISEVER

	WRITE (LUNO,9010) MESSAG(:MIN(LEN(MESSAG),132))
	IF (LUNSTP.NE.0) THEN
	  WRITE (LUNO,9020) LUNSTP
	ENDIF
 9010	FORMAT(1X,T8,A)
 9020	FORMAT(1X,T8,'(alternate file level ',I2,')')

	IF (ISEVER.EQ.0) THEN
	  RETURN
	ELSE IF (ISEVER.EQ.1) THEN
	  STOP 1	!???g77
	ELSE IF (ISEVER.EQ.2) THEN
	  IF (MODI.EQ.0) THEN
	    RETURN
	  ELSE
	    STOP 1      !???g77
	  ENDIF
	ENDIF
	END
