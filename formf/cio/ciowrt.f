	SUBROUTINE CIOWRT (STRING)
C++
C TITLE:  CIOWRT
C FACILITY:  CIO  - conversational I/O package.
C ABSTRACT: Buffers STRING till a later call to one of the entries
C	in CIOX. The buffered strings will then be displayed by the
C	routine CIOPRM before the prompt specified in the CIOX call.
C	If the CIOX call returns without having prompted for input
C	(since the inputbuffer still contained data) the buffered
C	strings are purged.
C	The use of this procedure is at those places where the amount
C	and the nature of the text to be used as prompt for input,
C	cannot properly be handled by the PROMPT parameter in the
C	various CIO calls. E.g. in the case the prompt text occupies
C	more than one line of text.
C ENVIRONMENT:  FORTRAN-77.
C PROCEDURES CALLED:
C AUTHOR: Peter A. Kroon, KVI, Groningen, NL.
C CREATION DATE:  21-oct-1980.
C MODIFIED BY:
C 26-jan-1981 PAK, Replaced call to LUNGET by call to LIB$GET_LUN
C FUNCTIONAL DESCRIPTION:
C	STRING is stored in an internal promptbuffer (PRBUF).
C	If, however, this internal buffer is full, a scratch file
C	is opened and STRING is written to the scratch file.
C	Subsequent calls of this routine also write STRING to this file
C	until CIOX closes the file (with deletion) and purges the
C	internal promptbuffer.
C CALLING SEQUENCE:
C	CALL CIOWRT (STRING)
C INPUT PARAMETERS:
C	STRING	character string to be displayed.
C--
	INCLUDE 'ciocom.inc'
	CHARACTER*(*)	STRING
	INTEGER			STATUS

C Once-only initialize CIO package.
	CALL CIO1ST

C If room in the promptstring buffer (PRBUF) store STRING there.
C Append a @ sign for later truncation of trailing blanks.
	IF (IPBPTR.LT.PRBLEN) THEN
	  IPBPTR = IPBPTR + 1
	  PRBUF(IPBPTR) = STRING // '@'

	ELSE
C If no room in the promptstringbuffer then
C open a scratch file if not already open
	  IF (LUNPRF.EQ.0) THEN
	    CALL CIOLUNGET(LUNPRF, STATUS)	!???g77 status testen
C	    OPEN (UNIT=LUNPRF, NAME='CIO.TM1', TYPE='SCRATCH')
C???g77	    OPEN (UNIT=LUNPRF, NAME='CIO.TM1', DISPOSE='DELETE')
	  ENDIF

C And write the promptstring to the file.

	  WRITE (LUNPRF,9000) STRING
 9000	  FORMAT(A,'@')

	ENDIF

	RETURN
	END
