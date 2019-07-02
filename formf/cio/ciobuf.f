	SUBROUTINE CIOBUF (USRBUF)
C++
C TITLE:  CIOBUF
C FACILITY:  CIO  - conversational I/O package.
C ABSTRACT: Supply input buffer to CIO.
C ENVIRONMENT:  FORTRAN-77.
C PROCEDURES CALLED: CIO1ST, CIOPAB
C AUTHOR: Peter A. Kroon, KVI, Groningen, NL.
C CREATION DATE:  21-oct-1980.
C MODIFIED BY:
C PAK 29-jan-1981: Added stripping of comment field.
C PAK 11-feb-1981 Repaired faulty logic around current-item pointers
C ENTRIES:
C	CIOBUF, CIOPUR
C FUNCTIONAL DESCRIPTION:
C	CIOBUF:
C	  The supplied characterstring USRBUF is copied into CIO's
C	  internal inputbuffer.
C	CIOPUR:
C	  CIO's internal inputbuffer is purged. This forces CIO to
C	  read a new buffer from the input file when it requires input.
C CALLING SEQUENCE:
C	CALL CIOBUF (USRBUF)
C	CALL CIOPUR
C INPUT PARAMETERS:
C	USRBUF	the characterstring to be copied into CIO's internal
C		input buffer.
C--

	INCLUDE 'ciocom.inc'
	CHARACTER USRBUF*(*)

	CALL CIO1ST		!Perform once only code
	INBUF = USRBUF		!Copy user's buffer to internal buffer
	INBUFL = LEN(USRBUF)	!Copy actual length
	CALL CIOPAB		!Pre-analyse buffer.
	RETURN

C CIOPUR - purge CIO's internal input buffer.

	ENTRY CIOPUR

	CALL CIO1ST		!Perform once only code.
	INBUFL = 0		!Zero buffer length
	INBUFC = 0		!Zero content length
	INPTR = 0		!Zero buffer pointer
	ITPTR1 = 0		!Zero current item pointers
	ITPTR2 = 0
	LCONTIN = .FALSE.	!Clear continuation flag
	RETURN
	END
