	SUBROUTINE CIOINV
C++
C TITLE:  CIOINV
C FACILITY:  CIO  - conversational I/O package.
C ABSTRACT: Invalidate current input buffer and, if not at the zero
C	level input file, close all active levels of alternate files
C	so that input proceeds from the zero level input file.
C ENVIRONMENT:  FORTRAN-77.
C PROCEDURES CALLED: CIOAIC
C AUTHOR: Peter A. Kroon, KVI, Groningen, NL.
C CREATION DATE:  21-oct-1980.
C MODIFIED BY:
C PAK 11-feb-1981 Repaired faulty logic around current-item pointers
C FUNCTIONAL DESCRIPTION:
C	Calls CIOAIC until all alternate files are closed and the
C	zero level input device is reached.
C	Also resets pointers so that effectively the internal
C	inputbuffer is purged.
C CALLING SEQUENCE: (only for internal use by CIO)
C	CALL CIOINV
C--

	INCLUDE 'ciocom.inc'

	INPTR = 0		!Kill current buffer.
	ITPTR1 = 0
	ITPTR2 = 0
	INBUFL = 0
	INBUFC = 0
	LCONTIN = .FALSE.

  100	CALL CIOAIC (*110)	!Drop back to zero level input medium.
	GOTO 100
  110	RETURN
	END
