	SUBROUTINE CIOPAB
C++
C TITLE:  CIOPAB
C FACILITY:  CIO  - conversational I/O package.
C ABSTRACT: Find size of data in inputbuffer excluding possible comment field
C and continuation character. Sets flag if continuation character present.
C Reset bufferpointer and current item pointers
C ENVIRONMENT:  FORTRAN-77
C PROCEDURES CALLED:
C AUTHOR: Peter A. Kroon, KVI, Groningen, NL.
C CREATION DATE:  29-jan-1981.
C MODIFIED BY:
C PAK 11-feb-1981 Repaired faulty logic around current-item pointers
C FUNCTIONAL DESCRIPTION:
C CALLING SEQUENCE: (only for internal use by CIO)
C	CALL CIOPAB
C IMPLICIT INPUTS AND OUTPUTS:
C	INBUF, INBUFL, INBUFC, INPTR, ITPTR1, ITPTR2 and LCONTIN in common.
C--

	INCLUDE 'ciocom.inc'

C External functions

	INTEGER LIB__SCANC

	INTEGER	K

      INPTR = 0         ! Reset inputbuffer pointer
	ITPTR1 = 0	! and current-item pointers
	ITPTR2 = 0

C      write (*,*) '?????? ciopab, inbufl, inbuf:', inbufl, inbuf

C Scan for first occurance of a comment character

	K = LIB__SCANC (INBUF(:INBUFL), C_TABLE, C_COMM)	!???g77
C      write (*,*) '?????? ciopab, k=scanc:', k
	IF (K-1) 20, 10, 30

C 1st character is comment character. No data or continuation symbol.
   10	INBUFC = 0
	LCONTIN = .FALSE.
	RETURN

C No comment symbol found.
   20	INBUFC = INBUFL
	GOTO 32

C Comment field starts at K which is not the 1st character.
   30	INBUFC = K - 1

C Trim trailing blanks and tabs from data field.
C Then if last character is continuation symbol, trim it and set flag.

C  32	CALL STR__TRIM (INBUF(:INBUFC), INBUF(:INBUFC), INBUFC)
C  replaced by:

   32 DO K = INBUFC, 1, -1
        IF (INBUF(K:K).NE.' ' .AND. INBUF(K:K).NE.CHAR(9)) GOTO 34
	END DO
   34	INBUFC = K

	LCONTIN = INBUF(INBUFC:INBUFC) .EQ. S_CONT
	IF (LCONTIN) INBUFC = INBUFC - 1

	RETURN
	END
