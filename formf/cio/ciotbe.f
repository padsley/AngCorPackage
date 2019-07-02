	SUBROUTINE CIOTBE (LEMPTY)
C++
C TITLE:  CIOTBE
C FACILITY:  CIO  - conversational I/O package.
C ABSTRACT: Test if internal input buffer empty.
C ENVIRONMENT:  FORTRAN-77.
C PROCEDURES CALLED: LIB__PANC
C AUTHOR: Peter A. Kroon, KVI, Groningen, NL.
C CREATION DATE:  30-jan-1981.
C MODIFIED BY:
C FUNCTIONAL DESCRIPTION:
C CALLING SEQUENCE:
C	CALL CIOTBE (LEMPTY)
C OUTPUTS:
C	LEMPTY	logical, returned .true. if buffer empty and
C		no continuation symbol specified.
C--

	INCLUDE 'ciocom.inc'

C External functions

	INTEGER LIB__SPANC

	LOGICAL LEMPTY

C First complete scan for qualifiers. Otherwise possible qualifiers
C remaining on the line will indicate a non-empty buffer although
C the only items left are qualifiers.

	CALL CIOQCO

C Then set empty switch if all of the following is true:
C	- the line doesn't end on a continuation character,
C	- the pointer has not yet reached the end of the line or
C	  the comment field
C	- the only characters left are separators.

	LEMPTY =
	1 .not. LCONTIN .and.
	1 (INPTR.GE.INBUFC .or.
	1 LIB__SPANC(INBUF(INPTR+1:INBUFC), C_TABLE, C_SEPA) .eq. 0)	!???2006
	RETURN
	END
