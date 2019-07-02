	SUBROUTINE CIOLIT (IP, I1, I2, LQUOTE, *)
C++
C TITLE:  CIOLIT
C FACILITY:  CIO  - conversational I/O package.
C ABSTRACT: Locate next item in current input buffer.
C ENVIRONMENT:  FORTRAN-77.
C PROCEDURES CALLED:
C	INDEX, LIB$SCANC, LIB$SPANC
C AUTHOR: Peter A. Kroon, KVI, Groningen, NL.
C CREATION DATE:  30-jan-1981.
C MODIFIED BY:
C FUNCTIONAL DESCRIPTION:
C	The next item from the input buffer is located.
C	If the inputbuffer is empty, alternate return 1 is taken.
C	The item is assumed to begin at the first non-separator
C	character after the current position indicated by IP.
C	The item ends at the detection of a separator
C	or at the end of the inputbuffer.
C	An item may be enclosed in single or double quotes if it
C	contains embedded blanks or one of the special characters
C	In case of a quoted item LQUOTE is returned as .true..
C CALLING SEQUENCE: (only for internal use by CIO)
C	CALL CIOLIT (IP, I1, I2, LQUOTE, *)
C INPUT PARAMETERS:
C	IP	position in INBUF where search should start.
C OUTPUT PARAMETERS:
C	IP	position in INBUF where search ended.
C	I1, I2	indicate position in INBUF where the extracted item
C		is located (thus at INBUF(I1:I2))
C	LQUOTE	logical returned true if the item was quoted.
C ALTERNATE RETURNS:
C	1	buffer empty, no more items can be extracted.
C--

	INCLUDE 'ciocom.inc'

	INTEGER		IP, I1, I2, K
	LOGICAL		LQUOTE
	CHARACTER*1	DELIM

C External functions

	INTEGER LIB__SPANC
	INTEGER LIB__SCANC

C See if anything left in buffer

C      write (*,*) '?????? ciolit called IP, INBUFC', ip, inbufc
	IF (IP.GE.INBUFC) RETURN 1		! Buffer exhausted
	IP = IP + 1

C Skip over separators till the 1st non-separator.
C Take alternate return if only data left is separators.

C      write (*,*) '?????? ciolit before spanc'
	K = LIB__SPANC(INBUF(IP:INBUFC),C_TABLE,C_SEPA)	! Skip over all
	IF (K.EQ.0) THEN	! characters in the group of separators.
	  IP = INBUFC
	  RETURN 1
	ENDIF

C We are at the 1st character of an item. See if quoted or unquoted.

	I1 = IP + K - 1	! Point to 1st char of item.
	DELIM = INBUF(I1:I1)			! Get possible delimiter.
	LQUOTE = INDEX(S_DELM, DELIM) .NE. 0	! See if quoted item.

C Quoted item. Extract it.
C Item ends at the next occurance of the same delimiter.

	IF (LQUOTE) THEN	! Item is quoted.
	  I1 = I1 + 1		! Skip over 1st quote
	  K = INDEX (INBUF(I1:INBUFC), DELIM)	! Search 2nd.
	  IF (K.NE.0) THEN
	    I2 = K + I1 - 2	! Found, I2 is last char before quote.
	    IP = K + I1 - 1	! Update bufferpointer to 2nd quote.
	  ELSE
	    I2 = INBUFC		! Not found, assume EOL is delimiter
	    IP = INBUFC		! Update bufferpointer to EOL.
	  ENDIF

C Unquoted item. Extract it.
C Item ends at a separator or at the start of a qualifier.

	ELSE			! Item is unquoted.
				! Scan for a character in the group of
				! separators or qualifier-prefix(es).
	  K = LIB__SCANC (INBUF(I1+1:INBUFC), C_TABLE, C_SEPA+C_QUAL)
	  IF (K.NE.0) THEN
	    I2 = K + I1 - 1	! Found.
	  ELSE
	    I2 = INBUFC		! Not found. Assume EOL is separator.
	  ENDIF
	  IP = I2		! Update bufferpointer to end of item.
	ENDIF

C Item located in INBUF(I1:I2). Return substring limits to caller.
C IP pointer points at last character of item, or to the terminating quote
C of a quoted item.
C Note that quoted items may have a length of zero !!

	RETURN
	END
