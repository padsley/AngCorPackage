	CHARACTER*3 FUNCTION CIOYNL (L)
C++
C TITLE:  CIOYNL
C FACILITY:  CIO  - conversational I/O package.
C ABSTRACT: Character function which returns the string 'YES' or 'NO'
C	corresponding to the value of its logical argument.
C ENVIRONMENT:  FORTRAN-77.
C AUTHOR: Peter A. Kroon, KVI, Groningen, NL.
C CREATION DATE:  21-oct-1980.
C CALLING SEQUENCE:
C	STRING =  CIOYNL (L)
C INPUT PARAMETERS:
C	L	logical to be converted to a 'YES' or 'NO' string.
C--

	LOGICAL	L
	IF (L) THEN
	  CIOYNL = 'YES'
	ELSE
	  CIOYNL = 'NO'
	ENDIF
	RETURN
	END
