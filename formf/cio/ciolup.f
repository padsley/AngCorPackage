	SUBROUTINE CIOLUP (KEYWRD,KEYTBL,NKEYS,KEYVAL)
C++
C TITLE:  CIOLUP
C FACILITY:  CIO  - conversational I/O package.
C ABSTRACT: Lookup (abbreviated) keyword in table and return its
C	entry number.
C ENVIRONMENT:  FORTRAN-77.
C PROCEDURES CALLED:
C	INDEX, STR__UPCASE
C AUTHOR: Peter A. Kroon, KVI, Groningen, NL.
C CREATION DATE:  21-oct-1980.
C MODIFIED BY:
C PAK 26-jan-1981 Changed call to CIOCLU to STR__UPCASE.
C FUNCTIONAL DESCRIPTION:
C	KEYTBL(1...NKEYS) is scanned for the occurance of KEYWRD.
C	If an exact match is found, the entry nr is returned in KEYVAL.
C	If a partial match is found, the entry nr is remembered and
C	the scan continues. If another partial match is found, the
C	KEYWRD is ambigious and KEYVAL=0 is returned.
C	If an exact match is found, that entry is returned in KEYVAL.
C	If no further match is found the entry nr of the partial
C	match is returned in KEYVAL.
C	If no match is found at all, KEYVAL=-1 is returned.
C CALLING SEQUENCE:
C	CALL CIOLUP (KEYWRD, KEYTBL, NKEYS, KEYVAL)
C INPUT PARAMETERS:
C	KEYWRD	character string to be searched for in KEYTBL.
C		KEYWRD is converted to uppercase before the search.
C	KEYTBL	character array containing the lookup table.
C	NKEYS	dimension of (nr of elements in) KEYTBL.
C OUTPUT PARAMETERS:
C	KEYWRD	the original contents of KEYWRD with lowercase
C		characters converted to uppercase.
C	KEYVAL	entry nr of matching entry in KEYTBL, or
C		0 if KEYWRD is ambigious, or -1 if KEYWRD does not
C		occur in KEYTBL.
C--

	CHARACTER*(*)	KEYWRD,KEYTBL(1)
	LOGICAL*1	FOUND

C Determine length of KEYWRD and convert lower to upper case.
	LENKEY = INDEX (KEYWRD,' ') - 1
	IF (LENKEY.LE.0) LENKEY = LEN (KEYWRD)
	FOUND = .FALSE.
	CALL STR__UPCASE (KEYWRD(:LENKEY), KEYWRD(:LENKEY))

C Search table for exact or partial match.
	DO 10 I = 1,NKEYS
	IF (KEYWRD.EQ.KEYTBL(I)) THEN
	  KEYVAL = I
	  GOTO 20
	ENDIF
	IF (KEYWRD(:LENKEY).EQ.KEYTBL(I)(:LENKEY)) THEN
	  IF (.NOT.FOUND) THEN
	    FOUND = .TRUE.
	    KEYVAL = I
	  ELSE
	    KEYVAL = 0
	    RETURN
	  ENDIF
	ENDIF
   10	CONTINUE

C Whole table has been searched. See if partial match found.
	IF (.NOT.FOUND) THEN
	  KEYVAL = -1
	  RETURN
	ENDIF

C Succesful completion. Return entry number.
   20	RETURN
	END
