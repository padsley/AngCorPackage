	SUBROUTINE CIODSS (SPSTR)
C++
C TITLE:  CIODSS
C FACILITY:  CIO  - conversational I/O package.
C ABSTRACT: Decode specification string.
C ENVIRONMENT:  FORTRAN-77.
C PROCEDURES CALLED:
C	CIO1ST, CIOMSG, CIOPUR, INDEX
C	STR__UPCASE
C AUTHOR: Peter A. Kroon, KVI, Groningen, NL.
C CREATION DATE:  21-oct-1980.
C MODIFIED BY:
C PAK 26-jan-1981 Convert specification string to uppercase before examining.
C PAK 27-feb-1981 Allow specifiers to force octal and hex conversion
C PAK 10-apr-1981 Add P specifier which purges the internal input buffer.
C FUNCTIONAL DESCRIPTION:
C	This procedure examines the specification string SPSTR.
C	Each legal character in the string causes the corresponding
C	switch KSPxxx to be set equal to the position of that
C	character in the specification string.
C	Separate position counters are maintained for argument-
C	specifiers (lower limit,higher limit,default) and alternate
C	return specifiers.
C	The 'C' (convert lower to upper case), 'O' and 'X'
C	(radix conversion) switches are treated separately.
C	The 'P' and 'Q' specifiers don't set a switch but immediately
C	cause purging of respectively the internal inputbuffer or the
C	qualifier definition list.
C CALLING SEQUENCE: (only for internal use by CIO)
C	CALL CIODSS (SPSTR)
C INPUT PARAMETERS:
C	SPSTR	specification string
C IMPLICIT OUTPUTS:
C	The optional-argument specification switches in COMMON
C	/CIO001/. These switches are names KSPxxx where xxx indicates
C	the meaning of the switch.
C--

	INCLUDE		'ciocom.inc'

	CHARACTER*(*) SPECS
	PARAMETER	(SPECS	= ' LHDCOX<>^$PQ')
	CHARACTER	SPSTR*(*)
	CHARACTER	TSPSTR*12	! Size must be large enough to
			! contain longest possible specification string.
	INTEGER		I, I1, I2

C      write (*,*) '?????? ciodss called'
C Initialize CIO package.
	CALL CIO1ST

C Reset pointers and switches.
	I1 = 1
	I2 = 1
	KSPLOW = 0
	KSPHIG = 0
	KSPDEF = 0
	KSPCNV = 0
	CSPRDX = 'I'	! Default radix is decimal (I-conversion).
	KSPBCK = 0
	KSPFWD = 0
	KSPUP  = 0
	KSPDOL = 0

C Treat upper and lower case as equal in specification string by first
C converting the entire spec.string to upper case.
C Note that we must convert into a local stringvariable, since we normally
C can and should not modify the actual argument.

C      write (*,*) '?????? str__upcase (1) called with: ', SPSTR
	CALL STR__UPCASE (TSPSTR,SPSTR)	!???g77
C      write (*,*) '?????? str__upcase (2) called returning: ', TSPSTR

C Examine all characters in specification string and set
C corresponding switch according to the position of that character.

	DO 100 I=1,LEN(SPSTR)
	GOTO (100, 21,22,23, 31,32,33, 41,42,43,44, 51,52)
	1	INDEX(SPECS,TSPSTR(I:I))
Cg77	CALL CIOMSG (1,'CIO - bad specification string: '//SPSTR)
	CALL CIOMSG (1,'CIO - bad specification string: ?g77?')

C It's a character specifying an lower-, upper limit or default.

   21	KSPLOW = I1
	GOTO 29
   22	KSPHIG = I1
	GOTO 29
   23	KSPDEF = I1
   29	I1 = I1 + 1
	GOTO 100

C It's one of the conversion specifiers

   31	KSPCNV = 1		! 'C' - convert lower to upper case.
	GOTO 100
   32	CSPRDX = 'O'		! 'O' for default radix.
	GOTO 100
   33	CSPRDX = 'Z'		! 'X' for default radix.
	GOTO 100

C It's a character corresponding to an alternate return.

   41	KSPBCK = I2
	GOTO 49
   42	KSPFWD = I2
	GOTO 49
   43	KSPUP  = I2
	GOTO 49
   44	KSPDOL = I2
   49	I2 = I2 + 1
	GOTO 100

C It's a request to purge the internal input buffer.
C Do it now, no switches involved.

   51	CALL CIOPUR
	GOTO 100

C It's a request to purge the internal input buffer.
C Do it now, no switches involved.

   52	CALL CIOQPU
	GOTO 100

  100	CONTINUE
	RETURN
	END
