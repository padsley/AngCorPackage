	SUBROUTINE CIOCMX (PROMPT,KEYTBL,NKEYS,KEYVAL,SPECST,*,*,*,*)
C++
C TITLE:  CIOCMX
C FACILITY:  CIO  - conversational I/O package.
C ABSTRACT: Prompt for and read commandline, extract keyword and decode.
C ENVIRONMENT:  FORTRAN-77.
C PROCEDURES CALLED:
C	CIODSS	to decode specification string
C	CIOGIT	to get the next item in the internal inputbuffer.
C	CIOLUP	to lookup a command in a commandtable.
C	CIOERL	to display error message and incurring line.
C	CIOHLP	to offer on-line help.
C AUTHOR: Peter A. Kroon, KVI, Groningen, NL.
C CREATION DATE:  10-apr-1981
C MODIFIED BY:
C PAK 19-feb-1984 In dynamic FORMAT labelled 9030 moved a call to LEN outside
C	the FORMAT statement. Caused a compiler crash with FORTRAN V4.0
C FUNCTIONAL DESCRIPTION:
C	CIOPRM is called to prompt for a command and read a commandline.
C	The first item in the line is assumed to be a keyword and is
C	looked up in a keyword table (KEYTBL (1...NKEYS)) through a
C	call to the procedure CIOLUP. If the command is found its
C	entrynumber in the table (KEYVAL).
C CALLING SEQUENCE:
C	CALL CIOCMX (PROMPT,KEYTBL,NKEYS,KEYVAL,SPECST [,alternate returns])
C INPUT PARAMETERS:
C	PROMPT	prompt string to be displayed.
C	KEYTBL	character array containing the command repertoire.
C	NKEYS	nr of commands in KEYTBL.
C	SPECST	specification string.
C OUTPUT PARAMETERS:
C	KEYVAL	receives entrynumber of the command in KEYTBL, or
C		0 if the command is ambigious, or
C		-1 if the command is not in KEYTBL, or
C		-2 if '=' was entered, or
C		-3 if '*' was entered.
C ALTERNATE RETURNS:
C	1 to 4 alternate returns, their number and sequence depending on
C	the precense of flow control characters in SPECST.
C	If any of the specified flow-control characters is entered,
C	the corresponding alternate return is taken.
C--

	INCLUDE 'ciocom.inc'
	CHARACTER*(*)	PROMPT,KEYTBL(1),SPECST
	INTEGER			NKEYS, KEYVAL
	CHARACTER*(80) 	DYNFMT
	INTEGER			I, K
	INTEGER			KRETUR
	INTEGER			STATUS

C Decode the specification string.
C Note that if the string contains a 'P' this will purge the internal
C input buffer.

	CALL CIODSS (SPECST)

C Prompt for a command line.
C Then extract the first item in the buffer, and call the
C command-table lookup routine.

   10	CALL CIOGIT (PROMPT,CWORK,K, *22, *23, *30, *100)
C				      =    *    ?   flow
	CALL CIOLUP (CWORK(:K),KEYTBL,NKEYS,KEYVAL)
	GOTO 700

C The item extracted is one of the special CIO characters '=' or '*'.
C Return a negative value which will normally be treated as a command
C that could not be found in the table.

   22	KEYVAL = -2			! = character
	GOTO 700
   23	KEYVAL = -3			! * character
	GOTO 700

C Come here if extracted item happens to be the 'show-current-value'
C symbol. In this procedure we act on it by showing the command
C repertoire, followed by the list of defined qualifiers.

C Dynamic formatting is used to adapt the nr of command displayed
C per line to the length of the commands in characters.
C ???g77 to be tested

   30	K = LEN(KEYTBL(1))
	WRITE (DYNFMT, 9030) 50/(K+2)
 9030 FORMAT (':,T8,''Commands ............'',(T30,',I2,'(A,:,2X)')
 	WRITE (LUNO,DYNFMT) (KEYTBL(I),I=1,NKEYS)

C 9030	FORMAT(:,T8,'Commands ............',(T30,<50/(K+2)>(A,:,2X)))

	CALL CIOQSH			! show defined qualifiers.
	WRITE (LUNO,'(1X)')		! terminate with blank line.
	GOTO 10

C CIO flow control character entered.
C See if alternate return has been specified for this control
C character. If so, take that return. If not complain, and
C invalidate input buffer and alternate files.

  100	I = INDEX (S_FLOW, CWORK(1:1))
	IF (KSPCTL(I).EQ.0) THEN
	  CALL CIOERL('you can''t go that way')
	  GOTO 10
	ELSE
	  KRETUR = KSPCTL(I)
	  GOTO 710
	ENDIF

C Come here just before returning to the caller in order to purge
C the internal promptbuffer, delete the possibly open prompt-
C scratchfile, and deallocate the corresponding logical unit number.
  700	KRETUR = 0
C Code for alternate returns caused by flow-control-char's joins here.
  710	IPBPTR = 0
	IF (LUNPRF .NE. 0) THEN
	  CLOSE (UNIT = LUNPRF)
	  CALL CIOLUNFREE (LUNPRF, STATUS) ! ???G77 status testen
	  LUNPRF = 0
	ENDIF
	RETURN KRETUR

	END
