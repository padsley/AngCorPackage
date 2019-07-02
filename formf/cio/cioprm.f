	SUBROUTINE CIOPRM (PROMPT)
C++
C TITLE:  CIOPRM
C FACILITY:  CIO  - conversational I/O package.
C ABSTRACT: If input device is a terminal and input
C   is not from an alternate file, then:
C   if the continuation flag is not  et, display possible contents of
C   promptbuffer, possible contents of promptfile, and promptstring;
C   but if the continuation flag is set, only display the continuation prompt.
C ENVIRONMENT:  FORTRAN-77 except for the use of the $ formatspecifier.
C PROCEDURES CALLED: None.
C AUTHOR: Peter A. Kroon, KVI, Groningen, NL.
C CREATION DATE:  21-oct-1980.
C MODIFIED BY:
C PAK 30-jan-1981: Changed for new prompting philosophy in case of
C	continuation.
C FZ  29-Oct-1987: Prompt to input device. Check only whether input
C	device is a terminal.
C PAK 02-oct-2006: Undid the above change, i.e. prompt to output device,
C     because Linux/f77 doesn't allow writing to stdin.
C
C FUNCTIONAL DESCRIPTION:
C CALLING SEQUENCE: (only for internal use by CIO)
C	CALL CIOPRM (PROMPT)
C INPUT PARAMETERS:
C	PROMPT	characterstring containing the prompt.
C--

	INCLUDE 'ciocom.inc'
	CHARACTER*(*) PROMPT
	INTEGER			I

C Prompts will only be displayed if the inputdevice
C is a terminal and we are not reading from an alternate file.

	IF (MODI.NE.0 .OR. LUNSTP.GT.0) RETURN

C If continuation flag is set, only display the continuation prompt.

	IF (LCONTIN) THEN
        WRITE (LUNO,'(1X,''_'',$)')

C If continuation flag not set then:
C First show the contents of the promptbuffer if it contains something.

	ELSE

   10	  IF (IPBPTR.GT.0) THEN
          WRITE (LUNO,9010)
	1   (PRBUF(I)(:INDEX(PRBUF(I),'@')-1), I=1,IPBPTR)
 9010	    FORMAT(1X,A)
	  ENDIF

C Then see if a scratchfile has been opened (which occurs if more
C promptstrings must be buffered than will fit in the internal buffer.
C If so, show the contents of the file.

   20	  IF (LUNPRF.NE.0) THEN
          REWIND LUNPRF
   22	    READ (LUNPRF,9022,END=30) INBUF
 9022	    FORMAT(A)
          WRITE (LUNO,9010) INBUF(:INDEX(INBUF,'@')-1)
	    GOTO 22
	  ENDIF

C Then display the final prompt if it is non-empty

   30	  IF (PROMPT .NE. ' ') THEN
          WRITE (LUNO,9030) PROMPT
 9030	    FORMAT(1X,A,' ',$)		! note ' ' is not same as 1X
	  ENDIF

	ENDIF
	RETURN
	END
