	SUBROUTINE CIOGIT (PROMPT,ITEM,ITEML,*,*,*,*)
C++
C TITLE:  CIOGIT
C FACILITY:  CIO  - conversational I/O package.
C ABSTRACT: Get and examine next item from current input buffer.
C ENVIRONMENT:  FORTRAN-77.
C PROCEDURES CALLED:
C	CIOAIF, CIOPUR, CIOLIT, CIOQLU, CIOQSH, CIOERL, CIOHLP
C	CIOGBF, CIOPRM, INDEX,  STR__UPCASE
C AUTHOR: Peter A. Kroon, KVI, Groningen, NL.
C CREATION DATE:  21-oct-1980.
C MODIFIED BY:
C PAK 30-jan-1981 Made item extraction a separate routine.
C PAK 13-may-1981 Implemented qualifiers.
C FUNCTIONAL DESCRIPTION:
C	  The next item from the input buffer is located by a call to CIOLIT.
C	If the inputbuffer is empty, CIOPRM is called to prompt for
C	input (in some cases it doesn't), and CIOGBF is called to read input.
C	  After an unquoted item is extracted, it it examined to see if it
C	equals one of the special characters. If so, the corresponding
C	alternate return is taken.
C	  If the item starts with an @-sign it is considered to be the
C	specification for an alternate input file. CIOAIF is called
C	to open this file, the current inputbuffer is purged, and
C	reading continues on the alternate input file.
C CALLING SEQUENCE: (only for internal use by CIO)
C	CALL CIOGIT (PROMPT,ITEM,ITEML,*,*,*,*)
C INPUT PARAMETERS:
C	PROMPT	characterstring to be used as prompt in case input
C		buffer is empty, no continuation symbol was present,
C		and new input is required.
C OUTPUT PARAMETERS:
C	ITEM	character string receiving the extracted item.
C	ITEML	integer receiving the nr of characters returned
C		in ITEM.
C ALTERNATE RETURNS:
C	1	item is 'keep-current-value' character.
C	2	item is 'take-default' character.
C	3	item is 'show-parameters' character.
C	4	item is 'flow-control' character.
C--

	INCLUDE 'ciocom.inc'

	CHARACTER*(*)	ITEM,PROMPT
      INTEGER           ITEML

      LOGICAL           LQUOTE,LQUALERR
      CHARACTER*4       CTEMP
      INTEGER           IRET
      INTEGER           I1, I2

C Extract next item from input buffer.

C      write (*,*) '?????? ciogit called, inbufc=', INBUFC

	LQUALERR = .FALSE.		! clear qualifier error flag.

   10	IRET = 0				! Assume normal return.
	CALL CIOLIT (INPTR, I1, I2, LQUOTE, *550) ! Locate next item. If buffer
                                        ! empty, go prompt and get new.
C      write (*,*) 'TEST OUTPUT'
      ITEM = INBUF(I1:I2)               ! Extract it,
      ITEML = I2 - I1 + 1               ! also save length.

	ITPTR1 = I1			! Save location of item so
	ITPTR2 = I2			! CIOERL can mark it if it wishes.

C See if item is a qualifier.
C If qualifier, lookup in qualifier list and update associated variable.
C If no qualifiers allowed, complain and simply go back for next item.
C If unknown or ambiguous qualifier then complain and go get new line in
C inputbuffer, but don't prompt. Then go back for next item.
C This give the user the choise of either re-entering the qualifier or
C simply omit it.

	CALL CIOQLU (I1, I2, *24, *10, *22)	! See if qual. and handle it.
      GOTO 10                           ! Good qualifier, try next item.
  22	LQUALERR = .TRUE.			! Qualifier error, remember
	GOTO 560				! and get new input, no prompt.
  24	CONTINUE				! Not a qualifier.

C Quoted non-qualifier items get no further examination.
C They are returned literally.

	IF (LQUOTE) RETURN

C Examine 1 character item. It may be a control character.
C Check this, and set IRET to take the appropriate return.
C IRET will take one of the following values:
C 	0	single character, unquoted, and non-special
C	1	keep-current-value character (=)
C	2	take-default-value character (*)
C	3	show-parameters character    (?)
C	4	any flow-control character   (< > ^ $)

	IF (ITEML.EQ.1) THEN
	  IRET = MIN (4, INDEX (S_CHAR, ITEM(1:1)))
	  GOTO 100
	ENDIF

C Item longer than 1 character.
C If last character of the item is a colon (:), ignore the item,
C it is an intermixed comment string. Go extract next item.

	IF (INBUF(I2:I2) .EQ. ':') GOTO 10

C If item equals the 4-character string HELP (in either upper or lower case)
C call on-line HELP routine, then go get next item.

	CALL STR__UPCASE (CTEMP, ITEM(1:4))
	IF (ITEML.EQ.4.AND.CTEMP.EQ.'HELP') THEN
	  CALL CIOHLP			! get help
	  IF (LQUALERR) THEN		! if just had qualifier error
	    GOTO 560			! get input, don't prompt
	  ELSE				! else
	    GOTO 10			! go get next item.
	  ENDIF
	ENDIF

C If item starts with an @-sign assume it is an alternate filename. Open
C the alternate file, purge the current inputbuffer, then read a new
C buffer from the alternate file.

	IF (ITEM(1:1).EQ.'@') THEN
	  CALL CIOAIF (ITEM(2:ITEML), *510, *520)
	  CALL CIOPUR
	  GOTO 550
	ENDIF

C Item extracted and analysed.
C If it is the questionmark in response to a qualifier error, react by
C displaying the legal qualifiers, then go get next item.

  100	IF (IRET.EQ.3 .and. LQUALERR) THEN
	  CALL CIOQSH			! show legal qualifiers
	  GOTO 560			! get input, don't prompt
	ENDIF

C In all other cases take the appropriate return.

	RETURN IRET

C Alternate file nesting too deep or file-open error.
C Complain, invalidate inputbuffer and alternate files, then
C join code below for prompting and reading new inputbuffer.

  510	CALL CIOERL ('alternate file nesting too deep')
	GOTO 550
  520	CALL CIOERL ('open error on alternate input file')

C Come here if inputbuffer exhausted. Prompt, then read new buffer.

  550	CALL CIOPRM (PROMPT)		! prompt
	LQUALERR = .FALSE.		! clear qualifier error flag.
  560	CALL CIOGBF			! read input buffer.
	GOTO 10
	END
