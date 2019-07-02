	SUBROUTINE CIOGBF
C++
C TITLE:  CIOGBF
C FACILITY:  CIO  - conversational I/O package.
C ABSTRACT: Get input buffer from current input file, and process
C	end-of-file and read-error conditions.
C ENVIRONMENT:  FORTRAN-77 except for the use of the Q format specifier.
C PROCEDURES CALLED:
C	CIOAIC, CIOERR, CIOMSG, CIOPAB
C AUTHOR: Peter A. Kroon, KVI, Groningen, NL.
C CREATION DATE:  21-oct-1980.
C MODIFIED BY:
C PAK 29-jan-1981 Added check for comment field and continuation character
C			by call to CIOPAB.
C PAK 11-feb-1981 Repaired faulty logic around current-item pointers
C FUNCTIONAL DESCRIPTION:
C	  The next record from the device with logical unit nr LUNI
C	is read into the internal inputbuffer INBUF.
C	  If no special condition occurs, CIOPAB is called to pre-analyse
C	the buffer (total length, length excl. comment, continuation symbol).
C	  If an end-of-file is detected, a message is displayed
C	and CIOAIC is called in order to switch input back to
C	the previous level alternate file if it exists.
C	  If the zero level file is reached, and the program runs
C	in non-interactive mode, a message is issued and the
C	program is aborted.
C	  If a read error occurs, a message is displayed. Then, if
C	in non-interactive mode, the program is aborted, if in
C	interactive mode, al alternate files are closed, the
C	inputbuffer is purged and input proceeds from the zero
C	level input device.
C CALLING SEQUENCE: (only for internal use by CIO)
C	CALL CIOGBF
C IMPLICIT OUTPUTS:
C	INBUF, INBUFL, INBUFC, INPTR, ITPTR1, ITPTR2 and LCONTIN in common.
C--

	INCLUDE 'ciocom.inc'

C Reset input-buffer-pointer. Read next buffer from current
C input file and if no EOF or error, call CIOPAB to pre-analyse the input.
C i.e. find the part before the comment field, and determine
C whether a continuation symbol is present. Then return to caller.

C Linux/f77 doesn't support the 'Q' format code. In OpenVMS Fortran
C this provides the number of characters remaining in to be transferred
C in an input record, so that we know how much will be read into INBUF.
C The main purpose is to detect that the input record is longer than
C will fit in INBUF, and issue a warning if needed.
C
C In Linux, we remove the Q. As a result, too long input records go
C undetected. Also, as we don't know the length of the actual content
C of INBUF, we rely on CIOPAB() to remove the trailing spaces.
C A bit expensive, but good enough for the time being.

C 100	READ (LUNI,'(Q,A)', END=200, ERR=300) INBUFL,INBUF
C	IF (INBUFL .GT. INBSIZ) THEN
C	   CALL CIOMSG (0,'warning - input line truncated')
C	   INBUFL = INBSIZ
C	ENDIF

  100	READ (LUNI,'(A)', END=200, ERR=300) INBUF
	INBUFL = LEN(INBUF)
	CALL CIOPAB	! Pre-analyse buffer.
	RETURN

C End-of-file encountered.
C Issue an informative message. (CIOMSG will indicate the alternate
C file level if other than zero).
C Then attempt to fall back to previous alternate-file level, and if
C it exists, proceed reading from that file.

  200	INBUFL = 0
	INBUFC = 0
	INPTR = 0
	ITPTR1 = 0
	ITPTR2 = 0
	LCONTIN = .FALSE.
	CALL CIOMSG (0,'end-of-file')
	CALL CIOAIC (*210)
	RETURN		!???? misschien beter GOTO 100
			!???? maar dan prompten we niet meer, dus toch niet??

C The end-of-file occurred at zero level input. If input not from
C a terminal, this is fatal; complain and abort.Otherwise simply ignore.

  210	IF (MODI.GT.0) THEN
	  CALL CIOMSG (1,'This is fatal in non-interactive mode')
	ELSE
	  RETURN
	ENDIF

C Read error on current input file.
C We issue a warning message. If not at the zero level input, CIOERR
C will show the alternate-file-level at which the error occured.
C Then we fall back to the zero level input.

  300	CALL CIOERR ('CIO - read error')
	RETURN
	END
