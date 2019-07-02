	SUBROUTINE CIOERL (MESSAG)
C++
C TITLE:  CIOERL
C FACILITY:  CIO  - conversational I/O package.
C ABSTRACT: Display error message (and incurring line).
C PROCEDURES CALLED:
C	CIOMSG, CIOINV, LIB$SYS_GETMSG
C AUTHOR: Peter A. Kroon, KVI, Groningen, NL.
C CREATION DATE:  24-nov-1980.
C MODIFIED BY:
C PAK 28-apr-1981 Made two entries CIOERR and CIOERL.
C PAK 19-may-1981 Added entry CIOERS
C PAK 11-feb-1981 Repaired faulty logic around current-item pointers
C--

	INCLUDE 'ciocom.inc'
	CHARACTER*(*)	MESSAG
	INTEGER         IST, K
C++
C ENTRY: CIOERL
C ABSTRACT: Display error message and invoking item.
C FUNCTIONAL DESCRIPTION:
C	The current internal input buffer is displayed with the
C	latest extracted item marked.
C	The string specified by MESSAG is displayed through a call
C	to CIOMSG, which will abort the program if not in
C	interactive mode.
C	Then CIOINV is called to discard the current inputbuffer
C	contents and to close all open alternate input files.
C CALLING SEQUENCE:
C	CALL CIOERL (MESSAG)
C INPUTS:
C	MESSAG	character string to be displayed.
C SIDE EFFECTS:
C	Program is aborted if in interactive mode.
C--

	IF (INBUFL.GT.0) WRITE (LUNO,9000)
	1 INBUF(1:ITPTR1-1), INBUF(ITPTR1:ITPTR2), INBUF(ITPTR2+1:INBUFL)
 9000	FORMAT (1X,T8,A,'<',A,'>',A)

C++
C ENTRY: CIOERR
C ABSTRACT: Display error message.
C FUNCTIONAL DESCRIPTION:
C	The string specified by MESSAG is displayed through a call
C	to CIOMSG, which will abort the program if not in
C	interactive mode.
C	Then CIOINV is called to discard the current inputbuffer
C	contents and to close all open alternate input files.
C CALLING SEQUENCE:
C	CALL CIOERR (MESSAG)
C INPUTS:
C	MESSAG	character string to be displayed.
C SIDE EFFECTS:
C	Program is aborted if in interactive mode.
C--

	ENTRY CIOERR (MESSAG)
C	---------------------
	CALL CIOMSG (2,MESSAG)
	CALL CIOINV
	RETURN

C++
C ENTRY: CIOERS
C ABSTRACT: Display system message according to cond.code.
C FUNCTIONAL DESCRIPTION:
C	The message specified by the condition code IST is obtained by a call
C	to LIB$SYS_GETMSG and displayed through a call to CIOMSG,
C	which will abort the program if not in interactive mode.
C	Then CIOINV is called to discard the current inputbuffer
C	contents and to close all open alternate input files.
C CALLING SEQUENCE:
C	CALL CIOERS (IST)
C INPUTS:
C	IST	condition code for message to be displayed.
C SIDE EFFECTS:
C	Program is aborted if in interactive mode.
C--

	ENTRY CIOERS (IST)
C	------------------

C	This VMS code not portable to Linux. Q&D workaround.
C	CALL LIB$SYS_GETMSG (IST, K, CWORK)	!???g77
C	CALL CIOMSG (2,CWORK(:K))
	WRITE (CWORK, 100) IST
100	FORMAT ('Error status I10')
	CALL CIOMSG (2, CWORK)
	CALL CIOINV
	RETURN
	END
