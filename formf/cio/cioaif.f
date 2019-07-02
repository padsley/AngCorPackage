	SUBROUTINE CIOAIF (FILALT,*,*)
C++
C TITLE:  CIOAIF
C FACILITY:  CIO  - conversational I/O package.
C ABSTRACT: Switch to alternate input file, referred to by
C	file-specification.
C ENVIRONMENT:  FORTRAN-77.
C PROCEDURES CALLED:
C	CIOLUNGET, CIOLUNFREE
C AUTHOR: Peter A. Kroon, KVI, Groningen, NL.
C CREATION DATE:  21-oct-1980.
C MODIFIED BY:
C 26-jan-1981 PAK, Replaced call to LUNGET by call to LIB$GET_LUN
C 28-apr-1981 PAK, Combined CIOAIF, CIOAIC.
C FUNCTIONAL DESCRIPTION:
C	A new lun is obtained by a call to CIOLUNGET and the
C	specified file is opened for the new lun.
C	The logical unit nr (lun) of the current CIO input device
C	is pushed on an internal stack (if the stack is full
C	alternate return 1 is taken), then input is switched to
C	the new lun.
C	CIO now proceeds reading input from the alternate lun until
C	end-of-file is encountered.
C	Input is then switched back to the lun most recently pushed
C	on the stack by a call to CIOAIC.
C CALLING SEQUENCE:
C	CALL CIOAIF (FILALT,*,*)
C INPUT PARAMETERS:
C	FILALT	the filespecification for the file from which CIO
C	should read input until end-of-file is detected.
C ALTERNATE RETURNS:
C	1	no more room on the stack (i.e. alternate file
C		nesting too deep) or process wide LUN pool exhausted.
C	2	Open failure on alternate file.
C--

	INCLUDE 'ciocom.inc'
	CHARACTER	FILALT*(*)
	INTEGER		IST
	INTEGER		LUN

C Allocate a logical unit number.
C If no more available take error return.
	CALL CIOLUNGET (LUN, IST) !???g77
	IF (IST.ne.1) GOTO 810	!???g77 status test

C Open the specified file on the allocated unit number.
C If file-open-error take alternate return.

C???g77	OPEN (UNIT=LUN, NAME=FILALT, TYPE='OLD', READONLY, SHARED ,ERR=900)

C Switch further CIO input to the new logical unit nr.
C The negative LUN indicates that we allocated it through LIB$GET_LUN.
C If the stack is full, invalidate the input buffer and
C close all open alternate files, then take alternate return.

	IF (LUNSTP.GE.LSTKSZ) GOTO 800

C Push current lun on the stack, including its sign which indicates
C whether this lun was allocated by CIOLUNGET (sign negative) or not.

	LUNSTP = LUNSTP + 1
	LUNSTK(LUNSTP) = LUNIS

C Then make the new /lun/ the current one.
C Also save the new negated LUN in LUNIS.

	LUNI = LUN
	LUNIS =  - LUN
	RETURN

C Too deep nesting of alternate files.
C We must close the file we just opened.
  800	CLOSE (UNIT=LUN)

C Process wide LUN pool exhausted.
  810	RETURN 1

C OPEN of alternate file fails.
 900	RETURN 2
	
	ENTRY CIOAIC (*)
C++
C ENTRY:  CIOAIC
C ABSTRACT: Close currently open alternate file (if one is open)
C	and let input proceed from previous level file.
C FUNCTIONAL DESCRIPTION:
C	Close the currently open alternate input file.
C	Then deallocate the lun for the file and proceed input from
C	the lun most recently pushed on the lun-stack.
C CALLING SEQUENCE:  (only for internal use by CIO)
C	CALL CIOAIC (*)
C ALTERNATE RETURNS:
C	1	Current file is the zero level input file.
C--

C If no more alternate files open (stack empty) take alternate return
	IF (LUNSTP.EQ.0) RETURN 1

C Close alternate file. Deallocate LUN if allocated by CIOLUNFREE
	CLOSE (UNIT=LUNI)
	IF (LUNIS.LT.0) THEN
	  CALL CIOLUNFREE (-LUNIS, IST)	!???g77
	  IF (IST.ne.1) STOP 1	!???g77 status test
	ENDIF

C Pop previous LUN from the stack.
	LUNIS = LUNSTK(LUNSTP)
	LUNI = IABS (LUNIS)
	LUNSTP = LUNSTP - 1

	RETURN
	END
