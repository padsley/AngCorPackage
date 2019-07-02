	SUBROUTINE CIO1ST
C++
C TITLE:  CIO1ST
C FACILITY:  CIO  - conversational I/O package.
C ABSTRACT: Once-only initialization code for CIO package.
C	Also contains initialization data statement for common CIO003.
C ENVIRONMENT:  FORTRAN-77.
C PROCEDURES CALLED:
C	CIOMOD	to obtain running mode (interactive or batch)
C	CIOINV	to invalidate input buffer contents.
C	CIOQPU	to purge the qualifier list.
C AUTHOR: Peter A. Kroon, KVI, Groningen, NL.
C CREATION DATE:  21-oct-1980.
C MODIFIED BY:
C FUNCTIONAL DESCRIPTION:
C	Performs initialization of various parameters and switches
C	in the CIO package. Then makes itself in active on further
C	calls.
C CALLING SEQUENCE:  (only for internal use by CIO)
C	CALL CIO1ST
C IMPLICIT OUTPUTS: Various parameters in CIO common blocks.
C--

	INCLUDE 'ciocom.inc'
	INCLUDE 'cio003ini.inc'
	LOGICAL*1	FIRST
	DATA		FIRST /.TRUE./
	INTEGER		K

C Lockout this code after the first time.
	IF (.NOT.FIRST) RETURN
	FIRST = .FALSE.

C Get device types for zero level input and output devices.
	CALL CIOMOD (MODI,MODO)

C Initialize the LUN pool so that all LUNs are available.

C      write (*,*) '?????? cio1st lunpool init'
	DO K = LUNPOOLMIN, LUNPOOLMAX
		LUNPOOL (K) = .FALSE.
	END DO

C Set input and output luns to their defaults and clear lun stack
C for alternate input devices.
	LUNI = LUNIDF
	LUNIS = LUNIDF
	LUNO = LUNODF
	LUNSTP = 0

C Reset promptbuffer pointer and clear lun for prompt-scratch-file
C indicating no file open.
	IPBPTR = 0
	LUNPRF = 0

C Invalidate inputbuffer and alternate files.
C      write (*,*) '?????? cio1st calls cioinv'
	CALL CIOINV

C Purge qualifier list
C      write (*,*) '?????? cio1st calls cioqpu'
	CALL CIOQPU

	RETURN
	END
