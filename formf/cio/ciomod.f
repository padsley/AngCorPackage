	SUBROUTINE CIOMOD
C++
C TITLE:  CIOMOD
C FACILITY:  CIO  - conversational I/O package.
C ABSTRACT: Determines whether the devices assigned to SYS$INPUT
C 	and SYS$OUTPUT are interactive terminals or not.
C ENVIRONMENT: VAX/VMS specific. Calls VMS system services.
C PROCEDURES CALLED:
C	SYS$GETDVIW
C AUTHOR: Peter A. Kroon, KVI, Groningen, NL.
C CREATION DATE:  21-oct-1980.
C MODIFIED BY:
C PAK  3-jan-1983 Changed EXTERNAL's for SSDEF and IODEF to INCLUDES.
C PAK 22-feb-1984 Changed to use $GETDVI.
C FUNCTIONAL DESCRIPTION:
C	The characteristics of SYS$INPUT and SYS$OUTPUT are obtained
C	by calling SYS$GETDVIW and MODI and MODO are set accordingly.
C CALLING SEQUENCE:
C	CALL CIOMOD
C OUTPUT PARAMETERS:
C	MODI	type of device assigned to SYS$INPUT / STDIN
C	MODE	type of device assigned to SYS$OUTPUT / STDOUT
C	One of the follwing values is returned: 0 if the device is
C	a terminal, 1 if the device is not a terminal.
C--


	INCLUDE		'ciocom.inc'

	INTRINSIC	IsaTty

	if (IsaTty (LUNIDF)) then
		MODI = 0
	else
		MODI = 1
	endif

	if (IsaTty (LUNODF)) then
		MODO = 0
	else
		MODO = 1
	endif

C	WRITE (*,*) 'MODI=',MODI,' MODO=',MODO
	RETURN
	END
