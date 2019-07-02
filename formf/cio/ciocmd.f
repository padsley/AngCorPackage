	SUBROUTINE CIOCMD (PROMPT,KEYTBL,NKEYS,KEYVAL)
C++
C TITLE:  CIOCMD
C FACILITY:  CIO  - conversational I/O package.
C ABSTRACT: Prompt for and read commandline, extract keyword and decode.
C ENVIRONMENT:  FORTRAN-77.
C PROCEDURES CALLED:
C	CIOCMX, CIOERL
C AUTHOR: Peter A. Kroon, KVI, Groningen, NL.
C CREATION DATE:  21-oct-1980.
C MODIFIED BY:
C PAK 10-apr-1981 Moved most of the job to CIOCMX.
C FUNCTIONAL DESCRIPTION:
C	Calls CIOCMX to prompt for and accept a command.
C	Procedure repeats in case of erroneous input.
C CALLING SEQUENCE:
C	CALL CIOCMD (PROMPT,KEYTBL,NKEYS,KEYVAL)
C INPUT PARAMETERS:
C	PROMPT	prompt string to be displayed.
C	KEYTBL	character array containing the command repertoire.
C	NKEYS	nr of commands in KEYTBL.
C OUTPUT PARAMETERS:
C	KEYVAL	entry nr of the command in KEYTBL.
C--

	CHARACTER*(*)	PROMPT,KEYTBL(1)

C Let CIOCMX do all the work. Request purge internal inputbuffer.
C Note that the qualifier definition list is not purged since we may
C wish to define qualifiers before a command is prompted for.

    5	CALL CIOCMX (PROMPT,KEYTBL,NKEYS,KEYVAL,'P')
	IF (KEYVAL) 10,20,30
   10	CALL CIOERL ('unknown command')
	GOTO 5
   20	CALL CIOERL ('ambiguous command')
	GOTO 5
   30	RETURN		! Ok. return.
	END
