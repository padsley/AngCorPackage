	SUBROUTINE CIOHLP
C++
C TITLE:  CIOHLP
C FACILITY:  CIO  - conversational I/O package.
C ABSTRACT: Procedure to give on-line help, using the
C		VMS help library facility.
C ENVIRONMENT:  FORTRAN-77. VMS V3.0 or higher
C PROCEDURES CALLED:
C	LBR$OUTPUT_HELP, LIB__GET_INPUT, LIB__PUT_OUTPUT
C	Condition handler: CIOHLPHAN
C AUTHOR: Peter A. Kroon, KVI, Groningen, NL.
C CREATION DATE:  31-jan-1981.
C MODIFIED BY:
C PAK 16-mrt-1981 changed so that both a single help library as well as
C	a multi-module help library can be used.
C PAK 02-oct-1982 Use LBR$OUTPUT_HELP, available in VMS V3.0, which
C	makes this routine much easier.
C PAK 28-dec-1982 Increase size of LIBNAME to 80 char.
C FUNCTIONAL DESCRIPTION:
C	Before the 1st call to CIOHLP, CIOHLPINI must be called with the
C	specification of the helplibrary to be consulted, and, if more
C	programs share the same helplibrary, the name of the module in the
C	library containing the required help (usually the program name).
C ENTRIES: CIOHLP, CIOHLPINI
C CALLING SEQUENCE:
C	CALL CIOHLP
C	  to get on-line help.
C	CALL CIOHLPINI (library-filespecification, module_name)
C	  to specify the library and optionally the module to be consulted.
C	  If the library is a single program help library, the second argument
C	  must be a blank string.
C	  Note: If CIOHLPINI is not called, the default VMS helplibrary
C	  will be consulted.
C--

C Not implemented for this non-VMS version but their entries are
C preserved in order not to break existing code.

	INCLUDE 'ciocom.inc'
	CHARACTER*(*)	LIB, MOD		! Arguments for CIOHLPINI

C Just purges the CIO internal buffers and outputs a rudimentary
C message.

	CALL CIOPUR				! Purge CIO's internal buffer

	WRITE (LUNO,9100)
 9100	FORMAT(1X,T56,'...HELP function is not implemented...',/)

	RETURN

C -------------------------------------------------------------------
	ENTRY CIOHLPINI (LIB, MOD)
C -------------------------------------------------------------------

	END
