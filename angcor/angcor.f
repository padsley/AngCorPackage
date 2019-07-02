	PROGRAM ANGCOR
	IMPLICIT REAL*8(A-H,O-Z)
C++
C	Main program calling subroutine ANGCORM, which contains the original
C	program ANGCOR. This allows the subroutine ANGCORM to be used for
C	search on a number of parameters in a chi**2 minimization procedure
C	using MINUIT.
C--

C	INDANG is a variable that is used in ANGCORM to indicate that no
C	minimization using the CERN MINUIT package is done.

	PARAMETER (MINU_PAR = 30)
	COMMON /ANG/INDANG, ITIME, CHI, PARA(MINU_PAR), ERR(MINU_PAR)

	INDANG = 1
	CALL ANGCORM

	STOP
	END
