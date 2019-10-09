c	PROGRAM MAIN
c
C	Minimizes the decay coefficients in ANGCOR
c
c	CALL MINNEW
c	END

	SUBROUTINE FCN(NPAR,G,CHISQ,HPAR,IFLAG)
	IMPLICIT REAL*8 (A-H,O-Z)

	PARAMETER (MINU_PAR = 30)	!Number of minuit parameters
	COMMON /PAREXT/ U(MINU_PAR),XNAM(MINU_PAR),WERR(MINU_PAR),MAXEXT,NU
	COMMON /ANG/ INDANG,ITIME,CHI,PAR(MINU_PAR),ERR(MINU_PAR)
	DIMENSION G(MINU_PAR),HPAR(MINU_PAR)

	INDANG = 0

C	WRITE(*,*)"fcnang"
	
	GO TO (10,20,30,40,50), IFLAG

C	IFLAG=1	Initialize.
C	     =2 Gradient.
C	     =3 Output.
C	     =4 Minimize.
C	     =5 Reserved.

C	First call from MINUIT.
10	CONTINUE

C	ITIME	Number of iterations.
	ITIME =0
	DO 11 I=1,MINU_PAR
11	ERR(I)=WERR(I)

12	ITIME=ITIME+1
	IF (ITIME.EQ.2.OR.ITIME.EQ.3) THEN
	  CHISQ=CHI
	  GO TO 20
	ENDIF

	DO 13 I=1,NU
13	PAR(I)=HPAR(I)
	IF (ITIME.EQ.1) THEN
	  CALL ANGCORM
	ELSE
	  CALL AGCRHLP
	ENDIF
	CHISQ=CHI
	WRITE(16,14) ITIME,CHI,(HPAR(KK),KK=1,NU)
14	FORMAT(1X,'Iteration No.',I3,3X,'CHI2',E11.4,3X,'Params.',7F10.4)
20	RETURN

C	Last call.
30	GO TO 12

C	Ordinary call.
40	GO TO 12

50	RETURN

	END
