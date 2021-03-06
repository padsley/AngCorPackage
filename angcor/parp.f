	SUBROUTINE PARP(ANGP,PHI,LMAX,T,SIGMA)
	IMPLICIT REAL*8(A-H,O-Z)

	INCLUDE 'ANGCOR.INC'
	COMPLEX*16 FP,AI,AF,X
	REAL TI(INTER_CH),SQRS(INTER_CH-1)
	COMMON /INTCRO/  NCH, TI, SQRS


C  THE FOLLOWING STATEMENTS HAVE BEEN ADDED TO ENSURE NORMALIZATION OF
C  THE ANGULAR DISTRIBUTION TO UNITY
c Nov-2000 M.Yosoi   Rev. for IBM-xlf

	DTEMP=0
	IF (NCH.EQ.1) THEN
	  TI(1)=T
	  DTEMP=TI(1)
	ELSE
	  DO N=1,NCH
	    DTEMP=DTEMP+TI(N)
	  ENDDO
	ENDIF

	SIG1=0.
	CALL PLM(ANGP,LMAX,LMAX)
	MA=-JA
	DO 1 M1=1,LF1
	XMA=FLOAT(MA)/2.
	IMSB=-ISB
	DO 2 MS=1,JS
	IMSA=-ISA
	DO 3 MR=1,JR
	IMSC=-ISC
	DO 4 MSC=1,NSD
	XMS=FLOAT(IMSC)/2.
	MC=-JC
	DO 5 M3=1,LF3
	XMC=FLOAT(MC)/2.
	X=(0.,0.)
	MT=0
	DO 6 NNCH=1,NCH
	MB=-JB(NNCH)
	DO 7 M21=1,LF2(NNCH)
	XMB=FLOAT(MB)/2.
	MJ=MB-MA
	XMJ=XMB-XMA
	MX=IABS(MJ)/2+1
	IND=0
	AI=(0.,0.)
	IF(ICO(5).EQ.1) THEN
	  JIND=(M1-1)*M21+M21+MT
	  AI=SUM(MS,MR,JIND)
	ELSE
	  DO 8 LSJ=1,NLTR
	  IF(MX.GT.JTR(LSJ)/2+1)GO TO 9
	  IF(ICO(4).EQ.0)GO TO 10
	  IF(NNCH.NE.LSJ)GO TO 9
10	  XJ=FLOAT(JTR(LSJ))/2.
	  IND1=(MJ+JTR(LSJ))/2+1
	  FP=SUM(MS,MR,IND+IND1)
	  IF (NNCH.GT.1) FP=SQRS(NNCH-1)*FP
	  AI=AI+CG(XJA,XMA,XJ,XMJ,XJB(NNCH),XMB)*FP
     1        *SQRT(DFLOAT(JTR(LSJ)+1))
9	  IND=IND+JTR(LSJ)+1
8	  CONTINUE
	ENDIF
	CALL PARDEC(XMB,XMC,XMS,NNCH,PHI,AF)
	X=X+AI*AF
	MB=MB+2
7	CONTINUE
	MT=MT+LF1*LF2(NNCH)
6	CONTINUE
	SIG1=SIG1+DREAL(X)**2+DIMAG(X)**2
	MC=MC+2
5	CONTINUE
	IMSC=IMSC+2
4	CONTINUE
	IMSA=IMSA+2
3	CONTINUE
	IMSB=IMSB+2
2	CONTINUE
	MA=MA+2
1	CONTINUE
	SIGMA = SIG1/DTEMP
	RETURN
	END
