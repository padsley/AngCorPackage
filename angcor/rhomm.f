	SUBROUTINE RHOMM
	IMPLICIT REAL*8(A-H,O-Z)

C  CALCULATES STATISTICAL TENSORS FOR VARIOUS M-STATE POPULATIONS
C  OF INTERMEDIATE STATE B.
c Nov-2000 M.Yosoi   Rev. for IBM-xlf

	INCLUDE 'ANGCOR.INC'
	COMPLEX*16 FP,X,X1

	WRITE(LUNOUT,100)
	MB=-JB(1)
	DO 1 M=1,LF2(1)
	XMB=FLOAT(MB)/2.
	MB1=-JB(1)
	DO 2 M1=1,LF2(1)
	XMB1=FLOAT(MB1)/2.
	FPP(M,M1)=(0.,0.)
	MA=-JA
	DO 3 MM=1,LF1
	XMA=FLOAT(MA)/2.
	MJ=MB-MA
	MJ1=MB1-MA
	XMJ=FLOAT(MJ)/2.
	XMJ1=FLOAT(MJ1)/2.
	IMSB=-ISB
	DO 4 MS=1,JS
	IMSA=-ISA
	DO 5 MR=1,JR
	MI=IABS(MJ)/2+1
	IND=0
	X=(0.,0.)
	X1=(0.,0.)
	IF(ICO(5).EQ.1) THEN
	  JIND=(MM-1)*M+M
	  X=SUM(MS,MR,JIND)
	ELSE
	  DO 6 LSJ=1,NLTR
	  IF(MI.GT.JTR(LSJ)/2+1)GO TO 7
	  XJ=FLOAT(JTR(LSJ))/2.
	  IND1=(MJ+JTR(LSJ))/2+1
	  FP=SUM(MS,MR,IND+IND1)*SQRT(DFLOAT(JTR(LSJ)+1))
	  X=X+CG(XJA,XMA,XJ,XMJ,XJB(1),XMB)*FP
7	  IND=IND+JTR(LSJ)+1
6	  CONTINUE
	ENDIF
	MF=IABS(MJ1)/2+1
	IND=0
	IF(ICO(5).EQ.1) THEN
	  JIND1=(MM-1)*M1+M1
	  X1=SUM(MS,MR,JIND1)
	ELSE
	  DO 8 LSJ1=1,NLTR
	  IF(MF.GT.JTR(LSJ1)/2+1)GO TO 9
	  XJ1=FLOAT(JTR(LSJ1))/2.
	  IND1=(MJ1+JTR(LSJ1))/2+1
	  FP=DCMPLX(DREAL(SUM(MS,MR,IND+IND1)),-DIMAG(SUM(MS,MR,IND
     1  +IND1)))
	  FP=FP*SQRT(DFLOAT(JTR(LSJ1)+1))
	  X1=X1+CG(XJA,XMA,XJ1,XMJ1,XJB(1),XMB1)*FP
9	  IND=IND+JTR(LSJ1)+1
8	  CONTINUE
	ENDIF
	FPP(M,M1)=FPP(M,M1)+X*X1
	IMSA=IMSA+2
5	CONTINUE
	IMSB=IMSB+2
4	CONTINUE
	MA=MA+2
3	CONTINUE
	MB1=MB1+2
2	CONTINUE
	MB=MB+2
1	CONTINUE
	RETURN
100	FORMAT(1H0,' RHOMM  ENTERED')
	END
