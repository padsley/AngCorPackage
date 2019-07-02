	SUBROUTINE DJMK(ANGP,PHI,PSI,IJ,IK,DJ)
	IMPLICIT REAL*8(A-H,O-Z)
c Nov-2000 M.Yosoi   Rev. for IBM-xlf

	PARAMETER (MAX_SUBST = 21)	!Number of populated substates; maximum
					!of JB(i)+1 or JTRMAX+1 (JTRMAX is
					!twice maximum transferred angular
					!momentum)
	COMPLEX*16 DJ(MAX_SUBST)

	IF(IJ.EQ.0) THEN
	  DJ(1)=(1.,0.)
	  RETURN
	ENDIF
	THETA=ANGP/114.591558
	Y=COS(THETA)
	Z=SIN(THETA)
	PSIT=-IK*PSI/2.
	JPK=IJ+IK
	JMK=IJ-IK
	LF=IJ+1
	M=-IJ
	DO 1 M21=1,LF
	XD=0.
	IF(ANGP.EQ.0.) THEN
	  IF(M.EQ.IK) XD=1.
	  GO TO 2
	ELSEIF(ANGP.EQ.180.) THEN
	  IF(M.EQ.-IK) XD=PAR((IJ-M)/2)
	  GO TO 2
	ENDIF
	PHIT=-M*PHI/2.
	JPM=IJ+M
	JMM=IJ-M
	IS2=MIN0(IJ-M,IJ+IK)
	IS1=MAX0(0,IK-M)
	IS1=IS1+1
	IS2=IS2+1
	DO 3 IS=IS1,IS2,2
	PHAS=PAR((IS-1)/2)
	I1=JPK-IS+1
	I2=JMM-IS+1
	T=BINOM(JMM/2,(IS-1)/2)*BINOM(JPK/2,(IS-1)/2)
     1  *BINOM(JPM/2,I1/2)*BINOM(JMK/2,I2/2)
	T=PHAS*SQRT(T)
	IT1=(IS-1)+(M-IK)/2
	T=T*(Y**(IJ-IT1))*(Z**IT1)
	XD=XD+T
3	CONTINUE
2	DJ(M21)=XD*DCMPLX(COS(PHIT),SIN(PHIT))*
     1  DCMPLX(COS(PSIT),SIN(PSIT))
	DJ(M21)=DJ(M21)*PAR((IK-M)/2)
	M=M+2
1	CONTINUE
	RETURN
	END
