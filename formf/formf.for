	PROGRAM FORMF
	IMPLICIT REAL*8(A-H,O-Z)
C  
C	This program calculates the form factors for isoscalar dipole and
C	monopole transitions according to the various prescriptions of
C	Harakeh and Dieperink, Orlandini et al. and Satchler.
C	Written by M.N. Harakeh.
C  
C	COMMON R(3),AR(3),SR(3),L_MULT,AMASS,Z,A13,EPSILON
	COMMON AMASS,Z,A13,EPSILON,AR(3),R(3),SR(3),L_MULT
C	COMMON /DEF/ BETA(3),LDFRM(3),VBETA,GAMMA,BETACOR,XK
C	1           ,BETA0_DEF,BETA3_DEF
	COMMON /DEF/ VBETA,GAMMA,BETACOR,XK,BETA0_DEF,BETA3_DEF
	1           ,BETA(3),LDFRM(3)
C	Changed by JE 14.04.2010
	COMMON /INT/ DR,KR
	DIMENSION R2(3)
	CHARACTER DEF*1,KMD*2,TYPE(2)*7
	COMMON /DEFSR/ DEF
	DATA TYPE(1),TYPE(2) /'VOLUME ','SURFACE'/
	DATA LDFRM /2,4,6/
	DATA DEF /'N'/
	HBAR = 197.329
	AMU = 931.502
	PI = 3.1415927
	EPS = (4./65.+5./80.)*HBAR*HBAR/(3.*AMU)
	FACT = (3./4.)*(HBAR*HBAR/(AMU*41))**2
C  
C	Read input parameters.
C  
100     CALL CIOAX (' PR[oceed] or EX[it]:',KMD,'C',' ')
	IF(KMD.EQ.'EX')STOP
    	CALL CIOF  (' MASS OF THE NUCLEUS :',AMASS)
	CALL CIOWRT(' WOODS-SAXON POTENTIAL;  1=VOLUME   :')
	CALL CIOI  ('                         2=SURFACE  :',INX)
	WRITE(6,1)
1	FORMAT('  DEPTH RADIUS AND DIFFUSENESS OF THE WOODS-SAXON
	1OPTICAL MODEL POTENTIAL')
	CALL CIOF  (' V or W:',V)
	CALL CIOF  (' RR/(A**1/3):',RR)
	CALL CIOF  (' AR:',ARR)
	CALL CIOF  (' MAXIMUM INTEGRATION RADIUS :',RMAX)
	CALL CIOF  (' INTEGRATION STEP :',DR)
C  
	KR=NINT(RMAX/DR)
	A13=AMASS**0.33333333
	EPSILON=EPS/(A13*A13) 
	R(2)=RR*A13
	AR(2)=ARR
	VR=V/R(2)
	VA=V/AR(2)
	VRA=VR/AR(2)
C  
	WRITE(6,2)AMASS
	WRITE(9,2)AMASS
2	FORMAT('1','                 NUCLEUS',
	1         /'              MASS :',F8.1)
	WRITE(6,3)TYPE(INX),V,RR,ARR,RMAX,DR
	WRITE(9,3)TYPE(INX),V,RR,ARR,RMAX,DR
3	FORMAT(//1X,A7,' WOODS-SAXON POTENTIAL PARAMETERS',
	1     /' DEPTH V :',F9.4,' MeV'
	2     /' RR/(A**1/3) :',F9.4,' Fm,   AR :',F6.4,' Fm'
	3     /' MAX. INTEG. RADIUS :',4X,F8.3,'   INTEG. STEP :',F5.3)
	IF(INX.EQ.1) THEN
	  CALL RFERMI(R(2),AR(2),2,R2(2))
	ELSE
	  CALL RFERMI(R(2),AR(2),1,R2_1)
	  CALL RFERMI(R(2),AR(2),-1 ,R_1)
	  R2(2)=2*R2_1/R_1
	ENDIF
C  
C	Write out form factor of Harakeh and Dieperink for isoscalar dipole
C	transition.
C  
	FX=6.
	IF(INX.EQ.2) FX=7.
	WRITE(6,4)
	WRITE(9,4)
4	FORMAT(/1X,' Harakeh-Dieperink dipole form factor for DWUCK
	1 or CHUCK'/)
	PWR=1.
	VTEMP=-VR*10
	WRITE(6,5)FX,PWR,VTEMP
	WRITE(9,5)FX,PWR,VTEMP
5	FORMAT('  FX =',F4.1,'  PWR =',F4.1,'   V =',F13.5)
	PWR=0.
	FX=FX+1.
	VTEMP=VRA*((5./3.)*R2(2)-4.*EPSILON)
	WRITE(6,5)FX,PWR,VTEMP
	WRITE(9,5)FX,PWR,VTEMP
	PWR=2.
	VTEMP=-VRA*3.
	WRITE(6,5)FX,PWR,VTEMP
	WRITE(9,5)FX,PWR,VTEMP
	PWR=1.
	FX=FX+1.
	VTEMP=-VRA*EPSILON/ARR
	WRITE(6,5)FX,PWR,VTEMP
	WRITE(9,5)FX,PWR,VTEMP
C  
C	Write out form factor of Orlandini et al. for isoscalar dipole
C	transition.
C  
	FX=6.
	IF(INX.EQ.2) FX=7.
	WRITE(6,6)
	WRITE(9,6)
6	FORMAT(/1X,' Orlandini et al. dipole form factor for DWUCK
	1 or CHUCK'/)
	PWR=1.
	VTEMP=VR*5
	WRITE(6,5)FX,PWR,VTEMP
	WRITE(9,5)FX,PWR,VTEMP
	PWR=0.
	FX=FX+1.
	VTEMP=VRA*(20./3.)*R2(2)
	WRITE(6,5)FX,PWR,VTEMP
	WRITE(9,5)FX,PWR,VTEMP
	PWR=2.
	VTEMP=-VRA*3.
	WRITE(6,5)FX,PWR,VTEMP
	WRITE(9,5)FX,PWR,VTEMP
	FACTOR=FACT*A13*A13
	PWR=-2.
	VTEMP=VRA*2*FACTOR
	WRITE(6,5)FX,PWR,VTEMP
	WRITE(9,5)FX,PWR,VTEMP
	PWR=-1.
	FX=FX+1.
	VTEMP=-VTEMP/ARR
	WRITE(6,5)FX,PWR,VTEMP
	WRITE(9,5)FX,PWR,VTEMP
C  
C	Write out form factor of Satchler (Version I) for isoscalar monopole
C	transition.
C  
	FX=6.
	IF(INX.EQ.2) FX=7.
	WRITE(6,7)
	WRITE(9,7)
7	FORMAT(/1X,' Satchler (Version I) monopole form factor for DWUCK
	1 or CHUCK'/)
	PWR=0.
	VTEMP=-V*3
	WRITE(6,5)FX,PWR,VTEMP
	WRITE(9,5)FX,PWR,VTEMP
	PWR=1.
	FX=FX+1.
	VTEMP=-VA
	WRITE(6,5)FX,PWR,VTEMP
	WRITE(9,5)FX,PWR,VTEMP
C  
C	Write out form factor of Satchler (Version II) for isoscalar monopole
C	transition.
C  
	FX=6.
	IF(INX.EQ.2) FX=7.
	WRITE(6,8)
	WRITE(9,8)
8	FORMAT(/1X,' Satchler (Version II) monopole form factor for DWUCK
	1 or CHUCK'/)
	PWR=0.
	X=(PI*AR(2)/R(2))**2
	X=(3+X)/(1+X)
	VTEMP=-V*X
	WRITE(6,5)FX,PWR,VTEMP
	WRITE(9,5)FX,PWR,VTEMP
	FX=FX+1.
	VTEMP=-VA*R(2)
	WRITE(6,5)FX,PWR,VTEMP
	WRITE(9,5)FX,PWR,VTEMP
        GO TO 100
	END
