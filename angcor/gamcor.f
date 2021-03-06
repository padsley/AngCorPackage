	SUBROUTINE GAMCOR(ANG,PHI)
	IMPLICIT REAL*8(A-H,O-Z)

C  CALCULATES PARTICLE-GAMMA ANGULAR CORRELATIONS
C  CONVENTIONS OF:1)RYBICKI ET.AL.,NUCL.PHYS.A146(1970)659
C		  2)ROSE AND BRINK,REV.OF MODERN PHYS. 39(1967)306
C  ARE FOLLOWED FOR THIS AND CONNECTED SUBROUTINES
c Nov-2000 M.Yosoi   Rev. for IBM-xlf

	INCLUDE 'ANGCOR.INC'
	COMMON /CASCADE/ LBC, LCD, JD2, LDE, JE2, NC, ARCDEL, ARCDELC,
     1 	ARCDELD
	REAL AN1(MAX_EXP),PH1(MAX_EXP),C1(MAX_EXP),EC1(MAX_EXP),
     1  TH1(MAX_EXP)
	COMMON /CROSS/   N1, AN1, PH1, C1,EC1, TH1
	COMMON /EXTRA/   ZW(MAX_LG), RKK(MAX_LG)
	DIMENSION RR(3,2*MAX_LG), UKB(MAX_LDEC,MAX_LG), RRC(3,2*MAX_LG),
     1  UKC1(MAX_LG), UKC2(MAX_LG), RRD(3,2*MAX_LG)
	DIMENSION PH2(MAX_EXP), TH2(MAX_EXP)	

	WRITE(LUNOUT,100)
	IF(ICO(1).EQ.0) THEN
	  KB = 2*LBC + 3
	  XLBC = FLOAT(LBC)
	  KL=MIN0(LF2(1),KB)
	ENDIF
	IF (NC.EQ.1) GO TO 1
	KC = 2*LCD + 3
	XLCD = FLOAT(LCD)
	XJD = FLOAT(JD2)/2.0
	KL=MIN0(LF2(1),LF3,KC)
	IF (NC.EQ.2) GO TO 1
	KD = 2*LDE + 3
	XLDE = FLOAT(LDE)
	XJE = FLOAT(JE2)/2.0
	KL=MIN0(LF2(1),LF3,JD2+1,KD)
1	CONTINUE
	KM = KL - 1
	CALL RHOKQ(KL)
	DO 2 K= 2, KM, 2
	XK = FLOAT(K)
	IF(ICO(1).EQ.0) THEN
	   RR(1,K) = RK(XLBC   ,XLBC   ,XJB(1),XJC,XK)
	   RR(2,K) = RK(XLBC   ,XLBC+1.,XJB(1),XJC,XK)
	   RR(3,K) = RK(XLBC+1.,XLBC+1.,XJB(1),XJC,XK)
	   IF(NC.EQ.1) GO TO 2
	   GO TO 3
	ELSE
	  DO 4 J=1,NLDEC(1)
	  UKB(J,K/2)=UK(XK,XJB(1),XJC,DFLOAT(JDEC(1,J))/2.)
4	  CONTINUE
	  GO TO 5
	ENDIF
3	UKB(1,K/2) =UK(XK,XJB(1),XJC,XLBC)
	UKB(2,K/2) = UK(XK,XJB(1),XJC,XLBC+1.)
5	RRC(1,K) = RK(XLCD   ,XLCD   ,XJC,XJD,XK)
	RRC(2,K) = RK(XLCD   ,XLCD+1.,XJC,XJD,XK)
	RRC(3,K) = RK(XLCD+1.,XLCD+1.,XJC,XJD,XK)
	IF(NC .EQ.2) GO TO 2
	UKC1(K/2) = UK(XK,XJC,XJD,XLCD)
	UKC2(K/2) = UK(XK,XJC,XJD,XLCD+1.)
	RRD(1,K) = RK(XLDE   ,XLDE   ,XJD,XJE,XK)
	RRD(2,K) = RK(XLDE   ,XLDE+1.,XJD,XJE,XK)
	RRD(3,K) = RK(XLDE+1.,XLDE+1.,XJD,XJE,XK)
2	CONTINUE
	T=RHO(1)
	V=57.29578
	IF(ICO(1).EQ.0) THEN
	  Z1 = ARCDEL/V
	  DEL = SIN(Z1)/COS(Z1)
	  D2=DEL*DEL
	ENDIF
	IF(NC.GT.1) THEN
	  Z1 = ARCDELC/V
	  DELC = SIN(Z1)/COS(Z1)
	  D2C = DELC*DELC
	  IF(NC.GT.2) THEN
	    Z1 = ARCDELD/V
	    DELD = SIN(Z1)/COS(Z1)
	    D2D = DELD*DELD
	  ENDIF
	ENDIF
	IF(ICO(8).EQ.0) THEN
	  IF(ICO(1).EQ.2) THEN
	    WRITE(LUNOUT,101)DELTA(1,1),DELTA(1,2),DELTA(1,3)
	  ELSE
	    WRITE(LUNOUT,102) DEL,ARCDEL
	  ENDIF
	  IF(NC.GT.1) THEN
	    WRITE(LUNOUT,103) DELC,ARCDELC
	    IF(NC.GT.2) WRITE(LUNOUT,104) DELD,ARCDELD
	  ENDIF
	ENDIF
	DO 6 K=2,KM,2
	IF(ICO(1).EQ.0) THEN
	   RKK(K/2)=(RR(1,K) + 2.0*DEL*RR(2,K) + D2*RR(3,K)) / (1.+D2)
	  IF(NC.EQ.1) GO TO 7
	  UBC =(UKB(1,K/2) + D2 * UKB(2,K/2))/( 1. + D2 )
	ELSE
	  UBC=0.
	  TOTD=0.
	  DO 8 J=1,NLDEC(1)
	  D2=DELTA(1,J)*DELTA(1,J)
	  TOTD=TOTD+D2
	  UBC=UBC+UKB(J,K/2)*D2
8	  CONTINUE
	  UBC=UBC/TOTD
	ENDIF
	RKK(K/2)=UBC*(RRC(1,K)+2.0*DELC*RRC(2,K)+D2C*RRC(3,K))/(1.+D2C)
	IF(NC.EQ.2) GO TO 7
	UCD =  (UKC1(K/2) + D2C * UKC2(K/2)) / (1. + D2C)
	RKK(K/2)=UBC*UCD*(RRD(1,K)+2.0*DELD*RRD(2,K)+D2D*RRD(3,K))/
     1  (1.+D2D)
 7	CONTINUE
6	CONTINUE
	IF (N1 .LE. 0 ) GO TO 9
	DO 10 I=1,N1
	PH2(I) = PH1(I)/57.295779
	CALL SIGMA( AN1(I), PH2(I), KL,TH2(I) )
	TH1(I) = TH2(I)/T
10	CONTINUE
	IF(ICO(8).EQ.1) THEN
	  WRITE(LUNOUT,105) DEL
	  IF(NC.GT.1) THEN
	    WRITE(LUNOUT,106) DELC
	    IF(NC.EQ.3) WRITE(LUNOUT,107) DELD
	  ENDIF
	ENDIF
9	CONTINUE
	IF(ICO(8).EQ.0) THEN
	  ANGP=-DANGP
	  DO 11 NP=1,NANGP
	  ANGP=ANGP+DANGP
	  CALL SIGMA(ANGP,PHI,KL,SIG1)
	  SIG(NP)=SIG1/T
	  XJB1=FLOAT(LF2(1))
	  SQ = SQRT(XJB1)*RHO(1)
11	  CONTINUE
	  WRITE(LUNOUT,108) ANG,SQ
	ENDIF
	RETURN
100	FORMAT(1H0,' GAMCOR ENTERED')
101	FORMAT(1H0,' DELTA(1) =',F9.4,'   DELTA(2) =',F9.4,
     1  '   DELTA(3) =',F9.4)
102	FORMAT(1H0,' DELTA  =',F8.3,'  ARCTAN =',F7.1)
103	FORMAT(1H ,' DELTAC =',F8.3,'  ARCTAN =',F7.1)
104	FORMAT(1H ,' DELTAD =',F8.3,'  ARCTAN =',F7.1)
105	FORMAT(1H ,' DELTA  =',F8.3)
106	FORMAT(1H+,21X,'DELTAC =',F8.3)
107	FORMAT(1H+,41X,'DELTAD =',F8.3)
108	FORMAT(' PROJ. SCATT. ANG.= ',F5.1,'  TOTAL-SIGMA=',E12.4)
	END












