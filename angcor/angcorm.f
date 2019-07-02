C	PROGRAM ANGCOR

	SUBROUTINE ANGCORM

	IMPLICIT REAL*8(A-H,O-Z)

C  Written by:  M.N. Harakeh and L.W. Put, Internal report KVI-67
C		Last modified 25 August 1985.
c Nov-2000 M.Yosoi   Rev. for IBM-xlf
c Sept-2009 R.G.T. Rev. for Linux 

C  Computes decay angular distributions from states excited by direct  
C  reactions. input amplitudes are read from DWUCK, CHUCK, or DWBA82
C  files.

C  DWUCK must be run with ICON(3) equal 0 or 1.

C  Reaction is defined by A(ma,mb)B* where B* decays into C+mc.

C  ICO(1)	.EQ.0  Particle-gamma angular correlations.
C		.EQ.1  Particle-particle angular correlations.
C		.EQ.2  Particle-particle-gamma angular correlations.
C		       The intermediate particle decay is not observed.
C		.EQ.3  Particle-fission angular correlation.

C  ICO(2)	.EQ.0  Do not print information from subroutines RHOKQ and
C		       RHOLSJ.
C		.EQ.1  Print.

C  ICO(3)	.EQ.0  Calculate RHOKQ from RHOMM.
C		.EQ.1  Calculate RHOKQ from RHOLSJ.

C  ICO(4)	.EQ.0  Only one intermediate state JB is excited.
C		.EQ.1  Two or more intermediate states JB are excited. this
C		       is only for particle decay. in this case only one JTR
C		       is allowed per  intermediate state.

C  ICO(5)	.EQ.0  M-state populations obtained from program DWUCK.
C		.EQ.1  M-state populations obtained from program CHUCK.
C		       In this case ICON(8) in  C H U C K  should be set
C		       equal to state number.
C		.EQ.2  M-state populations obtained from program DWBA82. In
C		       this case DWBA82 should be run once for every new
C		       ANGCOR calculation with a different scattering angle.

C  ICO(6)	.EQ.0  New DWUCK/CHUCK/DWBA82 information read in.
C		.EQ.1  Same DWUCK/CHUCK/DWBA82 information as in previous
C		       case. Card sets 3 and 4 should be deleted.

C  ICO(7)	.EQ.0  Do not print beta's/D-amplitudes/m-state population
C		       amplitude.
C		.EQ.1  Print.

C  ICO(8)	.EQ.0  Print angular correlation.
C		.EQ.1  Suppress printing angular correlation, only CHI2
C		       as function of delta will be printed.

C  ICO(9)	.EQ.0  Z-axis is the beam axis for cases where m-state
C		       population amplitudes are obtained from DWUCK or CHUCK
C		       or is the scattering axis if they are obtained from
C		       DWBA82.
C		.EQ.1  Z-axis is the recoil axis. In this case the recoil
C		       angle (RECANG) should be read on card 7.

C  NC		Number of cascades in case of gamma-ray decay.

C  M1		.GE.0  Number of experimental data.
C  		.LT.0  Same experimental data as in previous case,
C		       card set 9 should be deleted.

C  This program handles only the case where the spin transfer ISTR
C  is the same for all form factors in DWUCK.
C  Conventions of SATCHLER are followed. Nucl.Phys. 55(1964)1


	PARAMETER (MINU_PAR = 30)	!Number of minuit parameters
	INCLUDE 'ANGCOR.INC'
	COMMON /CASCADE/ LBC, LCD, JD2, LDE, JE2, NC, ARCDEL, ARCDELC,
     1 	ARCDELD
	REAL AN1(MAX_EXP),PH1(MAX_EXP),C1(MAX_EXP),EC1(MAX_EXP),
     1  TH1(MAX_EXP)
	COMMON /CROSS/   N1, AN1, PH1, C1,EC1, TH1
	COMMON /EXTRA/   ZW(MAX_LG), RKK(MAX_LG)
	COMMON /FISSION/ KA, KB1, XKA, XKB, XKJ
	REAL TI(INTER_CH),SQRS(INTER_CH-1)
	COMMON /INTCRO/  NCH, TI, SQRS
	REAL PARA(MINU_PAR), ERR(MINU_PAR)
	COMMON /ANG/     INDANG, ITIME, CHI, PARA, ERR
	COMPLEX*16 FL(MAX_DIM1,M_HALF), FLC(MAX_L*INTER_CH,MAX_DIM2), 
     1  F, SSQ,
     1  RSUM(MAX_MS,MAX_MS,M_HALF*MAX_LTR)
	COMPLEX*16 DJ(MAX_SUBST), DSA(MAX_MS), DSB(MAX_MS), XDJ, XDSB
	EQUIVALENCE (FL(1,1),FLC(1,1)), (RHO(1),DJ(1))
	DIMENSION DTEMP(INTER_CH), TS(M_HALF), ZQ(INTER_CH), 
     1  ZZQ(INTER_CH)
	DIMENSION ISTR(INTER_CH), NS(INTER_CH), XJBB(INTER_CH),
     1  XIS(INTER_CH), BLSJ(INTER_CH), TI_INT(INTER_CH), 
     1  RSCALE(INTER_CH-1)
     2  ,R(INTER_CH-1)
	DIMENSION HD(20), SIGTOT(MAX_TH), TH1TOT(MAX_EXP), S3(MAX_EXP)

	DO 1 K=1,INTER_CH
	NLDEC(K)=0
	DO 1 J=1,MAX_LDEC
1	DELTA(K,J)=0.
	XANG=-1.
	XFIS=1.
	INDROT=0
	LUNOUT=6

C  Read card 1.  Title.

2	READ(5,100,END=99) (HD(I), I=1,20)
	WRITE(LUNOUT,101)  (HD(I), I=1,20)

C  Read card 2. Control integers.

	READ(5,*) (ICO(I),I=1,NCONT), NC , M1
	NC=MIN(NC,N_GAM)
	M1=MIN(M1,MAX_EXP)
	IF (M1.GE.0) N1 = M1
	IF(ICO(1).NE.1) ICO(4)=0
	IF(ICO(4).EQ.0) THEN
	  NCH=1
	ELSE
	  READ(5,*) NCH
	ENDIF
	WRITE(LUNOUT,102) (ICO(I),I=1,NCONT)

C  Read card 3. Number of partial waves and spins of all particles
C		involved in the reaction.
C		All spins  and J transfers should be 2*actual value.
C		LP is no  of partial waves. Same as in DWUCK or CHUCK.

	IF (ICO(6) .EQ. 0) THEN
	  READ(5,*) LP,ISA,JA,ISB,(JB(N),N=1,NCH),(ISTR(N),N=1,NCH),
     2            (RSCALE(N),N=1,NCH-1)
	  LP = MIN(LP+1,MAX_L)
	  XJA=FLOAT(JA)/2.
	  DO 3 N=1,NCH
	  XJB(N)=FLOAT(JB(N))/2.
	  XIS(N)=FLOAT(ISTR(N))/2.
	  IF(TRIREL(ISA,ISTR(N),ISB).EQ.-1.)
     1  STOP ' SPINS OF ENTRANCE CHANNEL DONOT SATISFY SPIN STATS 3'
3	  CONTINUE

C  Read card 4. L and J transfers.

	  READ(5,*) NLTR,(LTR(I),I=1,NLTR),(JTR(I),I=1,NLTR)
	  NLTR=MIN(NLTR,MAX_LTR)
	  IF(ICO(4).EQ.0) THEN
	    IF(ICO(5).EQ.2) NLTR=1
	  ELSE
	    NLTR=NCH
	  ENDIF
	  DO 4 I=1,NLTR
	  IF(ICO(4).EQ.0) THEN
	    ISTEMP=ISTR(1)
	    JBTEMP=JB(1)
	  ELSE
	    ISTEMP=ISTR(I)
	    JBTEMP=JB(I)
	  ENDIF
	  IF(TRIREL(2*LTR(I),ISTEMP,JTR(I)).EQ.-1.
     1  .OR.TRIREL(JA,JTR(I),JBTEMP).EQ.-1.)
     2  STOP ' SPINS OF ENTRANCE CHANNEL DONOT SATISFY SPIN STATS 4'
4	  CONTINUE
	ENDIF

	WRITE(LUNOUT,103) LP-1,ISA,JA,ISB,NCH,(JB(N),N=1,NCH),
     1     (ISTR(N),N=1,NCH)
	WRITE(LUNOUT,104) NLTR,NLTR,(LTR(I),I=1,NLTR),(JTR(I),I=1,NLTR)

	IF(ICO(1).EQ.1.OR.ICO(1).EQ.2) THEN

C  Read cards 5a. Particle decay parameters.

	  READ(5,*) JC,ISC,(NLDEC(K),K=1,NCH)
	  DO K=1,NCH
	    NLDEC(K)=MIN(NLDEC(K),MAX_LDEC)
	  ENDDO
	  WRITE(LUNOUT,105)
	  WRITE(LUNOUT,106) JC,ISC,(NLDEC(K),K=1,NCH)
	  XS=FLOAT(ISC)/2.
	  DO 5 K=1,NCH
	  WRITE(LUNOUT,107) K
	  DO 5 I=1,NLDEC(K)
	  READ(5,*) LDEC(K,I),JDEC(K,I),DELTA(K,I)
	  IF(TRIREL(JDEC(K,I),2*LDEC(K,I),ISC).EQ.-1.)
     1  STOP ' SPINS OF EXIT CHANNEL DONOT SATISFY SPIN STATS 5a'
	  WRITE(LUNOUT,108) I, LDEC(K,I), JDEC(K,I)
5	  CONTINUE
	ENDIF
	IF(ICO(1).EQ.0.OR.ICO(1).EQ.2) THEN
	  WRITE(LUNOUT,109) NC
	  IF(ICO(1).EQ.2) THEN
	    NC=NC+1
	    GO TO 6
	  ENDIF
C  Read cards 5b. Gamma-ray decay parameters.

	  READ(5,*) LBC,JC2,ARCDEL
	  WRITE(LUNOUT,110) JC2,LBC,LBC+1
	  JC = JC2
	  ISC = 2
	  IF (NC.EQ.1) GO TO 7
6	  READ(5,*) LCD,JD2,ARCDELC
	  WRITE(LUNOUT,111) JD2,LCD,LCD+1
	  IF (NC.EQ.2) GO TO 7
	  READ(5,*) LDE,JE2,ARCDELD
	  WRITE(LUNOUT,112) JE2,LDE,LDE+1

C  Read card 6.	Geometrical attenuation coefficients
C		available only for gamma decay.

7	  READ(5,*) (ZW(I),I=1,MAX_LG)
	  WRITE(LUNOUT,113) (ZW(I),I=1,MAX_LG)

	ELSEIF(ICO(1).EQ.3) THEN

C  Read card 5c. Fission decay parameters
C		 spin projections are 2*actual value.

	  READ(5,*) KA,KB1
	  WRITE(LUNOUT,114)
	  WRITE(LUNOUT,115) KA,KB1
	  XKA=FLOAT(KA)/2.
	  XKB=FLOAT(KB1)/2.
	  XKJ=XKB-XKA
	ENDIF

	IF(ICO(6).EQ.0) THEN
	  JR=ISA+1
	  JS=ISB+1
	  LF1=JA+1
	  DO 8 N=1,NCH
	  NS(N)=ISTR(N)+1
	  XJBB(N)=JB(N)+1
	  LF2(N)=JB(N)+1
	  BLSJ(N)=SQRT(DFLOAT(NS(N))/DFLOAT(JR))
8	  CONTINUE
	  IF(NCH.GT.1) THEN
	    DO N=1,NCH-1
	      SQRS(N)=SQRT(RSCALE(N))
	    ENDDO
	  ENDIF

C  M2K.EQ.1 for 1/2 integer spin transfer.
	  M2K=NINT(1.0+PAR(NS(1)))/2

	ENDIF
	IF(ICO(1).LT.3) THEN
	  NSD=ISC+1
	  XJC=FLOAT(JC)/2.
	  LF3=JC+1
	  IF(ICO(1).EQ.0) THEN
	    IF(TRIREL(JC,JB(1),2*LBC).EQ.-1.)
     1    STOP ' SPINS OF EXIT CHANNEL DONOT SATISFY SPIN STATS 5c1'
	  ELSEIF(ICO(1).EQ.1.OR.ICO(1).EQ.2) THEN
	    DO 9 J=1,NCH
	    DO 9 I=1,NLDEC(J)
	    IF(TRIREL(JC,JB(J),JDEC(J,I)).EQ.-1.)
     1    STOP ' SPINS OF EXIT CHANNEL DONOT SATISFY SPIN STATS 5c2'
9	    CONTINUE
	  ENDIF
	ENDIF
	IF(ICO(6).EQ.0) THEN
	  IF (ICO(7).EQ.1) WRITE(LUNOUT,116)
	  IF(ICO(5).EQ.0) THEN

C  Read betas from file TAPE2 in DWUCK.

	    JTRMAX=JTR(1)
	    DO 10 KK=1,NLTR
	    KKIND=(KK-1)*JS*JR
	    MPLUS=JTR(KK)/2+1
	    JTRMAX=MAX0(JTR(KK),JTRMAX)
	    DO 10 J=1,JS
	    JIND=(J-1)*JR
	    DO 10 I=1,JR
	    IND=KKIND+JIND+I-1
	    LLI=IND*LP
	    DO 10 LL=1,LP
	    LLI=LLI+1
	    READ(2,117) L,(FL(LLI,K),K=1,MPLUS)
	    IF (ICO(7).EQ.1) WRITE(LUNOUT,117) L,(FL(LLI,K),K=1,MPLUS)
10	    CONTINUE

C  Read scattering amplitudes D from file TAPE2 in CHUCK.
C  ICON(8) in CHUCK should be equal to state number.

	  ELSEIF(ICO(5).EQ.1) THEN
	    KST1 = JR * LF1 * JS
	    DO 11 KK=1,NCH
	    KIND=KK-1
	    KST=KST1*(JB(NCH)+1)
	    LLI=KIND*LP
	    DO 11 LL=1,LP
	    LLI=LLI+1
	    READ(2,117) L,(FLC(LLI,K),K=1,KST)
	    IF (ICO(7).EQ.1) WRITE(LUNOUT,117) L,(FLC(LLI,K),K=1,KST)
11	    CONTINUE

C  Read m-state amplitudes from DWBA82.

	  ELSEIF(ICO(5).EQ.2) THEN
	    LLI=1
	    DO 12 KK=1,NLTR
	    MPLUS=2*(JTR(KK)+2)
	    DO 12 LL=1,MPLUS
	    READ(2,118) FL(LLI,1)
	    IF(ICO(7).EQ.1) WRITE(LUNOUT,118) FL(LLI,1)
	    LLI=LLI+1
12	    CONTINUE
	  ENDIF
	ENDIF

C  Read card 7. Polar angle for scat. partile and azimuthal and recoil angles
C		for decay particle.

	READ(5,*) ANG,PHID,RECANG,RECPHI,RECPSI
	XPHID=PHID
	XXANG=ANG

C  Read card 8. Polar and azimuthal opening angles for the ejectile detector
C		in the center of mass.

	READ(5,*) OPAN,STEPA,OPP,STEPP
	IF(RECANG.NE.0..OR.RECPHI.NE.0..OR.RECPSI.NE.0.) OPP=0.
	IF(STEPA.LT.1.0D-10) THEN
	  ICOUNT1 = 1
	ELSE
	  ICOUNT1 = 2*NINT(OPAN/(2*STEPA))
	  STEPA = OPAN/ICOUNT1
	ENDIF
	IF(STEPP.LT.1.0D-10) THEN
	  ICOUNT2 = 1
	ELSE
	  ICOUNT2 = NINT(OPP/STEPP) + 1
	  IF(OPP.EQ.360.) ICOUNT2=ICOUNT2-1
	ENDIF

C  Read cards 9. Experimental data.

	IF (M1.GT.0) READ(5,*) (AN1(I),PH1(I),C1(I),EC1(I),I=1,N1)

C  Read card 10. Vert. scale and horiz. scale for decay angular correlations.

	READ(5,*) NPLOT,NANGP
	NANGP=MIN(NANGP,181)
	WRITE(LUNOUT,121) NPLOT,NANGP
	DANGP=180./DFLOAT(NANGP-1)

	IF(INDANG.EQ.0.AND.ITIME.EQ.1) THEN
	  LUNOUT=1
	  OPEN(UNIT=1,STATUS='UNKNOWN',FILE='ANGCOR.LIS')
	  IPAR=0
	  DO N=1,NCH
	    DO I=1,NLDEC(N)
	      IPAR=IPAR+1
	      IF(ERR(IPAR).NE.0.0) THEN
		DELTA(N,I)=PARA(IPAR)
	      ELSE
		PARA(IPAR)=DELTA(N,I)
	      ENDIF
	    ENDDO
	  ENDDO
	  IF(NCH.GT.1) THEN
	    DO N=1,NCH-1
	      IPAR=IPAR+1
	      IF(ERR(IPAR).NE.0.0) THEN
		RSCALE(N)=PARA(IPAR)
	      ELSE
		PARA(IPAR)=RSCALE(N)
	      ENDIF
	    ENDDO
	  ENDIF
	  GO TO 1000
	ELSE
	  GO TO 1000
	ENDIF
	ENTRY AGCRHLP
	OPEN(UNIT=1,STATUS='UNKNOWN',FILE='ANGCOR.LIS')
	  IPAR=0
	  DO N=1,NCH
	    DO I=1,NLDEC(N)
	      IPAR=IPAR+1
	      DELTA(N,I)=PARA(IPAR)
	    ENDDO
	  ENDDO
	  IF(NCH.GT.1) THEN
	    DO N=1,NCH-1
	      IPAR=IPAR+1
	      RSCALE(N)=PARA(IPAR)
	    ENDDO
	  ENDIF
1000	CONTINUE

	IF (OPAN.GT.1.D-10) THEN
	  ANG=XXANG-OPAN*0.5
	  IF(ANG.LT.0.0) THEN
	    ANG=0.0
	    ICOUNT1=ICOUNT1/2
	  ENDIF
	  ANG=ANG+STEPA/2.

C  Remember STEPA has been changed to make ICOUNT1 even.

	  WRITE(LUNOUT,119) OPAN,STEPA
	ENDIF
	IF (OPP.GT.1.D-10) THEN
	  PHID=XPHID-OPP*0.5
	  WRITE(LUNOUT,120) OPP,STEPP
	ENDIF

	IF(ICO(1).EQ.1.OR.ICO(1).EQ.2) THEN
	  DO 13 K=1,NCH
	  IF(NLDEC(K).EQ.1) THEN
	    DELTA(K,1)=1.
	  ELSE
	    DTEMP(K)=0.
	    DO 14 I=1,NLDEC(K)
14	    DTEMP(K)=DTEMP(K)+DELTA(K,I)**2
	    DTEMP(K)=SQRT(DTEMP(K))
	    DO 15 I=1,NLDEC(K)
15	    DELTA(K,I)=DELTA(K,I)/DTEMP(K)
	  ENDIF
13	  CONTINUE
	ENDIF

	DO 16 I=1,NANGP
16	SIGTOT(I) = 0.
	DO 17 I=1,N1
17	TH1TOT(I) = 0.
	TTOT = 0.
	DO N=1,NCH
	  TI_INT(N) = 0.
	ENDDO
	SSINT = 0.

C  Begining of the loop to average over ejectile opening angle.

	DO 1001 IC=1,ICOUNT1

	WRITE(LUNOUT,116)
	ISKIP=0
	IF(ICO(6).EQ.1.AND.ANG.EQ.XANG) THEN
	  ISKIP=1
	  IF(INDROT.EQ.1.AND.ICO(9).EQ.0) ISKIP=0
	ENDIF
	IF(ICO(6).EQ.1.AND.ICO(5).EQ.2.AND.ANG.NE.XANG) STOP 'BY'
	XANG=ANG
	IF(ISKIP.EQ.1) GO TO 1002

	SINT=SIN(ANG/57.295779)
	DO 18 M=1,M_HALF*MAX_LTR
	DO 18 J=1,MAX_MS
	DO 18 I=1,MAX_MS
	SUM(J,I,M)=(0.,0.)
18	CONTINUE
	DO 19 M=1,M_HALF
	TS(M)=0.
19	CONTINUE
	T =0.
	T1=0.
	DO N=1,NCH
	  TI(N) = 0.
	ENDDO
	IF(ICO(5).EQ.0.OR.ICO(5).EQ.2) THEN
	  MT=0
	  IF(ICO(5).EQ.0) THEN
	    NPLUS=(JTRMAX+ISA+ISB)/2+1
	    CALL PLM(ANG,LP,NPLUS)
	  ENDIF
	  IND=0
	  IF (OPAN.LT.1.D-6) WRITE(LUNOUT,122)
	  DO 20 KK=1,NLTR
	  XJ=SQRT(DFLOAT(JTR(KK)+1))
	  MPLUS=JTR(KK)/2+1
	  BLSJK=BLSJ(1)
	  IF(ICO(4).EQ.1.AND.KK.EQ.2) BLSJK=BLSJ(2)
	  KKIND=(KK-1)*JS*JR
	  DO 21 M=1,MPLUS
	  MK=M+M-1
	  MM=MT+M
	  SQ=0.
	  ISS2=-ISB
	  DO 22 J=1,JS
	  JIND=(J-1)*JR
	  ISS1=-ISA
	  DO 23 I=1,JR

C  ML=M(Satchler)=MA-MB+Mma-Mmb

	  ML=(MK-1+M2K+ISS2-ISS1)/2
	  PHAS=1.
	  IF(ICO(5).EQ.0) THEN

C  IF(ML.GT.0) PHAS=PAR(ML)   [in DWUCK].
C  Notice that the following phase is different from DWUCK.
C  This phase gives the correct position of the symmetry axis.

	    IF(ML.LT.0) PHAS=PAR(ML)
	    ML=IABS(ML) + 1
	    F=(0.,0.)
	    IND=KKIND+JIND+I-1
	    LLI=IND*LP
	    DO 24 LL=1,LP
	    LLI=LLI+1
	    F=F+FL(LLI,M )*PL(LL,ML)
24	    CONTINUE
	    F=F*BLSJK/XJ

C   2			2
C  B    of DWUCK, also A   of Satchler, are proportional to
C   L0L			L0L
C  beta**2/(2*L+1) in vibrational model or beta**2*CG(JB,L,K,0,JA,K)**2
C  in rotational model, where beta is the deformation parameter.

	    SUM(J,I,MM)=SUM(J,I,MM)+F*PHAS

C						   M,MB,MA
C  SUM(J,I,MM) as defined here are the same as beta	  (ka,kb)
C						   LSJ
C  as defined by Satchler Nucl.Phys. 55(1964)1.


C  For DWBA82.

	  ELSE
	    IND=IND+1
	    IF(ISS2.LT.0) PHAS=PAR(ML)

C  Factor of SQRT(5) is to give same cross sections as DWBA82.

	    SUM(J,I,MM)=SQRT(5.)*PHAS*FL(IND,1)/XJ
	    F=SUM(J,I,MM)
	  ENDIF
	  IF (OPAN.LT.1.D-6) WRITE(LUNOUT,123) J,I,MM,SUM(J,I,MM)
	  SQ=SQ+DREAL(F)**2+DIMAG(F)**2
	  ISS1=ISS1+2
23	  CONTINUE
	  ISS2=ISS2+2
22	  CONTINUE

C  Assume each L,S transfer is different for different J transfers
C  sum over M incoherently.

	  IF(ICO(1).EQ.3) THEN
	    XJTR=DFLOAT(JTR(KK))/2.
	    XFIS=CG(XJA,XKA,XJTR,XKJ,XJB(1),XKB)*SQRT(DFLOAT(LF1)/
     1           XJBB(1))
	  ENDIF
	  IF(ICO(4).EQ.0) THEN
	    TS(M)=TS(M)+SQ*XJBB(1)*XFIS*XFIS
	  ELSE
	    IF(KK.EQ.1) THEN
	      TS(M)=TS(M)+SQ*XJBB(KK)
	    ELSE
	      TS(M)=TS(M)+SQ*XJBB(KK)*RSCALE(KK-1)
	    ENDIF
	  ENDIF
21	  CONTINUE
	  MT=MT+MPLUS
20	  CONTINUE
	ELSE
	  JBMAX=JB(1)
	  IF(NCH.GT.1) THEN
	    DO N=2,NCH
	      JBMAX=MAX0(JBMAX,JB(N))
	    ENDDO
	  ENDIF
	  NPLUS=JA+JBMAX+ISA+ISB
	  IF (OPAN.LT.1.D-6) WRITE(LUNOUT,124)
	  CALL PLM(ANG,LP,NPLUS)
	  MT=0
	  DO 25 KK=1,NCH
	  KIND=KK-1
	  IND = 1
	  MA=-JA
	  DO 26 MSA=1,LF1
	  MB=-JB(KK)
	  DO 27 MSB=1,LF2(KK)
	  SQ=0.
	  JIND=MSB*(MSA-1)+MSB+MT
	  IS1=-ISA
	  DO 28 I=1,JR
	  IS2=-ISB
	  DO 29 J=1,JS
	  ML=(IS1-IS2+MA-MB)/2

C  Notice that the following phase is extra to CHUCK m-state population
C  amplitudes. It gives the correct position of the symmetry axis.

	  PHAS = PAR(ML)
	  ML=IABS(ML)+1
	  F=(0.,0.)
	  LLI=KIND*LP
	  DO 30 LL=1,LP
	  LLI=LLI+1
	  F=F+FLC(LLI,IND)*PL(LL,ML)
30	  CONTINUE
	  SUM(J,I,JIND) = SUM(J,I,JIND) + F*PHAS
	  IF (OPAN.LT.1.D-6) WRITE(LUNOUT,125) MA,MB,IS1,IS2,
     1                                         SUM(J,I,JIND)
	  SQ=SQ+DREAL(F)**2+DIMAG(F)**2
	  IND=IND+1
	  IS2=IS2+2
29	  CONTINUE
	  IS1=IS1+2
28	  CONTINUE
	  TS(MSB)=TS(MSB)+SQ
	  MB=MB+2
27	  CONTINUE
	  MA=MA+2
26	  CONTINUE
	  MT=MT+LF1*LF2(KK)
25	  CONTINUE
	ENDIF
	IF(ICO(5).EQ.0.OR.ICO(5).EQ.2) THEN
	  MPLUS=JTRMAX/2+1
	  IF(ICO(5).EQ.2) THEN
	    MPLUS=JTR(1)/2+1
	    IF(ICO(4).NE.0) THEN
	      DO N=2,NCH
		MPLUS=MAX0(MPLUS,JTR(N)/2+1)
	      ENDDO
	    ENDIF
	  ENDIF
	  DO 31 M=1,MPLUS
	  SQ=0.
	  IF(ICO(4).NE.0) THEN
	    DO N=1,NCH
	      ZQ(N)=0.
	    ENDDO
	  ENDIF
	  DO 32 J=1,JS
	  DO 32 I=1,JR
	  IND=M
	  SSQ=(0.,0.)

C  Sum coherently over L,S for the same J transfer.

	  DO 33 LSJ=1,NLTR
	  ZZQ(LSJ)=0.
	  IF(M.LE.JTR(LSJ)/2+1) THEN
	    IF(ICO(1).EQ.3) THEN
	      XJTR=FLOAT(JTR(LSJ))/2.
	      XFIS=CG(XJA,XKA,XJTR,XKJ,XJB(1),XKB)
     1           *SQRT(DFLOAT(LF1)/XJBB(1))
	    ENDIF
	    SSQ=SSQ+SUM(J,I,IND)*XFIS
	    IF(ICO(4).NE.0) ZZQ(LSJ)=DREAL(SUM(J,I,IND))**2
     1 			    +DIMAG(SUM(J,I,IND))**2
	  ENDIF
	  IND=IND+JTR(LSJ)/2+1
33	  CONTINUE
	  SQ=SQ+DREAL(SSQ)**2+DIMAG(SSQ)**2
	  IF(ICO(4).NE.0) THEN
	    DO N=1,NCH
	      ZQ(N)=ZQ(N)+ZZQ(N)
	    ENDDO
	  ENDIF
32	  CONTINUE
	  SQ=SQ*XJBB(1)
	  IF(ICO(4).NE.0) THEN
	    ZQ(1)=ZQ(1)*XJBB(1)
	    DO N=2,NCH
	      ZQ(N)=ZQ(N)*XJBB(N)*RSCALE(N-1)
	    ENDDO
	  ENDIF

C  for M.NE.0 the m-state population cross section is double because
C  of alignment.

	  IF(M+M2K.NE.1) THEN
	    SQ=SQ+SQ
	    TS(M)=TS(M)+TS(M)
	    IF(ICO(4).NE.0) THEN
	      DO N=1,NCH
		ZQ(N)=ZQ(N)+ZQ(N)
	      ENDDO
	    ENDIF
	  ENDIF
	  T=T+SQ
	  T1=T1+TS(M)
	  IX=M-1

C  For the next 3 write statements (IX) is the projection of JTR the
C  total angular momentum transfer on the z-axis.

	  IF(ICO(4).NE.0) THEN
	    DO N=1,NCH
	      TI(N)=TI(N)+ZQ(N)
	    ENDDO
	  ENDIF
	  IF (ICO(8).EQ.0) THEN
	    WRITE(LUNOUT,126) IX,TS(M)
	    IF(ICO(4).NE.0) THEN
	      WRITE(LUNOUT,126) IX,(ZQ(N),N=1,NCH)
	    ELSE
	      WRITE(LUNOUT,126) IX,SQ
	    ENDIF
	  ENDIF
31	  CONTINUE
	ELSE
C+++++++This section has still to be checked to see if things are properly 
C	done concerning the calculation of M-substates for the various
C	intermediate channels calculated from D-amplitudes of CHUCK

	  MT=0
	  DO 34 KK=1,NCH
	  KIND=KK-1
	  MB=-JB(KK)
	  DO 35 MSB=1,LF2(KK)
	  SQ=0.
	  DO N=1,NCH
	    ZQ(2)=0.
	  ENDDO
	  DO 36 MSA=1,LF1
	  JIND=(MSA-1)*MSB+MSB+MT
	  DO 36 J=1,JS
	  DO 36 I=1,JR
	  F=SUM(J,I,JIND)
	  SQ=SQ+DREAL(F)**2+DIMAG(F)**2
36	  CONTINUE
	  T1=T1+TS(MSB)

C  For the next 3 write statements (MB) is the projection of JB the
C  spin of the intermediate state on the z-axis.

	  WRITE(LUNOUT,126) MB,TS(MSB)
	  IF(ICO(4).NE.0) THEN
	    ZQ(KK)=ZQ(KK)+SQ
	    DO N=1,NCH
	      TI(N)=TI(N)+ZQ(N)
	    ENDDO
	    WRITE(LUNOUT,126) MB,(ZQ(N),N=1,NCH)
	  ELSE
	    T=T+SQ
	    WRITE(LUNOUT,126) MB,SQ
	  ENDIF
	  MB = MB + 2
35	  CONTINUE
	  MT=MT+LF1*LF2(KK)
34	  CONTINUE
	ENDIF
C+++++++

1002	CONTINUE
	IF(ICO(4).NE.0) THEN
	  T=TI(1)
	  DO N=2,NCH
	    R(N-1)=TI(1)/TI(N)
	    T=T+TI(N)
	  ENDDO
	  IF (ICO(8).EQ.0) THEN
	    N=1
	    WRITE(LUNOUT,127) ANG,N,TI(N)
	    DO N=2,NCH
	      WRITE(LUNOUT,127) ANG,N,TI(N)
	      WRITE(LUNOUT,128) ANG,N,RSCALE(N-1),N,R(N-1)
	    ENDDO
	  ENDIF
	ENDIF

C  T  is   coherent sum of scattering amplitudes.
C  T1 is incoherent sum of scattering amplitudes.

	IF (ICO(8).EQ.0) THEN
	  WRITE(LUNOUT,129) ANG,T
	  WRITE(LUNOUT,129) ANG,T1
	ENDIF

	IF(ISKIP.EQ.1) GO TO 1003

C  Replaced the next line by the one following it because the next time
C  around the RSUM'S were not calculated (M.N.H. 19 September 1984).
C	IF(ICO(6).EQ.0.AND.ICO(5).NE.1) THEN

	IF(ICO(5).NE.1) THEN
	  MT=0
	  MTT=0
	  DO 37 KK=1,NLTR
	  LF=JTR(KK)+1
	  MJ=-JTR(KK)
	  DO 38 M2=1,LF
	  MX=IABS(MJ)/2+1
	  MXT=MX+MTT
	  MM2=M2+MT
	  MSB=-ISB
	  DO 39 J2=1,JS
	  MSA=-ISA
	  DO 40 I2=1,JR
	  IF(MJ.LT.0) THEN
	    MS2=(-MSB+ISB)/2+1
	    MS1=(-MSA+ISA)/2+1
	    PHAS=PAR((MJ+MSB-MSA+JTR(KK)+2*LTR(KK)+ISB-ISA)/2)
	  ELSE
	    MS2=J2
	    MS1=I2
	    PHAS=1.
	  ENDIF
	  RSUM(J2,I2,MM2)=SUM(MS2,MS1,MXT)*PHAS
	  MSA=MSA+2
40	  CONTINUE
	  MSB=MSB+2
39	  CONTINUE
	  MJ=MJ+2
38	  CONTINUE
	  MTT=MTT+JTR(KK)/2+1
	  MT=MT+LF
37	  CONTINUE
	  MT=MT+1
	  DO 41 M2=1,MT
	  DO 41 J2=1,JS
	  DO 41 I2=1,JR
	  SUM(J2,I2,M2)=RSUM(J2,I2,M2)
41	  CONTINUE
	ENDIF

1003	CONTINUE

C  This section rotates the m-state population amplitudes to the recoil
C  center of mass system.

	IF(ISKIP.EQ.0) THEN
	  IF(ICO(9).EQ.0) THEN
	    INDROT=0
	    GO TO 42
	  ENDIF
	ELSE
	  IF(INDROT.EQ.1.AND.ICO(9).EQ.1) THEN
	    WRITE(LUNOUT,130) RECANG
	    GO TO 42
	  ELSEIF(ICO(9).EQ.0) THEN
	    INDROT=0
	    GO TO 42
	  ENDIF
	ENDIF
	IF(ICO(5).EQ.2) THEN
	  RECTEMP=RECANG
	  RECANG=RECANG+ANG
	ENDIF
	MT=0
	RPHI=RECPHI/57.295779
	RPSI=RECPSI/57.295779
	DO 43 KK=1,NLTR
	LF=JTR(KK)+1
	IM=-JTR(KK)
	DO 44 M1=1,LF
	MM1=MT+M1
	CALL DJMK(-RECANG,RPHI,RPSI,JTR(KK),IM,DJ)
	IMB=-ISB
	DO 45 J1=1,JS
	CALL DJMK(-RECANG,RPHI,RPSI,ISB,IMB,DSB)
	IMA=-ISA
	DO 46 I1=1,JR
	CALL DJMK(-RECANG,RPHI,RPSI,ISA,IMA,DSA)
	RSUM(J1,I1,MM1)=(0.,0.)
	DO 47 M2=1,LF
	MM2=MT+M2
	XDJ=DCMPLX(DREAL(DJ(M2)),-DIMAG(DJ(M2)))
	DO 47 J2=1,JS
	XDSB=DCMPLX(DREAL(DSB(J2)),-DIMAG(DSB(J2)))
	DO 47 I2=1,JR
	RSUM(J1,I1,MM1)=RSUM(J1,I1,MM1)+SUM(J2,I2,MM2)
     1 *XDJ*XDSB*DSA(I2)
47	CONTINUE
	IMA=IMA+2
46	CONTINUE
	IMB=IMB+2
45	CONTINUE
	IM=IM+2
44	CONTINUE
	MT=MT+LF
43	CONTINUE
	DO 48 KK=1,MT
	DO 48 J2=1,JS
	DO 48 I2=1,JR
	SUM(J2,I2,KK)=RSUM(J2,I2,KK)
48	CONTINUE
	IF(ICO(5).EQ.2) RECANG=RECTEMP
	WRITE(LUNOUT,130) RECANG
	INDROT=1
42	CONTINUE

	IF(ICO(1).EQ.0.OR.ICO(1).EQ.2) THEN
	  CALL RHOMM
	  IF (OPAN.LT.1.D-6) THEN
	    MB=-JB(1)
	    DO 49 I=1,LF2(1)
	    SQ=FPP(I,I)

C  In the following write statement (M) is the projection of JB the
C  spin of the intermediate state on the z-axis.

	    WRITE(LUNOUT,131) MB,SQ
	    MB=MB+2
49	    CONTINUE
	  ENDIF
	ENDIF

	TSINT=T*SINT
	IF(IC.GT.1) PHID=XPHID-OPP*0.5
	DO 1004 JCC=1,ICOUNT2
	WRITE(LUNOUT,132) ANG,PHID
	PHI=PHID/57.295779
	IF(ICO(1).EQ.1.OR.ICO(1).EQ.3) THEN
	  CALL PARCOR(PHI,T)
	ELSEIF(ICO(1).EQ.0.OR.ICO(1).EQ.2) THEN
	  CALL GAMCOR(ANG,PHI)
	ENDIF

C  Weight the angular correlations with the cross section.

	DO 50 NP=1,NANGP
	SIGTOT(NP)=SIGTOT(NP)+SIG(NP)*TSINT
50	CONTINUE
	DO 51 NP=1,N1
	TH1TOT(NP)=TH1TOT(NP)+TH1(NP)*TSINT
51	CONTINUE
	PHID=PHID+STEPP
1004	CONTINUE
	TTOT=TTOT+ICOUNT2*TSINT
	SSINT=SSINT+ICOUNT2*SINT
	IF(ICO(4).NE.0) THEN
	  DO N=1,NCH
	    TI_INT(N)=TI_INT(N)+ICOUNT2*TI(N)*SINT
	  ENDDO
	ENDIF
	ANG=ANG+STEPA
1001	CONTINUE
	IF(ICOUNT1.GT.1.OR.ICOUNT2.GT.1) WRITE(LUNOUT,133)
	IF(SSINT.NE.0.) THEN
	  T = TTOT/SSINT
	  IF(ICO(4).NE.0) THEN
	    DO N=1,NCH
	      TI(N) = TI_INT(N)/SSINT
	    ENDDO
	  ENDIF
	ENDIF
	IF(ICO(4).EQ.0) THEN
	  WRITE(LUNOUT,134) T
	ELSE
	  WRITE(LUNOUT,135) TI(1),T
	  DO N=2,NCH
	    WRITE(LUNOUT,136) N,TI(N),N,TI(1)/TI(N)
	  ENDDO
	ENDIF
	IF(N1.EQ.0) GO TO 52
	IF(SSINT.NE.0.) THEN
	  DO 53 NP=1,N1
	  TH1(NP)=TH1TOT(NP)/TTOT
53	  CONTINUE
	ENDIF
	S1 = 0.
	S2 = 0.
	DO 54 I = 1, N1
	E = EC1(I) * EC1(I)
	S1 = S1 + C1(I) * TH1(I) / E
	S2 = S2 + TH1(I) * TH1(I) / E
54	CONTINUE
	C = S1 / S2
	CHI = 0.
	DO 55 I=1,N1
	S3(I) = (C1(I)-C*TH1(I))/EC1(I)
55	CHI = CHI + S3(I)*S3(I)
	IF (ICO(8).NE.0) THEN
	  IF(ICO(1).EQ.0.OR.ICO(1).EQ.2) THEN
	    IF(NC.EQ.1) THEN
	      WRITE(LUNOUT,137) CHI
	    ELSEIF(NC.EQ.2) THEN
	      WRITE(LUNOUT,138) CHI
	    ELSEIF(NC.EQ.3) THEN
	      WRITE(LUNOUT,139) CHI
	    ENDIF
	  ELSE
	    WRITE(LUNOUT,140) CHI
	  ENDIF
	ELSE
	  WRITE(LUNOUT,141) C,CHI
	  WRITE(LUNOUT,142) (AN1(I),PH1(I),C1(I),C*TH1(I),C1(I)/C,
     1  TH1(I),S3(I)*S3(I), I=1,N1)
	ENDIF
52	CONTINUE
	IF(SSINT.NE.0.) THEN
	  DO 56 NP=1,NANGP
	  SIG(NP)=SIGTOT(NP)/TTOT
56	  CONTINUE
	ENDIF

	CALL PLOT
	IF (INDANG.EQ.1) GO TO 2
	CLOSE(UNIT=1)
	RETURN
99	STOP

100	FORMAT(20A4)
101	FORMAT(1H1,20A4)
102	FORMAT(1H0,' 9*ICO = ',9I1)
103	FORMAT(1H0,' LMAX= ',I3,' ISA= ',I3,' JA= ',I3,' ISB= ',I3,
     1 ' [JB(N),ISTR(N)],N=1,',I1,' : ',10(I3,1H,))
104	FORMAT(1H0,I3,'*LTR,',I2,'*JTR = ',16(I3,1H,))
105	FORMAT(1H0,//,'   P A R T I C L E   D E C A Y',//)
106	FORMAT(1H0,' JC =',I3,'   ISC =',I3,'   NLDEC(1)=',I3,
     1 /23X,'NLDEC(2)=',I3)
107	FORMAT(1H0,' INTERMEDIATE STATE =',I3)
108	FORMAT(' NLDEC =',I3,'    LDEC =',I3,'   JDEC =',I3)
109	FORMAT(1H0,'GAMMA-RAY DECAY - - - CASCADE HAS',I3,
     1 '  TRANSITIONS')
110	FORMAT(1H0,'  JC =',I3,'     LBC =',I3,' AND',I3)
111	FORMAT(1H0,'  JD =',I3,'     LCD =',I3,' AND',I3)
112	FORMAT(1H0,'  JE =',I3,'     LDE =',I3,' AND',I3)
113	FORMAT(1H0,//,' ATTENUATION COEFFICIENTS :',//,' Q(2) =',F10.5,
     1 '    Q(4) =',F10.5,'    Q(6) =',F10.5)
114	FORMAT(1H0,//,'   F I S S I O N   D E C A Y',//)
115	FORMAT(1H0,'   KA =',I3,'    KB =',I3)
116	FORMAT('1')
117	FORMAT(I4,10E12.5/(4X,10E12.5))
118	FORMAT(2E12.5)
119	FORMAT(' OPENING POLAR ANGLE= ',F5.1,'  STEP SIZE
     1    = ',F7.3)
120	FORMAT(' OPENING AZIMUTHAL ANGLE = ',F5.1,'  STEP SIZE
     1    = ',F7.3)
121	FORMAT(1H0,' NPLOT= ',I3,' NANGP= ',I3)
122	FORMAT(1H0,'  J   I   M      REAL BETA     IMAG BETA')
123	FORMAT(3I4,2F15.6)
124	FORMAT(1H0,'  MA  MB IS1 IS2     REAL SIG      IMAG SIG')
125	FORMAT(4I4,2F15.6)
126	FORMAT(' M=',I3,'   SIGMA(M)=',5E12.4)
127	FORMAT(' PROJ. SCATT. ANG.= ',F5.1,'  CHAN ',I1,'-SIGMA=',E12.4)
128	FORMAT(' PROJ. SCATT. ANG.= ',F5.1,'  RSCALE(',I1,') = ',E12.4,
     1 '  SIG1/SIG',I1,' = ',E12.4)
129	FORMAT(' PROJ. SCATT. ANG.= ',F5.1,'  TOTAL-SIGMA=',E12.4)
130	FORMAT(1H0,'  ANGULAR CORRELATION TRANSFORMED TO THE RECOIL AXIS
     1  SYSTEM , RECOIL ANGLE = ',F7.2)
131	FORMAT(' M=',I3,'/2    SIGMA(M)=',E12.4)
132	FORMAT(//,' SCATT. ANGLE     = ',F5.1,'  AZIM. DECAY ANGLE = '
     1 ,F6.1)
133	FORMAT('1AVERAGE OVER ALL ANGLES')
134	FORMAT( ' SIGTOT = ',E12.4)
135	FORMAT( ' SIG1 = ',E12.4,' SIGTOT = ',E12.4)
136	FORMAT( ' SIG',I1,' = ',E12.4,' SIG1/SIG',I1,' = ',E12.4)
137	FORMAT(1H+,20X,'  CHI2 =',F8.3)
138	FORMAT(1H+,40X,'  CHI2 =',F8.3)
139	FORMAT(1H+,60X,'  CHI2 =',F8.3)
140	FORMAT(1H+,70X,'  CHI2 =',F8.3)
141	FORMAT(1H ,//,
     1 '   THETA      PHI      EXP      THEOR*C     EXP/C',
     1 '     THEOR     CHI2     C =',F10.3,'  CHI2TOT =',F7.3,/)
142	FORMAT(7F10.3)
	END
