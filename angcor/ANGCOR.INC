c Nov-2000 M.Yosoi   Rev. for IBM-xlf
c
	PARAMETER (NCONT = 9)	!Number of control integers
	PARAMETER (N_GAM =3)	!Number of gammas in cascade; if changed
					!from 3 section reading cards 5B in
					!angcor and subroutine GAMCOR should be
					!modified
	PARAMETER (MAX_EXP = 10)	!Number of experimental data points
	PARAMETER (INTER_CH = 3)	!Number of intermediate channels
	PARAMETER (MAX_L = 150) 	!Number of partial waves
	PARAMETER (MAX_MS = 3)		!Number of spin substates for
					!projectile or ejectile (i.e JR=ISA+1
					!or JS=ISB+1; ISA = twice spin of
					!projectile, ISB = twice spin of
					!ejectile 
	PARAMETER (MAX_SUBST = 21)	!Number of populated substates; maximum
					!of JB(i)+1 or JTRMAX+1 (JTRMAX is
					!twice maximum transferred angular
					!momentum)
	PARAMETER (M_HALF = MAX_SUBST/2+1)
	PARAMETER (MAX_LTR = 8)		!Number of L-transfers (determined
					!by DWUCK)
	PARAMETER (MAX_CHSPIN = 13)	!(JTRMAX+ISA+ISB)/2+1
	PARAMETER (MAX_LDEC = 3)	!Number of L-decay
	PARAMETER (MAX_LG = 3)		!Minimum of JB(i)/2 and Lgamma
	PARAMETER (MAX_TH = 181)	!Number of angles for angular
					!correlation

	PARAMETER (MAX_DIM1 = MAX_L*MAX_LTR*MAX_MS*MAX_MS)
	PARAMETER (MAX_DIM2 = MAX_MS*MAX_MS*MAX_SUBST*2)!2 takes the place of 
							!2*JA+1
	
	COMPLEX*16 SUM,FPP,RHO

	COMMON/AAA/ ICO(NCONT), LP, JA, LF1, ISA, JR, 
     2   JB(INTER_CH), LF2(INTER_CH),
     2  ISB, JS, NLTR, LTR(MAX_LTR), JTR(MAX_LTR), JC, LF3, ISC, NSD,
     2  NLDEC(INTER_CH), LDEC(INTER_CH,MAX_LDEC), 
     3  JDEC(INTER_CH,MAX_LDEC),NPLOT, NANGP, LUNOUT
	
	COMMON/BBB/ XJA, XJB(INTER_CH), XJC, XS, 
     1  DELTA(INTER_CH,MAX_LDEC),
     1  PL(MAX_L,MAX_CHSPIN), DANGP, SUM(MAX_MS,MAX_MS,M_HALF*MAX_LTR),
     2  FPP(MAX_SUBST,MAX_SUBST), RHO(MAX_SUBST*MAX_SUBST), SIG(MAX_TH)

