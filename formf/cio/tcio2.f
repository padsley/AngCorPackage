	PROGRAM TCIO1

C Ad Hoc test for CIOI

	IMPLICIT NONE

	INTEGER			IVAR, IXVAR, IQUAL1, INT, INTOPT
	INTEGER			LENQVAR, LENQVAROPT, IQVAR, LSVAR
	REAL			FVAR, REALVAR, REALOPT
	DOUBLEPRECISION	DVAR
	LOGICAL			LVAR
	LOGICAL*1		BVAR
	CHARACTER*37	AVAR,SVAR,QVAR,QVAROPT
	REAL			ARRAY(100)
	character*30	helplib
	LOGICAL			LQUAL
	LOGICAL			LEMPTY
	INTEGER			I, K, N

	call cios ('name of help library',helplib,k)
	call ciohlpini (helplib(:k),' ')

    1	CALL CIOWRT('Dit komt via CIOWRT')
	CALL CIOWRT('Evenals dit')
	CALL CIOIX ('(1) Integer',      IVAR, 'QLH>$', -6, 18, *2, *20)
	CALL CIOIX ('(1a) Hex integer', IXVAR,'LHX',   -6, 18)

	CALL CIOQLX ('LOGFULL', IQUAL1, 7, 3)
	CALL CIOQL  ('LOGSHORT', LQUAL)

	CALL CIOQI ('INT', INT, 20, 30)
	CALL CIOQI ('INTOPT', INTOPT, 20, 30, 23)

	CALL CIOQF ('REAL', REALVAR, 20., 30.)
	CALL CIOQF ('REALOPT', REALOPT, 20., 30., 23.)

	CALL CIOQS ('STRING', LENQVAR, QVAR)
	CALL CIOQS ('STRINGOPT', LENQVAROPT, QVAROPT, IQVAR)

    2	CALL CIOFX ('(2) F-float',FVAR,'D<^>$',123.45,*1,*1,*3,*20)
    3	CALL CIODX ('(3) D-float',DVAR,'DL<^>$',0.01932,2.,*2,*1,*4,*20)
    4	CALL CIOLX ('(4) Logical',LVAR,'<^>$',*3,*1,*5,*20)
    5	CALL CIOLX ('(5) Boolean',BVAR,'D<^>$',.FALSE.,*4,*1,*6,*20)
    6	CALL CIOAX ('(6) Text',AVAR,'D<^>$','automatic',*5,*1,*7,*20)
    7	CALL CIOSX ('(7) String',SVAR,LSVAR,'D<^$','AbCdEfGh',
	1 *6,*1,*20)

	CALL CIOQCO		! complete qualifier scan

   10	CONTINUE

	WRITE (*,*) 'LOGFULL',IQUAL1
	WRITE (*,*) 'LOGSHORT',LQUAL
	WRITE (*,*) 'INT',INT
	WRITE (*,*) 'INTOPT',INTOPT
	WRITE (*,*) 'REAL',REALVAR
	WRITE (*,*) 'REALOPT',REALOPT
	WRITE (*,*) 'STRING',QVAR
	WRITE (*,*) 'STRINGOPT',QVAROPT
	WRITE (*,*) 'LENQVAR', LENQVAR
	WRITE (*,*) 'LENQVAROPT', LENQVAROPT
	WRITE (*,*) 'IQVAR',IQVAR

	WRITE (*,*) 1,IVAR, IXVAR
	WRITE (*,*) 2,FVAR
	WRITE (*,*) 3,DVAR
	WRITE (*,*) 4,LVAR
	WRITE (*,*) 5,BVAR
	WRITE (*,*) 6,AVAR
	WRITE (*,*) 7,LSVAR,SVAR

C Now test the CIOTBE entry
	write (*,*) '(''0Nu testen we CIOTBE''/)'

	DO 100 I=1,100
	CALL CIOFX ('Enter an element of ARRAY',ARRAY(I),'$',*20)
	N = I
	CALL CIOTBE (LEMPTY)
	IF (LEMPTY) GOTO 110
  100	CONTINUE
  110	WRITE (*,*) 'You entered',N,'elements. Their values are:'
	WRITE (*,*) (ARRAY(I),I=1,N)
	GOTO 1

   20	WRITE (*,*) 'You took the escape exit'
	GOTO 1
	END
