	program tcio1

C Test CIOCMD routine

	IMPLICIT NONE
	LOGICAL LGO
	CHARACTER*5 FULL
	CHARACTER*5 CMDTBL(24)
	CHARACTER*30 helplib
	INTEGER K, ICMD

	DATA CMDTBL/
	1 'MACRO','MICRO','MACRI','COBOL','PASEN','VEEL ',
	2 'MEER ','VEREN','VAREN','VOREN','VIER ','VIJF ',
	3 'FRANS','FRATS','ACU  ','ACUCL','RSCAL','PSCAL',
	4 'STOP ','START','GO   ','GO-ON','FIND ','TERMI'/

	CALL CIOS ('Enter name of help library',helplib,k)
	CALL CIOHLPINI (helplib(:k),' ')

   10	CALL CIOWRT ('*******************')
	CALL CIOCMD ('TCIO1>',CMDTBL,24,ICMD)
	WRITE (*,*) 'ENTRY NR IS ',ICMD
	WRITE (*,*) 'FULL COMMAND = ',CMDTBL(ICMD)

	CALL CIOLX ('Do you want to proceed',LGO,'D',.TRUE.)
	IF (LGO) GOTO 10
	STOP
	END
