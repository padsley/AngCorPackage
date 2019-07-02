	SUBROUTINE CIOQSH
C++
C TITLE:  CIOQSH
C FACILITY:  CIO  - conversational I/O package.
C ABSTRACT: Show the legal qualifiers
C AUTHOR: Peter A. Kroon, KVI, Groningen, NL.
C PROCEDURES CALLED: none
C CREATION DATE:  13-may-1981
C MODIFIED BY:
C FUNCTIONAL DESCRIPTION:
C	Display the legal qualifiers.
C	Dynamic formatting is used to adapt the nr of qualifiers displayed
C	per line to the length of the qualifiers in characters.
C CALLING SEQUENCE:
C	CALL CIOQSH
C--

	INCLUDE		'ciocom.inc'
	INTEGER		I

C	IF (Q_NR.GT.0) WRITE (LUNO,9000) (Q_NAMES(I),I=1,Q_NR)
C       9000	FORMAT(T8,'qualifiers ..........',(T30,<50/(QUASIZ+3)>('/',A,:,2X)))
	WRITE(*,*) 'This is where lgal qualifiers would come'
	END
