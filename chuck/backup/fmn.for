      SUBROUTINE FMN(UB,KT,DRX)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION  UB(800)
      R = 0.0
      C = 2.50
      C2 = C*C
      F1 = 1.0
      DO 100 M=1,KT
      R = R + DRX
      E = R*R/C2
      F1 = EXP(-E)
      IF (F1 .GT. 1.0E-33 ) GOTO 250
      UB(M) = 0.0
      UB(M+400) = 0.0
      GOTO 101
250   CONTINUE
      IF ( C .EQ. 2.5)  F2 = -0.0036*(E**6) + 0.1066 * (E**5)
     1 -0.5039*(E**4) + 0.8375 *(E**3) - 0.6107 * (E**2) - 0.1036 * E
      UB(M) = F1 * F2
      UB(M+400) = 1.0
100   CONTINUE
 101  KT = M
      CALL PLOT(UB,DRX,KT)
      RETURN
      END
 
 
 
 
 
 
