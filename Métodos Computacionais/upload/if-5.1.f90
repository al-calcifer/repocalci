

PROGRAM ellis_5_1
IMPLICIT NONE
REAL:: a

PRINT*,'Por favor, digite um número.'
READ*,a
IF(a.gt.0.0)THEN
  PRINT*,'Este número é positivo.'
ELSE IF(a.lt.0.0)THEN
  PRINT*,'Este número é negativo.'
ELSE
    PRINT*,'Este número é zero.'
END IF




END PROGRAM ellis_5_1
