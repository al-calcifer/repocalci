

PROGRAM ellis_5_2
IMPLICIT NONE
REAL:: a,b

PRINT*,'Por favor, digite um número.'
READ*,a
PRINT*,'Por favor, digite outro número.'
READ*,b
IF(a.gt.b)THEN
  PRINT*,'O primeiro número é maior.'
ELSE IF(a.lt.b)THEN
  PRINT*,'O segundo número é maior.'
ELSE
    PRINT*,'Os dois números são iguais.'
END IF

END PROGRAM ellis_5_2
