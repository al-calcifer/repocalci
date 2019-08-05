

PROGRAM ellis_5_5
IMPLICIT NONE
INTEGER:: i

PRINT*,'Digite um número entre 1 e 6.'
READ*,i

IF(i.eq.1)THEN
  PRINT*,'One'
ELSE IF(i.eq.2)THEN
  PRINT*,'Two'
ELSE IF(i.eq.3)THEN
  PRINT*,'Three'
ELSE IF(i.eq.4)THEN
  PRINT*,'Four'
ELSE IF(i.eq.5)THEN
  PRINT*,'Five'
ELSE IF(i.eq.6)THEN
  PRINT*,'Six'
ELSE
  PRINT*,'O número está fora do intervalo pedido.'
END IF
  
END PROGRAM ellis_5_5
