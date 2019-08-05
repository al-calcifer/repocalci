


PROGRAM ellis_5_6
IMPLICIT NONE
INTEGER:: number,aux

! Dado de entrada (input)
PRINT*,'Por favor, digite um número inteiro.'
READ*, number

! Teste a (par ou ímpar)
aux=number/2
aux=aux*2
IF(aux.eq.number)THEN
  PRINT*,'O número é par.'
ELSE IF(aux.ne.number)THEN
  PRINT*,'O número é ímpar.'  
END IF

! Teste b (divisão por 7)
aux=number/7
aux=aux*7
IF(aux.eq.number)THEN
  PRINT*,'O número é divisível por 7.'
ELSE IF(aux.ne.number)THEN
  PRINT*,'O número não é divisível por 7.'  
END IF

! Teste c (quadrado perfeito)
aux=number**0.5
aux=aux**2
IF(aux.eq.number)THEN
  PRINT*,'O número é um quadrado perfeito.'
ELSE IF(aux.ne.number)THEN
  PRINT*,'O número não é um quadrado perfeito.'  
END IF

END PROGRAM ellis_5_6






