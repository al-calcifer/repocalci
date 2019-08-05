

PROGRAM ellis_6_10b
IMPLICIT NONE
INTEGER:: n,limit,i,factor,aux

DO n=2,32768
  ! Limite para a busca de divisores
  limit=n**0.5
  ! Valor inicial do fator
  factor=1
  ! Busca por divisores
  DO i=2,limit
    aux=n/i
    aux=aux*i
    IF(aux.eq.n)THEN
      ! Se passou no teste, o número não é primo
      factor=i
      EXIT
    END IF
  END DO
  IF(factor.eq.1)THEN
    PRINT*,n
  END IF
END DO


END PROGRAM ellis_6_10b
