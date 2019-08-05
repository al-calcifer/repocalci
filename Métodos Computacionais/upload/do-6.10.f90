

PROGRAM ellis_6_10
IMPLICIT NONE
INTEGER:: n,limit,i,factor,aux

! Input de dados
PRINT*,'Por favor, digite um inteiro positivo'
READ*,n
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

PRINT*,'O primeiro fator é',factor



END PROGRAM ellis_6_10
