

PROGRAM ellis_7_1
IMPLICIT NONE
INTEGER:: n,a(20),i

PRINT*,'Quantos números vc quer digitar?'
READ*,n
IF(n.gt.20)THEN
  PRINT*, 'Escolha no máximo 20 números'
  STOP
END IF

DO i=1,n
  READ*,a(i)
END DO

PRINT*,'Os números em ordem reversa são:'
DO i=n,1,-1
  PRINT*,a(i)
END DO

END PROGRAM ellis_7_1
