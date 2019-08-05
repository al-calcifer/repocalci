

PROGRAM ellis_6_2
IMPLICIT NONE
INTEGER:: initial,i,j,tmp

PRINT*,'Entre com o ponto de início da contagem.'
READ*,initial
DO i=initial,0,-1
  IF(i.eq.0)THEN
    PRINT*,'Buuuuuuuuummmmmmm!!!!'
  ELSE
    tmp=0
    DO j=1,100000000
      tmp=tmp+j
    END DO
    PRINT*,'Contagem em',i
  END IF
END DO
  
END PROGRAM ellis_6_2
