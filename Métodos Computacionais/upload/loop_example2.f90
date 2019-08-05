


PROGRAM loop_example
IMPLICIT NONE
INTEGER::i,j,produto

DO i=1,10
  DO j=1,10
    produto=i*j
    PRINT*,'Produto de',i,'e',j,'vale',produto
  END DO
END DO


END PROGRAM loop_example






