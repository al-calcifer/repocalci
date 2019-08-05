


PROGRAM loop_example
IMPLICIT NONE
INTEGER::i,soma

soma=0
DO i=1,10
  soma=soma+i
  IF(soma.gt.20)THEN
    EXIT
  END IF
END DO
PRINT*,'saída em i=',i
PRINT*,'Soma=',soma



END PROGRAM loop_example






