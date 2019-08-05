


PROGRAM loop_example
IMPLICIT NONE
INTEGER::i,n
REAL::a1,a2,q,soma

a1=1.0
q=0.5
n=40
soma=a1
DO i=2,n
  a2=a1*q
  soma=soma+a2
  a1=a2
END DO

PRINT*,'Soma=',soma



END PROGRAM loop_example






