PROGRAM picalc
IMPLICIT NONE
REAL(KIND=8):: x1,x2,x3,x4,x5,fmed 
INTEGER:: i,n

n=10000000
fmed=0.0D0
DO i=1,n
  CALL RANDOM_NUMBER(x1)
  CALL RANDOM_NUMBER(x2)
  CALL RANDOM_NUMBER(x3)
  CALL RANDOM_NUMBER(x4)
  CALL RANDOM_NUMBER(x5)
  fmed=fmed+(x1+x2+x3+x4+x5)**2
END DO
fmed=fmed/DFLOAT(n)

WRITE(*,*) 'O valor aproximado da integral é ',fmed
WRITE(*,*) 'O valor analítico da integral é  ',20.0D0/3.0D0

END PROGRAM picalc






