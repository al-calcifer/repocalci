


PROGRAM ellis_9_2
IMPLICIT NONE
INTEGER:: a(10),i

OPEN(UNIT=1,FILE='data.in')
DO i=1,10
  READ(1,*) a(i)
END DO
CLOSE(UNIT=1)

OPEN(UNIT=2,FILE='data.out')
DO i=10,1,-1
  WRITE(2,*) a(i)
END DO
CLOSE(UNIT=2)



END PROGRAM ellis_9_2






