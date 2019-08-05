


PROGRAM ellis_9_5
IMPLICIT NONE
INTEGER:: a(100),i,j,n

OPEN(UNIT=1,FILE='data.in')
DO i=1,100
  READ(1,*,IOSTAT=j) a(i)
  IF(j.ne.0)THEN
    n=i-1
    EXIT
  END IF
END DO
CLOSE(UNIT=1)

OPEN(UNIT=2,FILE='data.out')
DO i=n,1,-1
  WRITE(2,*) a(i)
END DO
CLOSE(UNIT=2)



END PROGRAM ellis_9_5






