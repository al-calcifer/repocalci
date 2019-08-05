


PROGRAM euler
IMPLICIT NONE
REAL(KIND=8):: x,y,xnew,ynew,h,f
INTEGER:: i

h=0.0001D0
x=0.0D0
y=1.0D0

OPEN(UNIT=1,FILE='euler.dat')
WRITE(1,*) x,y
DO i=1,10000
  xnew=x+h
  ynew=y+h*f(x,y)
  WRITE(1,*) xnew,ynew
  x=xnew
  y=ynew
END DO
WRITE(1,*)

DO i=0,10000
  x=i*h
  y=0.25D0*x-3.0D0/16.0D0+(19.0D0/16.0D0)*DEXP(4.0D0*x)
  WRITE(1,*) x,y
END DO
WRITE(1,*)
CLOSE(UNIT=1)

END PROGRAM euler

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION f(x,y)
IMPLICIT NONE
REAL(KIND=8):: x,y,f

f=1.0D0-x+4.0D0*y

END FUNCTION f



