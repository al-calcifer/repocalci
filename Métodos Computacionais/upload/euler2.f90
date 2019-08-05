


PROGRAM euler
IMPLICIT NONE
REAL(KIND=8):: xnew,ynew,h,f,a,b
REAL(KIND=8),ALLOCATABLE:: x(:),y(:)
INTEGER:: i,n

WRITE(*,*) 'Entre com o intervalo de solucão'
WRITE(*,*) 'a='
READ(*,*) a
WRITE(*,*) 'b='
READ(*,*) b
WRITE(*,*) 'Entre com o número de pontos no intervalo'
WRITE(*,*) 'n='
READ(*,*) n
h=(b-a)/(n-1)
ALLOCATE(x(n),y(n))
WRITE(*,*) 'Entre com a condicão inicial'
WRITE(*,*) 'y(a)='
READ(*,*) y(1)
x(1)=a


OPEN(UNIT=1,FILE='euler.dat')
WRITE(1,*) x(1),y(1)
DO i=1,n-1
  x(i+1)=x(i)+h
  y(i+1)=y(i)+h*f(x(i),y(i))
  WRITE(1,*) x(i+1),y(i+1)
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



