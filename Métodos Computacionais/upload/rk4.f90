


PROGRAM lanc_obl
IMPLICIT NONE
REAL(KIND=8):: t0,t1,h,v0,theta 
REAL(KIND=8),ALLOCATABLE:: x(:),y(:),vx(:),vy(:),t(:)
INTEGER:: i,n



WRITE(*,*) 'Entre com o intervalo de solucão'
WRITE(*,*) 't0='
READ(*,*) t0
WRITE(*,*) 't1='
READ(*,*) t1
WRITE(*,*) 'Entre com o número de pontos no intervalo'
WRITE(*,*) 'n='
READ(*,*) n
h=(t1-t0)/(n-1)
ALLOCATE(x(n),y(n),vx(n),vy(n),t(n))

WRITE(*,*) 'Entre com a posicão de lancamento'
WRITE(*,*) 'x0='
READ(*,*) x(1)
WRITE(*,*) 'y0='
READ(*,*) y(1)
WRITE(*,*) 'Entre com a velocidade de lancamento'
WRITE(*,*) 'v0='
READ(*,*) v0
WRITE(*,*) 'Entre com o ângulo de lancamento em graus'
WRITE(*,*) 'theta='
READ(*,*) theta
theta=theta*DACOS(-1.0D0)/180.0D0
vx(1)=v0*DCOS(theta)
vy(1)=v0*DSIN(theta)


OPEN(UNIT=1,FILE='rk4.dat')
WRITE(1,*) x(1),y(1)
DO i=1,n-1
  x(i+1)=x(i)+h
  k1=h*f(x(i),y(i))
  k2=h*f(x(i)+h*0.5D0,y(i)+k1*0.5D0)
  k3=h*f(x(i)+h*0.5D0,y(i)+k2*0.5D0)
  k4=h*f(x(i)+h,y(i)+k3)
  y(i+1)=y(i)+(k1+2.0D0*k2+2.0D0*k3+k4)/6.0D0
  WRITE(1,*) x(i+1),y(i+1)
END DO
WRITE(1,*)
 CLOSE(UNIT=1)





END PROGRAM lanc_obl

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION f(x,y)
IMPLICIT NONE
REAL(KIND=8):: x,y,f

f=1.0D0-x+4.0D0*y

END FUNCTION f



