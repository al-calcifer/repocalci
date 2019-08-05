


PROGRAM lanc_obl
IMPLICIT NONE
REAL(KIND=8):: m,b,t0,t1,h,v0,theta,fx,fy,fvx,fvy
REAL(KIND=8):: k1,k2,k3,k4,g
REAL(KIND=8), ALLOCATABLE:: x(:),y(:),vx(:),vy(:),t(:)
INTEGER:: i,n

g=10.0D0
WRITE(*,*) 'Entre com a massa do objeto'
WRITE(*,*) 'm='
READ(*,*) m
WRITE(*,*) 'Entre com o fator b'
WRITE(*,*) 'b='
READ(*,*) b
WRITE(*,*) 'Entre com o intervalo tempo de solucão'
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


DO i=1,n-1
  ! Resolvendo para x
  k1=h*fx(t(i),x(i),vx(i))
  k2=h*fx(t(i)+h*0.5D0,x(i)+k1*0.5D0,vx(i))
  k3=h*fx(t(i)+h*0.5D0,x(i)+k2*0.5D0,vx(i))
  k4=h*fx(t(i)+h,x(i)+k3,vx(i))
  x(i+1)=x(i)+(k1+2.0D0*k2+2.0D0*k3+k4)/6.0D0
  ! Resolvendo para y
  k1=h*fy(t(i),y(i),vy(i))
  k2=h*fy(t(i)+h*0.5D0,y(i)+k1*0.5D0,vy(i))
  k3=h*fy(t(i)+h*0.5D0,y(i)+k2*0.5D0,vy(i))
  k4=h*fy(t(i)+h,y(i)+k3,vy(i))
  y(i+1)=y(i)+(k1+2.0D0*k2+2.0D0*k3+k4)/6.0D0
  ! Resolvendo para vx
  k1=h*fvx(t(i),vx(i),m,b)
  k2=h*fvx(t(i)+h*0.5D0,vx(i)+k1*0.5D0,m,b)
  k3=h*fvx(t(i)+h*0.5D0,vx(i)+k2*0.5D0,m,b)
  k4=h*fvx(t(i)+h,vx(i)+k3,m,b)  
  vx(i+1)=vx(i)+(k1+2.0D0*k2+2.0D0*k3+k4)/6.0D0
  ! Resolvendo para vy
  k1=h*fvy(t(i),vy(i),m,b,g)
  k2=h*fvy(t(i)+h*0.5D0,vy(i)+k1*0.5D0,m,b,g)
  k3=h*fvy(t(i)+h*0.5D0,vy(i)+k2*0.5D0,m,b,g)
  k4=h*fvy(t(i)+h,vy(i)+k3,m,b,g)    
  vy(i+1)=vy(i)+(k1+2.0D0*k2+2.0D0*k3+k4)/6.0D0
END DO

OPEN(UNIT=1,FILE='trajectory.dat')
DO i=1,n
  IF(y(i).lt.0.0D0)THEN
    EXIT
  END IF
  WRITE(1,*) x(i),y(i)
END DO
WRITE(1,*)
CLOSE(UNIT=1)





END PROGRAM lanc_obl

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION fx(t,x,vx)
IMPLICIT NONE
REAL(KIND=8):: x,t,vx,fx

fx=vx

END FUNCTION fx

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION fy(t,y,vy)
IMPLICIT NONE
REAL(KIND=8):: y,t,vy,fy

fy=vy

END FUNCTION fy

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION fvx(t,vx,m,b)
IMPLICIT NONE
REAL(KIND=8):: vx,t,m,b,fvx

fvx=-b*vx/m

END FUNCTION fvx

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION fvy(t,vy,m,b,g)
IMPLICIT NONE
REAL(KIND=8):: vy,t,m,b,fvy,g

fvy=-g-b*vy/m

END FUNCTION fvy




