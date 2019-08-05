


PROGRAM gravitation
IMPLICIT NONE
REAL(KIND=8):: GM,t0,t1,h,v0,theta,fx,fy,fvx,fvy
REAL(KIND=8):: k1,k2,k3,k4,g,r
REAL(KIND=8),ALLOCATABLE:: x(:),y(:),vx(:),vy(:),t(:)
INTEGER:: i,n

WRITE(*,*) 'Entre com GM'
WRITE(*,*) 'GM='
READ(*,*) GM
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
WRITE(*,*) 'vx0='
READ(*,*) vx(1)
WRITE(*,*) 'vy0='
READ(*,*) vy(1)


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
  k1=h*fvx(t(i),vx(i),x(i),y(i),GM)
  k2=h*fvx(t(i)+h*0.5D0,vx(i)+k1*0.5D0,x(i),y(i),GM)
  k3=h*fvx(t(i)+h*0.5D0,vx(i)+k2*0.5D0,x(i),y(i),GM)
  k4=h*fvx(t(i)+h,vx(i)+k3,x(i),y(i),GM)  
  vx(i+1)=vx(i)+(k1+2.0D0*k2+2.0D0*k3+k4)/6.0D0
  ! Resolvendo para vy
  k1=h*fvy(t(i),vy(i),x(i),y(i),GM)
  k2=h*fvy(t(i)+h*0.5D0,vy(i)+k1*0.5D0,x(i),y(i),GM)
  k3=h*fvy(t(i)+h*0.5D0,vy(i)+k2*0.5D0,x(i),y(i),GM)
  k4=h*fvy(t(i)+h,vy(i)+k3,x(i),y(i),GM)    
  vy(i+1)=vy(i)+(k1+2.0D0*k2+2.0D0*k3+k4)/6.0D0
END DO

OPEN(UNIT=1,FILE='orbit.dat')
DO i=1,n
  r=DSQRT(x(i)*x(i)+y(i)*y(i))
  IF(r.lt.0.1) EXIT
  WRITE(1,*) x(i),y(i)
END DO
WRITE(1,*)
DO i=0,360
  WRITE(1,*) 0.1*DCOS(i*DACOS(-1.0D0)/180),0.1*DSIN(i*DACOS(-1.0D0)/180)
END DO
WRITE(1,*)

CLOSE(UNIT=1)





END PROGRAM gravitation

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

FUNCTION fvx(t,vx,x,y,GM)
IMPLICIT NONE
REAL(KIND=8):: fvx,vx,t,x,y,GM,r

r=DSQRT(x*x+y*y)
fvx=-GM*x/(r**3)

END FUNCTION fvx

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION fvy(t,vy,x,y,GM)
IMPLICIT NONE
REAL(KIND=8):: fvy,vy,t,x,y,GM,r

r=DSQRT(x*x+y*y)
fvy=-GM*y/(r**3)

END FUNCTION fvy




