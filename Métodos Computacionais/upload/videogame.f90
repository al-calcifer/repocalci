PROGRAM videogame
IMPLICIT NONE
REAL(KIND=8):: W,L,r,frame(10),river(10),basis,x(10000,2),v,vb(2),vr(2),dt,theta,dtheta,shift
INTEGER:: i,j,n

W=100.0
L=250.0
r=10.0
basis=MAX(L,W)
shift=0.5*basis
x=0.0D0
j=1
x(1,1)=basis-0.5*W-shift
x(1,2)=0.7*basis 


frame(1)=0.0
frame(2)=0.0
frame(3)=2.0*basis
frame(4)=0.0
frame(5)=2.0*basis
frame(6)=1.4*basis
frame(7)=0.0
frame(8)=1.4*basis
frame(9)=0.0
frame(10)=0.0

river(1)=basis-0.5*W-shift
river(2)=0.7*basis-0.5*L
river(3)=basis+0.5*W-shift
river(4)=0.7*basis-0.5*L
river(5)=basis+0.5*W-shift
river(6)=0.7*basis+0.5*L
river(7)=basis-0.5*W-shift
river(8)=0.7*basis+0.5*L
river(9)=basis-0.5*W-shift
river(10)=0.7*basis-0.5*L


OPEN(UNIT=1,FILE='trajectory.dat')
WRITE(1,'(2F17.7)') frame
WRITE(1,*)
WRITE(1,'(2F17.7)') river
WRITE(1,*) 
WRITE(1,*) basis-0.5*W-shift,0.7*basis 
WRITE(1,*) basis+0.5*W-shift,0.7*basis 
WRITE(1,*) 
WRITE(1,*) 
DO i=0,360
  WRITE(1,*) basis-0.5*W-2*r+r*DCOS(DACOS(-1.0D0)*DFLOAT(i)/180.0)-shift,0.7*basis+r*DSIN(DACOS(-1.0D0)*DFLOAT(i)/180.0) 
END DO
WRITE(1,*)
DO i=0,360
  WRITE(1,*) basis+0.5*W+2*r+r*DCOS(DACOS(-1.0D0)*DFLOAT(i)/180.0)-shift,0.7*basis+r*DSIN(DACOS(-1.0D0)*DFLOAT(i)/180.0) 
END DO
WRITE(1,*)
DO i=1,j
  WRITE(1,*) x(i,:)
END DO
WRITE(1,*)
CLOSE(UNIT=1)


vr(1)=0.0D0
vr(2)=1.0D0
dt=5.0
theta=0.0D0
v=2.0


DO n=1,100
  WRITE(*,*) 'Enter an angle variation in degrees'
  WRITE(*,*) 'Input an integer value from -3 to 3'
  READ(*,*) dtheta
  theta=theta+dtheta*DACOS(-1.0D0)/180.0
  j=j+1
  vb(1)=v*DCOS(theta)
  vb(2)=v*DSIN(theta)
  x(j,:)=x(j-1,:)+dt*(vb+vr)
  IF(x(j,1).gt.(basis+0.5*W-shift)) EXIT
  
  

  OPEN(UNIT=1,FILE='trajectory.dat')
  WRITE(1,'(2F17.7)') frame
  WRITE(1,*)
  WRITE(1,'(2F17.7)') river
  WRITE(1,*) 
  WRITE(1,*) basis-0.5*W-shift,0.7*basis 
  WRITE(1,*) basis+0.5*W-shift,0.7*basis 
  WRITE(1,*) 
  WRITE(1,*) 
  DO i=0,360
    WRITE(1,*) basis-0.5*W-2*r+r*DCOS(DACOS(-1.0D0)*DFLOAT(i)/180.0)-shift,0.7*basis+r*DSIN(DACOS(-1.0D0)*DFLOAT(i)/180.0) 
  END DO
  WRITE(1,*)
  DO i=0,360
    WRITE(1,*) basis+0.5*W+2*r+r*DCOS(DACOS(-1.0D0)*DFLOAT(i)/180.0)-shift,0.7*basis+r*DSIN(DACOS(-1.0D0)*DFLOAT(i)/180.0) 
  END DO
  WRITE(1,*)
  DO i=1,j
    WRITE(1,*) x(i,:)
  END DO
  WRITE(1,*)
  CLOSE(UNIT=1)
  
  CALL SYSTEM('xmgrace trajectory.dat')
  
  
  
END DO

WRITE(*,*) x(j-1,2)-0.7*basis







END PROGRAM videogame
