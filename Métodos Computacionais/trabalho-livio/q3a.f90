Program q3a
implicit none
real(kind=8):: tmp,dist,dist2,distance,distance2,t0,t,v(2),dist_new
real(kind=8):: arhenius,probabilidade
real(kind=8),allocatable:: r(:,:)
integer:: n,i,nt,i1,i2

open(unit=1,file='cities.dat')
read(1,*) n
allocate(r(n,2))
do i=1,n
  read(1,*) r(i,:)
end do
close(unit=1)

dist=distance(n,r)

t0=10.d0
nt=100000000
do i=nt,1,-1
  if(mod(i,(nt/100)).eq.0) write(*,*) 100-i/(nt/100),'%' 
  t=t0*dfloat(i)/dfloat(nt)
  call random_number(tmp)
  i1=ceiling(tmp*n)
  call random_number(tmp)
  i2=ceiling(tmp*n)
  v=r(i1,:)
  r(i1,:)=r(i2,:)
  r(i2,:)=v
  dist_new=distance(n,r)
  ! arhenius
  arhenius=dexp(-(dist_new-dist)/t)
  ! probabilidade aleatória
  call random_number(probabilidade)
  ! aplicar o critério
  if(arhenius.ge.probabilidade)then
    dist=dist_new
  else
    v=r(i1,:)
    r(i1,:)=r(i2,:)
    r(i2,:)=v    
  end if
end do

do i=1,n
  write(*,*) 'cidade',i,'->',r(i,:)
end do
  write(*,*) 'distância total=',dist


end program 

!*********************************************************

FUNCTION distance(n,r)
IMPLICIT NONE
REAL(KIND=8):: r(n,2),distance
INTEGER:: n,i

distance=0.0D0
DO i=1,n-1
  distance=distance+DSQRT((r(i,1)-r(i+1,1))**2+(r(i,2)-r(i+1,2))**2)
END DO
distance=distance+DSQRT((r(1,1)-r(8,1))**2+(r(1,2)-r(8,2))**2)

END FUNCTION
