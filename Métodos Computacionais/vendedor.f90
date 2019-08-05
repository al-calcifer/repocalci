Program vendedor
implicit none
real(kind=8):: tmp,dist,distance,t0,t,v(2),dist_new
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

t0=2.0d0
nt=1000000000
do i=nt,1,-1
  if(mod(i,(nt/100)).eq.0) write(*,*) 100-i/(nt/100),'%' 
  t=t0*dfloat(i)/dfloat(nt)
  call random_number(tmp)
  i1=ceiling(tmp*n)
	if (i1 .eq. 1.d0 .or. i1 .eq. 8.d0) then
		cycle
	end if
  call random_number(tmp)
	if (i2 .eq. 1.d0 .or. i2 .eq. 8.d0) then
		cycle
	end if
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


end program vendedor

!*********************************************************

FUNCTION distance(n,r)
IMPLICIT NONE
REAL(KIND=8):: r(n,2),distance
INTEGER:: n,i

distance=0.0D0
DO i=1,n-1
  distance=distance+DSQRT((r(i,1)-r(i+1,1))**2+(r(i,2)-r(i+1,2))**2)
END DO

END FUNCTION
