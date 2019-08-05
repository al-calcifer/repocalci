program questao !
implicit none
integer:: i,j,l,na,ma,nb,mb
real(kind=8),allocatable:: a(:,:),b(:,:),c(:,:) 
real(kind=8):: tmp
na=3
ma=3
nb=3
mb=4

allocate(a(na,ma))
write(*,*) 'reading a'
a(1,1)=1.d0
a(1,2)=2.d0
a(1,3)=3.d0
a(2,1)=3.d0
a(2,2)=2.d0
a(2,3)=1.d0
a(3,1)=1.d0
a(3,2)=1.d0
a(3,3)=1.d0


allocate(b(nb,mb))
write(*,*) 'reading b'
b(1,1)=1.d0
b(1,2)=2.d0
b(1,3)=3.d0
b(1,4)=4.d0
b(2,1)=3.d0
b(2,2)=2.d0
b(2,3)=1.d0
b(2,4)=0.d0
b(3,1)=1.d0
b(3,2)=1.d0
b(3,3)=1.d0
b(3,4)=1.d0


write(*,*) 'making the multiplication.'
allocate(c(na,mb))
 c=0.0d0
do j=1,mb
  do l=1,ma
    tmp=b(l,j)
    do i=1,na      
      c(i,j)=c(i,j)+a(i,l)*tmp
    end do
  end do
end do

write(*,*) 'writting c'
open(unit=1,file='c.dat',form='unformatted')
write(1) na,mb
write(1) c
close(unit=1)




end program 




