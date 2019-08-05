program matmultiplicator_lap
implicit none
integer:: i,j,l,na,ma,nb,mb
!parameter (na=3, ma=3)
!parameter (nb=3, mb=4)
real(kind=8), allocatable :: a(:,:),b(:,:),c(:,:) 
real(kind=8):: tmp




!write(*,*) 'reading b'
!open(unit=1,file='b.dat',form='unformatted')
!read(1) nb,mb
!if(ma.ne.nb) stop 'matrices not compatible'
!allocate(b(nb,mb))
!read(1) b
close(unit=1)

write(*,*) 'writing a'
open(unit=1,file='a.dat',form='unformatted')
allocate(a(na,ma))
a(1,1)=1.d0
a(1,2)=2.d0
a(1,3)=3.d0
a(2,1)=3.d0
a(2,2)=2.d0
a(2,3)=1.d0
a(3,1)=1.d0
a(3,2)=1.d0
a(3,3)=1.d0
write(1,*) na, ma
write(1,*) a
close(unit=1)

write(*,*) 'reading a'
open(unit=1,file='a.dat',form='unformatted')
read(1) na,ma
!allocate(a(na,ma))
read(1) a
close(unit=1)


write(*,*) 'writing b'
open(unit=1,file='b.dat',form='unformatted')
allocate(b(nb,mb))
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
write(1,*) nb, mb
write(1,*) b
close(unit=1)

write(*,*) 'reading b'
open(unit=1,file='b.dat',form='unformatted')
read(1) nb,mb
if(ma.ne.nb) stop 'matrices not compatible'
!allocate(b(nb,mb))
read(1) b
close(unit=1)


write(*,*) 'making the multiplication.'
call dgemm('N','N',na,mb,ma,1.0d0,a,na,b,nb,0.0d0,c,na)

!transa- é a determinação da matriz, se no caso for so a matriz simples, usa-se 'n'. caso seja a transposta de da mtriz-a usa-se 't' ou 'c'.
!transb- é a determinação da matriz, se no caso for so a matriz simples, usa-se 'n'. caso seja a transposta de da mtriz-a usa-se 't' ou 'c'.
!na- especifica o numero de linhas da matriz a.
!mb- especifica o numero de colunas da matriz b.
!ma- especifica o numero de colunas d matriz a.
!alpha- sempre vai ser 1 .
!a- sera a matriz a.
!na- é o numero de linhas da matriz a.
!b- é a matriz b.
!nb- é o numero de linhas da matriz b.
!beta- vai ser sempre zero.
!c- é a matriz c.
!na- é o numero de linhas da matriz resultante c.

write(*,*) 'writting c'
open(unit=1,file='matrizc.dat', form='unformatted')
write(1) na,mb
write(1) c
close(unit=1)




end program matmultiplicator_lap




