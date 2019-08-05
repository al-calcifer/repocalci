program matmultiplicator_lap
implicit none
integer:: i,j,l,na,ma,nb,mb
real(kind=8),allocatable:: a(:,:),b(:,:),c(:,:) 
real(kind=8):: tmp

write(*,*) 'reading a'
open(unit=1,file='a.dat',form='unformatted')
read(1) na,ma
print*,na,ma
allocate(a(na,ma))
read(1) a
close(unit=1)

write(*,*) 'reading b'
open(unit=1,file='b.dat',form='unformatted')
read(1) nb,mb
if(ma.ne.nb) stop 'matrices not compatible'
allocate(b(nb,mb))
read(1) b
close(unit=1)

write(*,*) 'making the multiplication.'
allocate(c(na,mb))
call dgemm('n','n',na,mb,ma,1.0d0,a,na,b,nb,0.0d0,c,na)

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
open(unit=1,file='c.dat',form='unformatted')
write(1) na,mb
write(1) c
close(unit=1)




end program matmultiplicator_lap




