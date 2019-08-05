


PROGRAM matmultiplicator2
IMPLICIT NONE
INTEGER:: i,j,l,na,ma,nb,mb
REAL(KIND=8),ALLOCATABLE:: a(:,:),b(:,:),c(:,:) 
REAL(KIND=8):: tmp

WRITE(*,*) 'Reading A'
OPEN(UNIT=1,FILE='a.dat',FORM='UNFORMATTED')
READ(1) na,ma
ALLOCATE(a(na,ma))
READ(1) a
CLOSE(UNIT=1)

WRITE(*,*) 'Reading B'
OPEN(UNIT=1,FILE='b.dat',FORM='UNFORMATTED')
READ(1) nb,mb
IF(ma.ne.nb) STOP 'Matrices not compatible'
ALLOCATE(b(nb,mb))
READ(1) b
CLOSE(UNIT=1)

WRITE(*,*) 'Making the multiplication.'
ALLOCATE(c(na,mb))
 c=0.0D0
DO j=1,mb
  DO l=1,ma
    tmp=b(l,j)
    DO i=1,na      
      c(i,j)=c(i,j)+a(i,l)*tmp
    END DO
  END DO
END DO

WRITE(*,*) 'Writting C'
OPEN(UNIT=1,FILE='c.dat',FORM='UNFORMATTED')
WRITE(1) na,mb
WRITE(1) c
CLOSE(UNIT=1)




END PROGRAM matmultiplicator2




