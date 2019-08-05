 


PROGRAM matmaker
IMPLICIT NONE
INTEGER:: i,j,n
REAL(KIND=8),ALLOCATABLE:: a(:,:),b(:,:) 

WRITE(*,*) 'Enter with the size of the matrices'
READ(*,*) n
ALLOCATE(a(n,n),b(n,n))
CALL RANDOM_NUMBER(a)
CALL RANDOM_NUMBER(b)
OPEN(UNIT=1,FILE='a.dat',FORM='UNFORMATTED')
WRITE(1) n,n
WRITE(1) a
CLOSE(UNIT=1)

OPEN(UNIT=2,FILE='b.dat',FORM='UNFORMATTED')
WRITE(2) n,n
WRITE(2) b
CLOSE(UNIT=2)

END PROGRAM matmaker




