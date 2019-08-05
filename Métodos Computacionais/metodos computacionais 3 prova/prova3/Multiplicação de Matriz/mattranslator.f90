


PROGRAM mattranslator
IMPLICIT NONE
INTEGER:: i,j,n,m
REAL(KIND=8),ALLOCATABLE:: a(:,:),b(:,:) 

OPEN(UNIT=1,FILE='a.dat',FORM='UNFORMATTED')
READ(1) n,m
ALLOCATE(a(n,m))
READ(1) a
CLOSE(UNIT=1)

OPEN(UNIT=1,FILE='a_form.dat')
WRITE(1,*) n,m
DO i=1,n
  WRITE(1,*) (a(i,j),j=1,m)
END DO
WRITE(1,*)
CLOSE(UNIT=1)

OPEN(UNIT=1,FILE='b.dat',FORM='UNFORMATTED')
READ(1) n,m
ALLOCATE(b(n,m))
READ(1) b
CLOSE(UNIT=1)

OPEN(UNIT=1,FILE='b_form.dat')
WRITE(1,*) n,m
DO i=1,n
  WRITE(1,*) (b(i,j),j=1,m)
END DO
WRITE(1,*)
CLOSE(UNIT=1)

END PROGRAM mattranslator




