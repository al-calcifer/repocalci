


PROGRAM diag_lapack
IMPLICIT NONE
INTEGER:: LWORK,INFO,i
REAL(KIND=8):: A(3,3),W(3)
REAL(KIND=8),ALLOCATABLE:: WORK(:)

A(1,1)=4.0D0
A(1,2)=2.0D0
A(1,3)=2.0D0
A(2,1)=2.0D0
A(2,2)=4.0D0
A(2,3)=2.0D0
A(3,1)=2.0D0
A(3,2)=2.0D0
A(3,3)=4.0D0

! 1st call
ALLOCATE(WORK(1))
CALL DSYEV('V','U',3,A,3,W,WORK,-1,INFO)
LWORK=WORK(1)
DEALLOCATE(WORK)
ALLOCATE(WORK(LWORK))
CALL DSYEV('V','U',3,A,3,W,WORK,LWORK,INFO)

DO i=1,3
  WRITE(*,*) 'Autovalor ',i,'=',W(i)
END DO

DO i=1,3
  WRITE(*,*) 'Autovetor ',i,'=',A(:,i)
END DO


END PROGRAM diag_lapack




