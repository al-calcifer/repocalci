PROGRAM diag_4x4
IMPLICIT NONE
REAL(KIND=8):: A(4:4)
INTEGER::

A(1,1)=3.0d0
A(1,2)=3.0d0
A(1,3)=0.0d0
A(1,4)=0.0d0
A(2,1)=3.0d0
A(2,2)=3.0d0
A(2,3)=0.0d0
A(2,4)=0.0d0
A(3,1)=0.0d0
A(3,2)=0.0d0
A(3,3)=3.0d0
A(3,4)=3.0d0
A(4,1)=0.0d0
A(4,2)=0.0d0
A(4,3)=3.0d0
A(4,4)=3.0d0


!PARA A PRIMEIRA CHAMADA 

ALLOCATE(WORK(1))
 CALL DSYEV( JOBZ "V", "U", 4, A, LDA"4", W, WORK, LWORK"-1", INFO )
DEALLOCATE(WORK)

!SEGUANDA CHAMADA 
ALLOCATE(LWORK)
 CALL DSYEV( v, U, 4, A, 4, W, WORK, LWORK, INFO )


DO i=1, 4
write "autovalor",i, "=",w(i)
end do


DO i=1, 4
write "autovetor",i, "=", A(:,i)
end do 

END PROGRAM diag_4x4
