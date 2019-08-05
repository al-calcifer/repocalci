PROGRAM teste
IMPLICIT NONE

INTEGER :: n,m,r1,r2,dr,s1,s2,L,p,A,B,i

PRINT*, '(n,m)'
READ*, A, B


DO i=1, 20
	IF (A .GE. B) THEN
	L=A-B
		IF (L .EQ. 0) THEN
		PRINT*, A, B
		ELSE
		A=A-B
		END IF
	ELSE
	L=B-A
		IF (L .EQ. 0) THEN
		PRINT*, A, B
		ELSE
		B=B-A
		END IF
	END IF
END DO

	


!r1=(2*m+n)
!r2=(2*n+m)
!r1=348
!r2=156
!DO WHILE (r1 .GT. r2)
!r1=r1-r2
!END DO

!DO WHILE (r2 .GT. r1)
!	r2=r2-r1
!END DO
!WRITE(*,*) r1,r2

END PROGRAM
