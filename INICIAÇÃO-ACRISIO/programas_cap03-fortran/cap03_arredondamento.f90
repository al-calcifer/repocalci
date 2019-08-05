PROGRAM exemplo

IMPLICIT NONE

REAL :: x,y
CHARACTER(LEN=*), PARAMETER :: FMT='(A,F10.7)'

INTEGER :: i,n

	
	PRINT*, "Exemplo de problemas de arredondamento."
	WRITE(6,"(A,$)") "Entre com numero real: "
	READ*, x
	
	y = SQRT(x)
	
	WRITE(6, FMT) "x = ", x
	WRITE(6, FMT) "y = SQRT(x) = ", y
	WRITE(6, FMT) "y*y = ", y*y
	WRITE(6, FMT) "x - y*y = ", x - y*y
	
END PROGRAM
