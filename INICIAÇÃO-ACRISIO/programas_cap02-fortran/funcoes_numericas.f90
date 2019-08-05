PROGRAM funcoes_numericas 
	IMPLICIT NONE 
	
	REAL :: x
	DOUBLE PRECISION :: y
	INTEGER :: i
	INTEGER*8 :: j
	
	PRINT*, & 
"O menor numero positivo que somado a 1 retorna numero maior que 1: "
	PRINT*, EPSILON(x), EPSILON(y)  

	PRINT*, &
	"O maior numero positivo que pode ser representado: "
	PRINT*, HUGE(x),HUGE(y),HUGE(i),HUGE(j)
	
	PRINT*, &
	"O menor numero positivo que pode ser representado: : "
	PRINT*, TINY(x),TINY(y)
	
END PROGRAM funcoes_numericas