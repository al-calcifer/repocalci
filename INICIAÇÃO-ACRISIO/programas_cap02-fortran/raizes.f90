PROGRAM exemplo
!Programa de exemplo para usar a subrotina raizes
IMPLICIT NONE

	REAL :: arg, raiz2, raiz3, raiz4, raiz5

	PRINT*, "Entre com valor do argumento: "
	READ*, arg
	
!Chama a subrotina
	CALL raizes(arg, raiz2, raiz3, raiz4, raiz5)
	
	PRINT*, "Raizes = ", raiz2, raiz3, raiz4, raiz5
	
END PROGRAM


SUBROUTINE raizes(x,x2,x3,x4,x5)
!Essa subrotina calcula varias raizes da variavel x

IMPLICIT NONE 

!Declaracao do input 
	REAL, INTENT(in) :: x
	
!Declaracao do output
	REAL, INTENT(out) :: x2,x3,x4,x5
	
!Variaveis locais
	REAL :: log_x
	
!Calcula raiz quadrada
	x2 = SQRT(x)
	
!Calcula as outras raizes usando logaritmos
	log_x = LOG(x)
	
	x3 = EXP(log_x/3.)
	x4 = EXP(log_x/4.)
	x5 = EXP(log_x/5.)

END SUBROUTINE raizes