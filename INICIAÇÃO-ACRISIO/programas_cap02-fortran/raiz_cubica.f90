PROGRAM exemplo
!Programa de exemplo para usar a funcao raiz cubica
IMPLICIT NONE

	REAL :: arg
!Declaracao da funcao externa
	REAL, EXTERNAL :: raiz_cubica

	PRINT*, "Entre com valor do argumento: "
	READ*, arg
	
	PRINT*, "Raiz cubica = ", raiz_cubica(arg)
	
END PROGRAM

REAL FUNCTION raiz_cubica(x)

IMPLICIT NONE 

!Declaracao do argumento da funcao
	REAL, INTENT(in) :: x
	
!Variaveis locais
	REAL :: log_x
	
!Calcula a raiz cúbica usando logaritmos
	log_x = LOG(x)
	
	raiz_cubica = EXP(log_x/3.)

END FUNCTION raiz_cubica