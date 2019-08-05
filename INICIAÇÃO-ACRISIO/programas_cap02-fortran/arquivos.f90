PROGRAM arquivos
!Programa de exemplo para se mostrar como se abre e fecha arquivos.
!Usa o mesmo codigo que o programa formatos.f90
IMPLICIT NONE

	REAL :: arg, raiz2, raiz3, raiz4, raiz5
	INTEGER :: i
	CHARACTER(LEN=10) :: arquivo

!Calcular o valor das raizes do argumento arg
	WRITE(*,"(A,$)") "Entre com valor do argumento: "
	READ*, arg
	
!Chama a subrotina raizes
	CALL raizes(arg, raiz2, raiz3, raiz4, raiz5)

!Le o nome do arquivo de saida
	WRITE(*,"(A,$)") "Entre com o nome do arquivo: "
	READ*, arquivo
	
	OPEN(UNIT=7, FILE=arquivo, STATUS="NEW")
	
	
	WRITE(7,"(5A16)") "Argumento", "Raiz Quadrada", "Raiz Cubica", &
				   "Raiz Quadrupla", "Raiz Quintupla" 

!Exemplo de formatacao usando o descritor F				   
	WRITE(7,"(5F16.4)") arg,raiz2, raiz3, raiz4, raiz5
	
!Exemplo de formatacao usando o descritor E				   
	WRITE(7,"(5E16.4)") arg,raiz2, raiz3, raiz4, raiz5
	
!Exemplo de formatacao usando inteiros
	WRITE(7,"(A,I1,A,F6.4)") "x**1/",2," = ",raiz2
	WRITE(7,"(A,I1,A,F6.4)") "x**1/",3," = ",raiz3
	WRITE(7,"(A,I1,A,F6.4)") "x**1/",4," = ",raiz4
	WRITE(7,"(A,I1,A,F6.4)") "x**1/",5," = ",raiz5
	
	CLOSE(UNIT=7)

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