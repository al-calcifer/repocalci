PROGRAM exercicio1 
	IMPLICIT NONE 
	
!Calcular a parte inteira e fracionaria de um número inteiro
   
!Declaracao de variaveis
	REAL :: x,frac
	INTEGER :: i
	
!Ler o numero real	
	PRINT *,"Entre um numero: " 
	READ *, x
	
!Armazena a parte inteira na variável i
	i = x
		
!Calcula a parte fracionaria
	frac = x - i
	
!Escrever o resultado
	PRINT*, "Numero fornecido = ", x
	PRINT*, "Parte inteira = ", i
	PRINT*, "Parte fracionaria = ", frac
		
END PROGRAM exercicio1