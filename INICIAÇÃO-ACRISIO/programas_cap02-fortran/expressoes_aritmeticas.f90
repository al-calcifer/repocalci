PROGRAM expressoes_aritmeticas 
	IMPLICIT NONE 
	
!Esse programa ilustra como o fortran interpreta expressoes aritmeticas
   
!Declaracao de variaveis
	REAL :: x,y
	INTEGER :: i,j
	REAL :: a,b
	INTEGER :: c,d
	
!Divisão de inteiros
	i=1
	j=3
	
	PRINT*, "Resultado = ", i/j
	
!Divisão de inteiros atribuida a uma variável real. Cuidado!
	x = i/j
	
	PRINT*, "Resultado = ", x
	
!Expressoes numericas mixtas
	x = 1./3 !3 é promovido a real antes da operação
	
	PRINT*, "Resultado = ", x
	
!Truncamento de inteiros sempre na direção de zero
	i = 3.9
	x = 3.9
	y = 0.1
	j = (x+y)
	PRINT*, "Resultado = ", i,j
	PRINT*, "Resultado = ", NINT(3.9)
	
!Operacoes aritméticas nao são comutativas
	b = 100.
	c = 9
	d = 10.
	a = b*c/d
	PRINT*, "Resultado = ", a
	a = c/d*b
	PRINT*, "Resultado = ", a
		
END PROGRAM expressoes_aritmeticas