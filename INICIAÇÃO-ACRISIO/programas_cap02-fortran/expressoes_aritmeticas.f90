PROGRAM expressoes_aritmeticas 
	IMPLICIT NONE 
	
!Esse programa ilustra como o fortran interpreta expressoes aritmeticas
   
!Declaracao de variaveis
	REAL :: x,y
	INTEGER :: i,j
	REAL :: a,b
	INTEGER :: c,d
	
!Divis�o de inteiros
	i=1
	j=3
	
	PRINT*, "Resultado = ", i/j
	
!Divis�o de inteiros atribuida a uma vari�vel real. Cuidado!
	x = i/j
	
	PRINT*, "Resultado = ", x
	
!Expressoes numericas mixtas
	x = 1./3 !3 � promovido a real antes da opera��o
	
	PRINT*, "Resultado = ", x
	
!Truncamento de inteiros sempre na dire��o de zero
	i = 3.9
	x = 3.9
	y = 0.1
	j = (x+y)
	PRINT*, "Resultado = ", i,j
	PRINT*, "Resultado = ", NINT(3.9)
	
!Operacoes aritm�ticas nao s�o comutativas
	b = 100.
	c = 9
	d = 10.
	a = b*c/d
	PRINT*, "Resultado = ", a
	a = c/d*b
	PRINT*, "Resultado = ", a
		
END PROGRAM expressoes_aritmeticas