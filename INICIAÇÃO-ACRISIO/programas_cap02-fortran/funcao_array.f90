PROGRAM exemplo
USE teste

IMPLICIT NONE


REAL :: a(5),b(5)

a = (/-1.,5.,0.1,10.,101./)

b = (/25.,-2.,52.,1.E-5,0.01/)

PRINT*, max_array(a,b)

END PROGRAM

MODULE teste
IMPLICIT NONE

CONTAINS

FUNCTION max_array(arr1,arr2)
IMPLICIT NONE

!Argumentos
	REAL, DIMENSION(:) :: arr1,arr2
	
!Variavel resultante
	REAL, DIMENSION(SIZE(arr1)) :: max_array
	
!Usar a funcao MAX para comparar os elementos
	max_array = MAX(arr1,arr2)
	
END FUNCTION

END MODULE


