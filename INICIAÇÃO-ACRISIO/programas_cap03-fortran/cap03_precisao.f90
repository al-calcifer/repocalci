PROGRAM exemplo
!Alex C Carciofi. Fev/2008
!Esse programa mostra um exemplo das consequências da perda de precisao
!acarretada pela subtracao de dois numeros muito proximos entre si.
!O programa calcula os valores da funcao:
!
!	f(x) = (1-cos(x))/x**2
!
!O problema dessa funcao e que para valores de x proximos de 0, cos(x) -> 1
!e 1-cos(x) -> 0
!
!A funcao func_err mostra uma implementacao obvia do programa, mas que retorna
!resultados errados
!
!A funcao func_cor mostra uma implementacao alternativa.

IMPLICIT NONE

INTEGER, PARAMETER :: n = 10	!Numero de valores do argumento da funcao
DOUBLE PRECISION :: x(n)		!Array com os valores do argumento da funcao
!CHARACTER(LEN=*), PARAMETER  :: FMT = "(A,/,5F14.10,/,5F14.10)"
CHARACTER(LEN=*), PARAMETER  :: FMT = "(A,/,2F20.16,/,2F20.16,/,2F20.16,/,2F20.16,/,2F20.16,/)"

!Inicializa falores do argumento
	x = (/1.,2.,3.,5.,10.,20.,30.,50.,5000.,50000./)*1.D-8
	WRITE(6,FMT) "Valores do argumento: ", x
	
!Calcular valor do cosseno
	WRITE(6,FMT) "cos(x) = ", cos(x)

!Calcula funcao errada
	WRITE(6,FMT) "Funcao errada: ", func_err(x)

!Calcula funcao correta
	WRITE(6,FMT) "Funcao correta: ", func_cor(x)

	
CONTAINS

FUNCTION func_err(x)
!Essa funcao implementa o problema da forma mas obvia, mas que retorna 
!valores errados

IMPLICIT NONE

DOUBLE PRECISION, INTENT(in) :: x(:)
DOUBLE PRECISION :: func_err(size(x))

	func_err = (1.D0 - DCOS(x)) / (x*x)
	
	RETURN

END FUNCTION

FUNCTION func_cor(x)
!Essa funcao mostra uma possivel alternativa para o problema.
!Ela faz uso de uma da expensao em series de taylor do cosseno
!
!cos (x) = 1 - x**2/2! + x**4/4! ...
!

IMPLICIT NONE

DOUBLE PRECISION, INTENT(in) :: x(:)
DOUBLE PRECISION :: func_cor(size(x))

INTEGER :: i

	DO i=1,SIZE(x)
		IF (x(i) <= 1.D-8) THEN
!Uso apenas o primeiro termo da serie
			func_cor(i) = 0.5D0 
		ELSEIF (x(i) > 1.D-8 .AND. x(i) <= 1.D-4) THEN
!Uso os dois primeiros termos da serie
			func_cor(i) = 0.5D0 - x(i)*x(i)/(4.*3.*2.) 
		ELSE
!Uso a função cos(x)
			func_cor(i) = (1.D0 - DCOS(x(i))) / (x(i)*x(i))
		ENDIF
		
	ENDDO
	
	RETURN

END FUNCTION

END PROGRAM
