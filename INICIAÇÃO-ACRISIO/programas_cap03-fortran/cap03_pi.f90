PROGRAM exemplo
!Alex C Carciofi. Fev/2008
!Esse programa calcula o valor de pi usando uma expansão em séries.
!São mostradas soluções que usam um crescente número de termos da série.
!O programa ilustra erros de truncamento e também de perda de precisão,
!pois é mostrado que a partir de um certo número de termos da série o
!resultado final não mais se altera.

IMPLICIT NONE

INTEGER(4) :: i,j!,n		!Número de termos a serem somados
INTEGER(4) :: n(10)

REAL :: pi_cal, pi_real
REAL :: denominador, sinal
REAL :: err_abs, err_rel
CHARACTER(LEN=*), PARAMETER  :: FMT='(A,F20.7)'

	pi_real = 3.1415926535897932384626433832795


!	WRITE(6,'(A,$)') "Entre o numero de termos da serie: "
!	READ(*,*) n
	n = (/1,10,100,1000,10000,100000,1000000,10000000,100000000,1000000000/)

	DO j=1,10
!
		pi_cal = 0.D0
		sinal = 1.D0
		denominador = 1.D0

		DO i=1,n(j)
			pi_cal = pi_cal + sinal/denominador
			sinal = sinal*(-1.D0)
			denominador = denominador + 2.D0
		END DO
	
		pi_cal = pi_cal * 4.D0
	
		err_abs = pi_cal - pi_real
		err_rel = err_abs/pi_real
	
		WRITE(6,'(/,A,I10)')   "Numero de termos = ", n(j)
		WRITE(6,FMT) "Valor correto de pi = ", pi_real
		WRITE(6,FMT) "Valor calculado de pi = ", pi_cal
		WRITE(6,FMT) "Erro absoluto = ", err_abs
		WRITE(6,FMT) "Erro relativo = ", err_rel	
	ENDDO

END PROGRAM
