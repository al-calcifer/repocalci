PROGRAM exercicio2
	IMPLICIT NONE 
	
!Calcular o valor de pi com uma precisao definida pelo usuário

	REAL :: prec
	REAL :: pi_atual, pi_anterior
	REAL :: denominador, sinal
	INTEGER :: contador
	
!Ler o numero real	
	PRINT *,"Entre a precisao desejada para o valor de pi: " 
	READ *, prec
	
	contador = 0

	pi_anterior = 0.
	pi_atual = 0.
	
	sinal = 1.
	denominador = 1.

	DO 
		pi_atual = pi_atual + 4.*sinal/denominador
		sinal = sinal*-1.
		denominador = denominador + 2.
		contador = contador + 1
		IF(ABS(pi_atual - pi_anterior) < prec) EXIT
		pi_anterior = pi_atual
	END DO
	
	PRINT*, "pi = ", pi_atual
	PRINT*, "Foram somados ", contador, "termos da serie."
	
END PROGRAM exercicio2