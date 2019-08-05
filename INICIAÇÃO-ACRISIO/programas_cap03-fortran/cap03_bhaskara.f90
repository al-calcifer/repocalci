PROGRAM bask

IMPLICIT NONE

	REAL :: a, b, c
	REAL :: raizes_boa(2),raizes_ruim(2)
	INTEGER :: n
	REAL, EXTERNAL :: le_valor
		
!Le a,b,c
	a = le_valor("a")	
	b = le_valor("b")	
	c = le_valor("c")	
		
	CALL bhaskhara_ruim(a,b,c,raizes_ruim,n)
	CALL bhaskhara_boa(a,b,c,raizes_boa,n)
	
	IF (n == 0) THEN
		PRINT*, "Nao ha raizes"
	ELSE
		PRINT*, "Raizes (ruim) = ", raizes_ruim
		PRINT*, "Raizes (boa) = ", raizes_boa
	ENDIF

END PROGRAM bask

!Ma interpretacao da formula de Bhaskhara
SUBROUTINE bhaskhara_ruim(a, b, c, r,n)
IMPLICIT NONE

	REAL, INTENT(in) :: a, b, c
	REAL, INTENT(out) :: r(2)
	INTEGER, INTENT(out) :: n
	
	REAL :: delta,m1

    delta = b**2 - 4*a*c
    
    IF (delta < 0) THEN
    	n = 0
	ELSE
        m1 = SQRT(delta)
        r(1) = (-b + m1) / (2.*a)
        r(2) = (-b - m1) / (2.*a)
        n = 2
    ENDIF
END SUBROUTINE bhaskhara_ruim

!Boa interpretacao da formula de Bhaskhara
SUBROUTINE bhaskhara_boa(a, b, c, r,n)
IMPLICIT NONE

	REAL, INTENT(in) :: a, b, c
	REAL, INTENT(out) :: r(2)
	INTEGER, INTENT(out) :: n
	
	REAL :: delta,m1

    delta = b**2 - 4*a*c
    
    IF (delta < 0) THEN
    	n = 0
	ELSE
        n = 2
        m1 = SQRT(delta)
!Evito a possibilidade de subtracao de números próximos
        IF (b < 0) THEN
        	r(1) = (-b + m1) / (2.*a)
        ELSE
        	r(1) = (-b - m1) / (2.*a)
        ENDIF
!Uso formula de Viete
			r(2) = c / a / r(1)
    ENDIF
END SUBROUTINE bhaskhara_boa


REAL FUNCTION le_valor(oque)
IMPLICIT NONE 
	
	CHARACTER(LEN=1), INTENT(in) :: oque

!Le numero
	PRINT*, "Digite valor de "//oque//":"
	READ*, le_valor

END FUNCTION le_valor
