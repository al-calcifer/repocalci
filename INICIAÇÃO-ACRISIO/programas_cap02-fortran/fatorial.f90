PROGRAM calcula_fatorial
!Esse programa usa a funcao fatorial para calcular o fatorial de um inteiro 
!dado pelo usuario
IMPLICIT NONE

	INTEGER :: arg
	
	INTEGER, EXTERNAL :: fatorial
	
	PRINT*, "Entre com um numero inteiro: "
	READ*, arg
	
!Verifica se o numero fornecido eh positivo
	IF (arg >= 0) THEN
		PRINT*, "O fatorial de ", arg, "eh ", fatorial(arg)
	ELSE
		PRINT*, "ERRO: o numero tem que ser positivo!"
	ENDIF

END PROGRAM

INTEGER FUNCTION fatorial(arg)
IMPLICIT NONE

	INTEGER, INTENT(in) :: arg
	INTEGER :: i !Contador do loop
	
!Caso em que arg eh 0
	IF (arg == 0) THEN
		fatorial = 1
	ELSE
		fatorial = 1
		DO i=1,arg
			fatorial = fatorial*i
		ENDDO
	ENDIF
END FUNCTION fatorial