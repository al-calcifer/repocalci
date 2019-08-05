PROGRAM media_final
!Programa que le o nome do aluno, as notas dos 
!EPs e das provas, calcula a media final 
!e diz se o aluno foi aprovado ou nao
!Usa a funcao le_valor
IMPLICIT NONE
!Definicao de variaveis

	CHARACTER(LEN=10) :: nome,situacao
	REAL :: EP1, EP2, EP3, EP4
	REAL :: P1, P2
	REAL :: media
	
	REAL, EXTERNAL :: le_valor

!Le nome do aluno
	PRINT*, "Digite o nome do aluno:" 
	READ*, nome
	
!Le as notas dos EPs	
	EP1 = le_valor("EP1")
	EP2 = le_valor("EP2")
	EP3 = le_valor("EP3")
	EP4 = le_valor("EP4")
	
!Le as notas das provas
	P1 = le_valor("P1 ")
	P2 = le_valor("P2 ")
	
!Calcula a media final
	media = 0.4*(EP1+EP2+EP3+EP4)/4. + &
		   0.6*(P1+P2)/2.
		   
!Determina situacao
	IF (media < 3.) THEN
		situacao = "reprovado"
	ELSE IF (media < 5) THEN
		situacao = "recuperacao"
	ELSE
		situacao = "aprovado"
	ENDIF
	
!Escreve resultado
	PRINT*, "O aluno "//TRIM(nome)//" teve media = ", media
	PRINT*, "Sua situacao: "//situacao
	
	
END PROGRAM

REAL FUNCTION le_valor(oque)
IMPLICIT NONE 
	
	CHARACTER(LEN=3), INTENT(in) :: oque

!Le numero
	PRINT*, "Digite a nota do "//oque//":"
	READ*, le_valor

END FUNCTION le_valor

