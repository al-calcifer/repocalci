PROGRAM hello
IMPLICIT NONE
INTEGER:: a,b,c

! A seguir temos uma mensagem solicitando dados
PRINT*,'Digite os valores de a e b'
READ*,a,b
 c=a+b ! A seguir temos a realizacao da soma dos dois numeros
PRINT*,'O resultado da soma de a com b vale:',c

END PROGRAM hello
