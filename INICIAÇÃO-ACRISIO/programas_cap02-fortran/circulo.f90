PROGRAM circulo 
	IMPLICIT NONE 
   
!Esse programa calcula a equacao de um circulo passando por 3
!pontos fornecidos !pelo usuario

!Declaracao de variaveis
	REAL :: x1, y1, x2, y2, x3, y3, a, b, r 

!Passo 1: le coordenadas
	PRINT *,"Entre com a coordenada dos tres pontos" 
	PRINT *,"na ordem x1,y1,x2,y2,x3,y3" 

	READ*, x1,y1,x2,y2,x3,y3 

!Passo 2: chama subrotina calcula_circulo
  	CALL calcula_circulo(x1,y1,x2,y2,x3,y3,a,b,r) 

!Passo 3: escreve resultado na tela
	PRINT *,"O Centro do circulo que passa por esses &
		    &pontos eh (",a,",",b,")"
	PRINT *,"Seu raio eh ",r 
	
END PROGRAM circulo

SUBROUTINE calcula_circulo(xx1,yy1,xx2,yy2,xx3,yy3,aa,bb,rr) 
!ESSA SUBROTINA ESTA ERRADA! ESTE PROBLEMA SERA ABORDADO NO EP 2
IMPLICIT NONE

!Declaracao de variaveis
	REAL :: xx1, yy1, xx2, yy2, xx3, yy3, aa, bb, rr 

	aa = (xx1+xx2+xx3)/3.
	bb = (yy1+yy2+yy3)/3.
	rr = sqrt((xx1-aa)**2+(yy1-bb)**2) 
	
RETURN 
END SUBROUTINE calcula_circulo 
