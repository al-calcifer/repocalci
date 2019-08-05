PROGRAM FRACAODEVOLUME
IMPLICIT NONE

REAL(KIND=8) :: pc, df, x1, x2, y1, y2
REAL(KIND=8) :: a1, a2, b1, b2, deltaV, V0
INTEGER :: i, j
INTEGER :: f1, f2
REAL(KIND=8), ALLOCATABLE :: A(:,:), B(:,:)


!!!!!! CALCULO DA FRACAO DE VOLUME !!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
f1=371
f2=205
ALLOCATE(A(f1,2))
ALLOCATE(B(f2,2))


OPEN(UNIT=10, FILE='pv-circular19_0', STATUS="OLD")
OPEN(UNIT=20, FILE='pv-colaps19_0', STATUS="OLD")

!!!!! inserir pressao de colapso !!!!
PRINT*, 'pressao de colapso:'
READ*, pc
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!! lendo pontos dos arquivos abertos !!!!
DO i=1, f1
	READ(10,*) A(i,1), A(i,2)
END DO

DO i=1, f2
	READ(20,*) B(i,1), B(i,2)
END DO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!! separando pares de pontos em torno de "pc" em ambos os graficos, e determinando os coeficientes da regressao linear !!!! 
DO i=1, f1
	IF(A(i+1,1) < pc) THEN
	x1=A(i,1)
	y1=A(i,2)	
	x2=A(i+1,1)
	y2=A(i+1,2)			
	END IF
END DO


a1=(y1-y2)/(x1-x2)
a2=y1-a1*x1


DO i=1, f2
	IF(B(i+1,1) < pc) THEN
	x1=B(i,1)
	y1=B(i,2)	
	x2=B(i+1,1)
	y2=B(i+1,2)			
	END IF
END DO


b1=(y1-y2)/(x1-x2)
b2=y1-b1*x1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!! determinando um V0 e a variacao de volume na pressao de colapso !!!!

V0=A(1,2)

deltaV= (a1-b1)*pc+(a2-b2)


WRITE(*,*) deltaV, V0, deltaV/V0

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


END PROGRAM


