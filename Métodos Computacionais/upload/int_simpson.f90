PROGRAM int_simpson
IMPLICIT NONE
REAL(KIND=8),ALLOCATABLE:: f(:),w(:) 
REAL(KIND=8):: x,h,a,b,integral
INTEGER:: n,i

! Dados de entrada
WRITE(*,*) 'Entre com o intervalo de integracão.'
WRITE(*,*) 'a='
READ(*,*) a
WRITE(*,*) 'b='
READ(*,*) b
WRITE(*,*) 'Entre com o número de pontos no grid.'
WRITE(*,*) 'n='
READ(*,*) n
IF(MOD(n,2).eq.0)THEN
  STOP 'n deve ser ímpar'
END IF
! Allocando vetores 
ALLOCATE(f(n),w(n))
! Determinando o valor do passo
h=(b-a)/DFLOAT(n-1)
! Determinando os valores de xi e de f(xi)
DO i=1,n
  x=a+DFLOAT(i-1)*h  !-> Este é xi
  f(i)=x             !-> Este é f(xi)
END DO
! Determinando os valores dos pesos
w(1)=h/3.0D0
w(n)=h/3.0D0
DO i=2,n-1,2
  w(i)=4.0D0*h/3.0D0
END DO
DO i=3,n-2,2
  w(i)=2.0D0*h/3.0D0
END DO
! Calculando a integral
integral=0.0D0
DO i=1,n
  integral=integral+f(i)*w(i)
END DO
! Imprimindo o resultado
WRITE(*,*) 'Integral=',integral

END PROGRAM int_simpson



