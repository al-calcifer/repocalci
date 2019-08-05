PROGRAM int_left
IMPLICIT NONE
REAL(KIND=8),ALLOCATABLE:: f(:),w(:),x(:) 
REAL(KIND=8):: h,a,b,integral
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
! Allocando vetores 
ALLOCATE(f(n),w(n),x(n))
! Determinando o valor do passo
h=(b-a)/DFLOAT(n-1)
! Determinando os valores de xi e de f(xi)
DO i=1,n
  x(i)=a+DFLOAT(i-1)*h  !-> Este é xi
END DO
f=x ! Fazendo cada fi igual a f(xi)
! Determinando os valores dos pesos
w=h
w(n)=0.0D0
! Calculando a integral
integral=SUM(f*w)
! Imprimindo o resultado
WRITE(*,*) 'Integral=',integral

END PROGRAM int_left



