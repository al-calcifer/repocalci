


PROGRAM newton_raphson
IMPLICIT NONE
REAL(KIND=8):: f,xtrial,xm,ftrial,dftrial,h,tol
INTEGER:: iteration

WRITE(*,*) 'Entre com um valor tentativa.'
READ(*,*) xtrial
WRITE(*,*) 'Entre com a tolerância.'
READ(*,*) tol

h=1.0D-8
iteration=0
DO
  iteration=iteration+1
  ftrial=f(xtrial)
  IF(ABS(ftrial).le.tol)THEN
    EXIT
  END IF
  dftrial=(f(xtrial+h*0.5D0)-f(xtrial-h*0.5D0))/h
  xtrial=xtrial-ftrial/dftrial
END DO
WRITE(*,*) 'A raiz é ',xtrial
WRITE(*,*) 'Número de tentativas ',iteration
END PROGRAM newton_raphson

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION f(x)
IMPLICIT NONE
REAL(KIND=8):: x,f

f=x-DCOS(x)

END FUNCTION f



