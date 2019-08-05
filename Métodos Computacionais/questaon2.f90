PROGRAM questao2
IMPLICIT NONE

REAL(KIND=8):: r1, r2, r3, e1, e2, e3, i1, i2, i3
REAL(KIND=8):: A(3,3), B(3,1)
INTEGER:: IPIV(3),INFO
!FORMULA A*X=B
WRITE(*,*) 'Entre com os valores das resistências'
WRITE(*,*) 'R1='
READ(*,*) r1
WRITE(*,*) 'R2='
READ(*,*) r2
WRITE(*,*) 'R3='
READ(*,*) r3
WRITE(*,*) 'Entre com os valores das f.e.m.s'
WRITE(*,*) 'E1='
READ(*,*) e1
WRITE(*,*) 'E2='
READ(*,*) e2
WRITE(*,*) 'E3='
READ(*,*) e3

! Montando o sistema
A(1,1)=r1
A(1,2)=0.0D0
A(1,3)=r3
A(2,1)=0.0D0
A(2,2)=r2
A(2,3)=r3
A(3,1)=1.0D0
A(3,2)=1.0D0
A(3,3)=-1.0D0
B(1,1)=e1
B(2,1)=e2
B(3,1)=e3

! Resolver o sistema
CALL  DGESV(3,1,A,3,IPIV,B,3,INFO)


IF(INFO.ne.0) STOP 'Houve um erro.'

! Informando os resultados
WRITE(*,*) 'Os valores das correntes são'
WRITE(*,*) 'I1=',B(1,1)
WRITE(*,*) 'I2=',B(2,1)
WRITE(*,*) 'I3=',B(3,1)




END PROGRAM 



