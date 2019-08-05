PROGRAM lin_sys
IMPLICIT NONE

REAL(KIND=8):: A(6,6),B(6,1)
INTEGER:: IPIV(6),INFO

! Montando o sistema
A(1,1)=1.0D0
A(1,2)=0.0D0
A(1,3)=-1.0D0
A(1,4)=0.0D0
A(1,5)=-1.0D0
A(1,6)=0.0D0

A(2,1)=0.0D0
A(2,2)=0.0D0
A(2,3)=1.0D0
A(2,4)=-1.0D0
A(2,5)=0.0D0
A(2,6)=-1.0D0

A(3,1)=-1.0D0
A(3,2)=1.0D0
A(3,3)=0.0D0
A(3,4)=1.0D0
A(3,5)=0.0D0
A(3,6)=0.0D0

A(4,1)=0.0D0
A(4,2)=0.0D0
A(4,3)=3.0D0
A(4,4)=0.0D0
A(4,5)=-5.0D0
A(4,6)=6.0D0

A(5,1)=0.0D0
A(5,2)=-2.0D0
A(5,3)=0.0D0
A(5,4)=4.0D0
A(5,5)=0.0D0
A(5,6)=-6.0D0

A(6,1)=0.0D0
A(6,2)=2.0D0
A(6,3)=0.0D0
A(6,4)=0.0D0
A(6,5)=5.0D0
A(6,6)=0.0D0

B(1,1)=0.0D0
B(2,1)=0.0D0
B(3,1)=0.0D0
B(4,1)=0.0D0
B(5,1)=0.0D0
B(6,1)=5.0D0

! Resolver o sistema
CALL  DGESV(6,1,A,6,IPIV,B,6,INFO)
IF(INFO.ne.0) STOP 'Houve um erro.'

! Informando os resultados
WRITE(*,*) 'Os valores das correntes são'
WRITE(*,*) 'I1=',B(1,1)
WRITE(*,*) 'I2=',B(2,1)
WRITE(*,*) 'I3=',B(3,1)
WRITE(*,*) 'I4=',B(4,1)
WRITE(*,*) 'I5=',B(5,1)
WRITE(*,*) 'I6=',B(6,1)



END PROGRAM lin_sys



