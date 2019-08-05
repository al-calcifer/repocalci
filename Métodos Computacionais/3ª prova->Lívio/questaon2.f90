program questao2
implicit none

real(kind=8):: r1, r2, r3, e1, e2, e3, i1, i2, i3
real(kind=8):: A(3,3), B(3,1)
integer :: IPIV(3),INFO

write(*,*) 'Entre com os valores das resistências'
write(*,*) 'R1='
read(*,*) r1
write(*,*) 'R2='
read(*,*) r2
write(*,*) 'R3='
read(*,*) r3
write(*,*) 'Entre com os valores das f.e.m.s'
write(*,*) 'E1='
read(*,*) e1
write(*,*) 'E2='
read(*,*) e2
write(*,*) 'E3='
read(*,*) e3

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
call  DGESV(3,1,A,3,IPIV,B,3,INFO)


if(INFO.ne.0) stop 'Houve um erro.'

! Informando os resultados
write(*,*) 'Os valores das correntes são'
write(*,*) 'I1=',B(1,1)
write(*,*) 'I2=',B(2,1)
write(*,*) 'I3=',B(3,1)


end program 



