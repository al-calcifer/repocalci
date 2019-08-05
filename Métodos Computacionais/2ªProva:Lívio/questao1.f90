program questao_1_regra_trapezio
implicit none


real(kind=8), allocatable :: f(:), w(:)
real(kind=8) :: x, a, b, h
real(kind=8) :: integral, val_analit
integer :: n, i
parameter(a=0, b=3.1415, n=10000)

allocate(f(n), w(n))

h=(b-a)/dfloat(n-1)

do i=1, n
  x=a+DFLOAT(i-1)*h  
  f(i)=dsin(x)            
end do

do i=1,n
  w(i)=h
end do
w(1)=h*0.5D0
w(n)=h*0.5D0

integral=0.0d0
do i=1, n
	integral=integral+f(i)*w(i)
end do
write(*,*)'integral:', integral

!!!!!!!!!!!!!!! valor analítico !!!!!!!!!!!!!!!!!!

val_analit=-dcos(b)-(-dcos(a))
write(*,*) 'valor analítico:', val_analit




end program
