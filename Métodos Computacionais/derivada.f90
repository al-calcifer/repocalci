program derivada
implicit none

real(kind=8), allocatable :: x(:), f(:), df1(:), df2(:)
real(kind=8) :: A, B, h
integer :: N, i
parameter (h=1.0d-5)
parameter (A=0, B=5, N=51)

open(unit=10, file='funcao.dat')
open(unit=11, file='primeira_derivada_adiantada.dat')
open(unit=12, file='primeira_derivada_central.dat')


allocate(x(n))
allocate(f(n))
allocate(df1(n))
allocate(df2(n))



do i=1, 51
	x(i)=A+((B-A)*(i-1))/N-1
	f(i)=sin(x(i))*x(i)**2
	df1(i)=((sin(x(i)+h)*(x(i)+h)**2)-(sin(x(i))*x(i)**2)/h)
	df2(i)=((sin(x(i)+0.5d0*h)*(x(i)+0.5d0*h)**2)-(sin(x(i))*x(i)**2)/h)
	write(10,*) x(i), f(i)
	write(11,*) x(i), df1(i)
	write(12,*) x(i), df2(i)
end do




end program 
