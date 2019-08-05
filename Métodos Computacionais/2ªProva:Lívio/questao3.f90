program questao_3_rk
implicit none

real(kind=8) :: f, h, k1, k2, k3, k4
real(kind=8), allocatable :: x(:), y(:)
integer :: i, n
parameter(h=0.01d0)
parameter(n=201)
open(unit=1, file='rk4.dat')

allocate(x(n), y(n))

x(1)=0.0d0
y(1)=0.0d0
write(1,*) x(1),y(1)
do i=1, n-1
  x(i+1)=x(i)+h
  k1=h*f(x(i),y(i))
  k2=h*f(x(i)+h*0.5D0,y(i)+k1*0.5D0)
  k3=h*f(x(i)+h*0.5D0,y(i)+k2*0.5D0)
  k4=h*f(x(i)+h,y(i)+k3)
  y(i+1)=y(i)+(k1+2.0D0*k2+2.0D0*k3+k4)/6.0D0
  WRITE(1,*) x(i+1),y(i+1)
end do


end program

function f(x, y)
implicit none
real(kind=8) :: x, y, f

f=3.0d0 + x - y

end function
