program euler
implicit none

real(kind=8) :: dy, h
real(kind=8) :: y(0:1), x(0:1)
real(kind=8), allocatable :: f(:,:)
integer :: i, n, m
parameter(h=0.0001d0)

allocate(f(n,m))

!dy=1-x+4*y

x(0)=0.0d0
y(0)=1.0d0
do i=0, 1, 0.1d0
	x(i+1)=x(i)+h

	f(x(i), y(i))=(y(x(i))+h)-y(x(i)))/h

	y(i+1)=y(i)+h*f(x(i), y(i))
	write(*,*) y(i+1), x(i) 
end do


end program
