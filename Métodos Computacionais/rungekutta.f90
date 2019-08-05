program runge_kutta
implicit none


integer :: n, m
real(kind=8), allocatable :: y(:), k(:), t(:), f(:,:)
real(kind=8) :: dy, h
parameter(h=0.1d0)

allocate(y(n))
allocate(k(n))
allocate(t(n))
allocate(f(t(n),y(n)))

write(*,*) 'digite a ordem'
read(*,*) m


do n=1, m
	f(t(n),y(n))=1-t(n)+4*y(n)	
	
	k(1)=h*f(t(n),y(n))
	k(n+1)=h*f(t(n)+(h/2), y(n)+(k(n)/2))

end do


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! correção







end program
