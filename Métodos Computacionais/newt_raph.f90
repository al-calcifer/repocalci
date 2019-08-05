program newton_raphson
implicit none


real(kind=8) :: f,fm, xm, xt1, xt2, tol, m, b
real(kind=8) :: h
integer :: interaction
parameter (h=1.0d-5)


write(*,*) 'entre com um x tentativa.'
read(*,*) xt1
write(*,*) 'entre com a trolerância'
read(*,*) tol



100 if (abs(f(xt1)) .lt. tol) then
		stop
	end if

interaction=0
do
	m=(f(xt1+h)-f(xt1))/h
	b=f(xt1)-m*xt1
!	f(xm)=m*xm+b

	xt2=xt1-(f(xt1)/m)
	if (xm .eq. -(b/m)) then
		xt1=xt2
		f(xt1)=f(xt2)
		go to 100
		
	else
	exit
	end if
end do

write(*,*) 'a raiz é:', f(xm) 

end program

!!!!!!!!!!!!!!função
function f(x)
implicit none
real(kind=8) :: x, f

f=1-(x*x)-x*dtan(x)

end function
