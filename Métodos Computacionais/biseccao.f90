program biseccao
implicit none


real(kind=8) :: f, f1, f2, fm, x1, x2, xm, tol
integer :: interaction


write(*,*) 'entre com um intervalo tentativa.'
read(*,*) x1, x2
write(*,*) 'entre com a trolerância'
read(*,*) tol


f1=f(x1)
f2=f(x2)
if (f1*f2 .gt. 0.0d0) then
	stop 'o imtervalo não é adequado!'
end if

interaction=0
do
	interaction=interaction+1
	xm=(x1-x2)/2.0d0
	fm=f(xm)
	if (abs(fm) .le. tol) then
		exit
	end if
	if (f1*fm .gt. 0.0d0) then
		x1=xm
		f1=fm
	else if (f2*fm .gt. 0.0d0) then
		x2=xm
		f2=fm
	end if
write(*,*) interaction
end do

write(*,*) 'a raiz é:', xm

end program

!!!!!!!!!!!!!!função
function f(x)
implicit none
real(kind=8) :: x, f

f=x-dcos(x)

end function
