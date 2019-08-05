program questao_2_newt_raph
implicit none

real(kind=8) :: xt, ft, tol, h, dft, f
integer :: tent

write(*,*) 'Digite o valor tentativa.'
read(*,*) xt
write(*,*) 'Digite a tolerância.'
read(*,*) tol

h=1.0d-9
tent=0
do 
	tent=tent + 1 
	ft=f(xt)
	if (dabs(ft) .le. tol) then
		exit
	else
		dft = (f(xt+h*0.5d0) - f(xt-h*0.5d0))/h
		xt = xt - (ft/dft)
	end if
end do
write(*,*) 'A raíz é:', xt, 'Número de tentativas:', tent



end program

function f(x)
implicit none
real(kind=8) :: f, x

f=dsqrt(dsin(x))

end function
