program random_walker_1D
implicit none

real(kind=8) :: x, passo, alpha, xmed, x2med
integer :: n, m, i, j

m=1000
xmed=0.0d0
x2med=0.0d0
do n=1, 100
	do j=1, m
	x=0.d0
		do i=1, n
			call random_number(passo)
			if (passo .le. 0.5d0) then
			alpha=-1.d0
			else
			alpha=1.d0
			else if
			x=x+alpha
			end if
		end do
		xmed=xmed+x
		x2med=x2med+x*x
	end do
end do
xmed=xmed/dfloat(m)
x2med=x2med/dfloat(m)

end program
