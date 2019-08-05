program teovm
implicit none


real(kind=8) :: x1, x2, x3, x4, x5, f
real(kind=8) :: vmf, It
integer :: i, n
parameter(n=100000)

vmf=0
do i=1, n
call random_number(x1)
call random_number(x2)
call random_number(x3)
call random_number(x4)
call random_number(x5)

f=(x1+x2+x3+x4+x5)**2

vmf = vmf + (x1+x2+x3+x4+x5)**2
end do
vmf = (f/dfloat(n))

It = vmf
write(*,*) It
end program
