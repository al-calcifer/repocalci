program q2
implicit none
real(kind=8):: x1,x2,x3,x4,x5,fmed 
integer:: i,n

n=10000000
fmed=0.0d0

do i=1,n
  call random_number(x1)
  call random_number(x2)
  call random_number(x3)
  call random_number(x4)
  call random_number(x5)
  fmed=fmed+((x1*2)**2+(x2*2)**2+(x3*2)**2+(x4*2)**2+(x5*2)**2)
end do

fmed=(fmed/dfloat(n))*(2**5)

write(*,*) 'o valor aproximado da integral é ',fmed

end program






