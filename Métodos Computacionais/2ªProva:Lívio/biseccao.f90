program questao_2_biseccao
implicit none
real(kind=8):: f,x1,x2,xm,f1,f2,fm,tol
integer :: tent

write(*,*) 'Entre com um intervalo tentativa.'
read(*,*) x1,x2
write(*,*) 'Entre com a tolerância.'
read(*,*) tol

f1=f(x1)
f2=f(x2)
if (f1*f2.gt.0.0D0) then
  stop 'O intervalo não é adequado.'
end if

tent=0.0d0
do
  tent=tent+1
  xm=(x1+x2)/2.0d0
  fm=f(xm)
  if (abs(fm) .le. tol) then
    exit
  end if
  if (f1*fm .gt. 0.0D0) then
    x1=xm
    f1=fm
  else if (f2*fm .gt. 0.0D0) then
    x2=xm
    f2=fm
  end if
end do
write(*,*) 'A raiz é ',xm
write(*,*) 'Número de tentativas ', tent
end program 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

function f(x)
implicit none
real(kind=8):: x,f

f=dsqrt(dsin(x))

end function



