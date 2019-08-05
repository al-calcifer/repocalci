program ising
implicit none
integer ::i,j,w,q,n,k
integer::l,up1,up2,dw1,dw2,nt
real*8 :: spin(20,20),x,y,z
real*8 :: T,tf,ti,dt,E,de,Pr
real*8 :: magf,mag1
l=20 !l é o tamanho de um lado da rede
n=l**2
nt=100
do i=1,l
do j=1,l
spin(i,j)=1.0
end do
end do

ti=0.5
tf=3.5
dt=(tf-ti)/dfloat(nt)


!do i=1,l
!do j=1,l
do k=0,nt-1
T=Ti+k*dt

!write(*,*) T



do w=1,1000 !loop da termalização
do q=1,n !loop na rede
call random_number(x)
i=nint(x*l)
call random_number(y)
j=nint(y*l)
up1=i+1
up2=j+1
dw1=i-1
dw2=j-1
if (i.eq.l) then
up1=1
end if
if (j.eq.l) then
up2=1
end if
if (i.eq.1) then
dw1=l
end if
if (j.eq.1) then
dw2=l
end if

! algoritmo de metropolis
E=spin(i,j)*(spin(up1,j)+spin(dw1,j)+spin(i,up2)+spin(i,dw2))
de=-2.0*E
if (de.lt.0.d0) then
spin(i,j)=-1.0d0*spin(i,j)
else
call random_number(z)
Pr=exp((-1.0d0*E)/T)

if (Pr.le.z) then
spin(i,j)=-1.0d0*spin(i,j)
end if
end if

end do !fim do loop na rede
end do !fim do loop na termalização






do w=1,1000 !loop das medidas
do q=1,n !loop na rede
call random_number(x)
i=nint(x*l)
call random_number(y)
j=nint(y*l)

up1=i+1
up2=j+1
dw1=i-1
dw2=j-1
if (i.eq.l) then
up1=1
end if
if (j.eq.l) then
up2=1
end if
if (i.eq.1) then
dw1=l
end if
if (j.eq.1) then
dw2=l
end if

! algoritmo de metropolis
E=spin(i,j)*(spin(up1,j)+spin(dw1,j)+spin(i,up2)+spin(i,dw2))
de=-2.0*E
if (de.lt.0.d0) then
spin(i,j)=-1.0d0*spin(i,j)
else
call random_number(z)
Pr=exp((-1.0d0*E)/T)
write(*,*) Pr
if (Pr.le.z) then
spin(i,j)=-1.0d0*spin(i,j)
end if
end if

end do !fim do loop na rede


mag1=0.d0
do i=1,l
do j=1,l
mag1=mag1+spin(i,j)
end do
end do
!write(*,*) mag1
magf=magf+mag1
end do !fim do loop nas medidas
magf=magf/1000
!write(*,*) magf
write(11,*) T,magf
end do !fim do loop na temperatura


!end do
!end do


end program
