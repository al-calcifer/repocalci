program harmonicos
implicit none 

integer :: i, j, r, s, n, nm, step1, step2, l, o
parameter(nm=100)
parameter(n=2)
parameter(l=301)
complex :: b=(0,1)
real(kind=8) :: x1(l), x2(-2:2), z(l), w(-2:2), k(-2:2)
real(kind=8) :: f1(x1,z), f2(x2,w)
real(kind=8) :: h, t1, t0
real(kind=8), parameter :: pi = 3.14159265358979323846
real(kind=8) :: theta, phi, lit, lip, lft, lfp, dlt, dlp
real(kind=8) :: y(0:2, -2:2)
real(kind=8) :: fat0, fat1, fat2, fat3
real(kind=8) :: deriv, fracao, raiz, expo, pl, plm
real(kind=8) :: k1, k2, k12, k22

h=(t1-t0)/(n-1)
lit=0.d0; lft=pi
lip=0.d0; lfp=2*pi
dlt=(lft-lit)/dfloat(nm)
dlp=(lfp-lip)/dfloat(nm)

if (i .eq. 0) then
   fat0=1
else
   fat0=1
   do i=1, 2
      fat0=i*fat0
   end do
end if

do j=-2, 2, 1
   fat1=j
   do r=1, 2
      fat1=r*fat1
   end do
end do

do i=0,2
   do j=-2, 2, 1
      fat2=(i+j)
      do r=1, 2
         fat2=r*fat2
      end do
   end do
end do

do i=0,2
   do j=-2, 2, 1
      fat3=(i-j)
      do r=1, 2
         fat3=r*fat3
      end do
   end do
end do

fracao=(1/(dsin(theta))**j)
expo=dcos(j*phi)+b*dsin(j*phi)
raiz=dsqrt(((2*i+1)/4*pi)*(fat3/fat2))
do i=0, 2
   do j=-2, 2, 1
      theta=lit
      do step1=0, nm
         phi=lip
         do step2=0, nm
                






            write(11,*) theta, phi, y(i,j)                                        
!            write(12,*) theta, phi, y(1,-1)                                        
!            write(13,*) theta, phi, y(1,0)                                        
!            write(14,*) theta, phi, y(1,1)                                        
!            write(15,*) theta, phi, y(2,-2)                                        
!            write(16,*) theta, phi, y(2,-1)                                        
!            write(17,*) theta, phi, y(2,0)                                        
!            write(18,*) theta, phi, y(2,1)                                        
!            write(19,*) theta, phi, y(2,2)                                                    
            end do

            end do
phi=phi+dlp
         end do
theta=theta+dlt
      end do
   end do
end do






end program 
