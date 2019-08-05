program ypsilons
implicit none

integer :: l, m, step1, step2, nm, step3
parameter (nm=100)
complex :: i=(0,1)
real(kind=8), parameter :: pi = 3.14159265358979323846
real(kind=8) :: phi, theta, dphi, dtheta
real(kind=8) :: thetai, thetaf, phii, phif
real(kind=8) :: y(0:2,-2:2)
real(kind=8) :: z(0:2,-2:2)
real(kind=8) :: xi, xf, si, sf, wi, wf, dx, ds, dw
real(kind=8) :: x, s, w

open(unit=111, file='y00x.dat')
open(unit=112, file='y00y.dat')
open(unit=113, file='y00z.dat')
open(unit=121, file='y10x.dat')
open(unit=122, file='y10y.dat')
open(unit=123, file='y10z.dat')
open(unit=131, file='y1-1x.dat')
open(unit=132, file='y1-1y.dat')
open(unit=133, file='y1-1z.dat')
open(unit=141, file='y11x.dat')
open(unit=142, file='y11y.dat')
open(unit=143, file='y11z.dat')
open(unit=151, file='y20x.dat')
open(unit=152, file='y20y.dat')
open(unit=153, file='y20z.dat')
open(unit=161, file='y2-2x.dat')
open(unit=162, file='y2-2y.dat')
open(unit=163, file='y2-2z.dat')
open(unit=171, file='y2-1x.dat')
open(unit=172, file='y2-1y.dat')
open(unit=173, file='y2-1z.dat')
open(unit=181, file='y21x.dat')
open(unit=182, file='y21y.dat')
open(unit=183, file='y21z.dat')
open(unit=191, file='y22x.dat')
open(unit=192, file='y22y.dat')
open(unit=193, file='y22z.dat')
open(unit=200, file='y20.dat')
open(unit=210, file='z20.dat')


thetai=0.d0; thetaf=pi
dtheta=(thetaf-thetai)/dfloat(nm)
phii=0.d0; phif=2*pi
dphi=(phif-phii)/dfloat(nm)



theta=thetai
do step1=1, nm
   phi=phii
   do step2=1, nm

!******************************para l=0
y(0,0)=1.d0/(dsqrt(4.d0*pi))
write(111,*) theta
write(112,*) phi
write(113,*) y(0,0)

!**************************************

!******************************para l=1
y(1,0)=dsqrt(3.d0/(4.d0*pi))*dcos(theta)
write(121,*) theta
write(122,*) phi
write(123,*) y(1,0)

y(1,-1)=dsqrt(3.d0/(8.d0*pi))*(dcos(phi)-i*dsin(phi))*dsin(theta)
write(131,*) theta
write(132,*) phi
write(133,*) y(1,-1)

y(1,1)=-dsqrt(3.d0/(8.d0*pi))*(dcos(phi)+i*dsin(phi))*dsin(theta)
write(141,*) theta
write(142,*) phi
write(143,*) y(1,1)

!**************************************

!*****************************para l=2

y(2,0)=dsqrt(5.d0/(16.d0*pi))*(3.d0*dcos(theta) - 1.d0)
write(151,*) theta
write(152,*) phi
write(153,*) y(2,0)
write(200,*) theta, phi, y(2,0)


y(2,-2)=dsqrt(15.d0/(32.d0*pi))*(2.d0*dcos(phi) - 2.d0*i*dsin(phi))*(dsin(theta))**2
write(161,*) theta
write(162,*) phi
write(163,*) y(2,-2)

y(2,-1)=dsqrt(15.d0/(8.d0*pi))*(dcos(phi)-i*dsin(phi))*dsin(theta)*dcos(theta)
write(171,*) theta
write(172,*) phi
write(173,*) y(2,-1)

y(2,1)=-dsqrt(15.d0/(8.d0*pi))*(dcos(phi)+i*dsin(phi))*dsin(theta)*dcos(theta)
write(181,*) theta
write(182,*) phi
write(183,*) y(2,1)

y(2,2)=dsqrt(15.d0/(32.d0*pi))*(2.d0*dcos(phi) + 2.d0*i*dsin(phi))*(dsin(theta))**2
write(191,*) theta
write(192,*) phi
write(193,*) y(2,2)

   phi=phi+dphi
   end do
write(*,*) step1, '%'
theta=theta+dtheta
end do


xi=0.d0; si=0.d0; wi=0.d0
xf=1.d0; sf=1.d0; wf=1.d0
dx=(xf-xi)/dfloat(nm)
ds=(sf-si)/dfloat(nm)
dw=(wf-wi)/dfloat(nm)

x=xi
s=si
w=wi
do step1=1, nm
   do step2=1, nm
      do step3=1, nm


z(2,0)=(dsqrt(5.d0)*3.d0*((w**2)-(x**2+s**2+w**2))) / (dsqrt(16.d0*pi)*(x**2+s**2+w**2))
write(210,*) x, s, w, z(2,0)

w=w+dw
      end do
s=s+ds
   end do
write(*,*) step1, '%'
x=x+dx
end do








end program
