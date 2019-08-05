!************************************************************************************************************************************!
program ising_rede_quadrada
implicit none

integer :: i, j, n, nt, nn, npt, nbin
integer :: nb, term, pmed, k, r, nmed
integer :: step, tstep
parameter (npt= 100) !número de passos no eixo da temperatura
parameter (term= 100000) ! passos para termalização
parameter (nmed=10000) !Numero de passos para cálculo da média
parameter (n=10, nb=10)
parameter (nbin=1000)
parameter (pmed=1000) ! passos para o cálculo da média
parameter (nn=n*n)
integer(kind=2) :: iseed(3) 
integer :: spin(n,n)
integer :: up(n)
integer :: dw(n)
real(kind=8), parameter :: KbeVK=0.0001*0.8629375
real(kind=8) :: energy(nb)
real(kind=8) :: mag1(nb), medmag(10)
real(kind=8) :: e, w, H, stepbin, mag2, e2, e4, mag, med, magnew
real(kind=8) :: ei, ei2, tmag, binder, sus, cesp
real(kind=8) :: ti, tf, dt, T    !temperaturas
real(kind=8) :: x  !números aleatórios
real(kind=8) :: rand48

open(unit=11,file='01-mag.dat')
open(unit=12,file='02-energ_in.dat')
open(unit=13,file='03-sus.dat')
open(unit=14,file='04-cesp.dat')


iseed(1)=1145; iseed(2)=2345; iseed(3)=9205
ti=5.d0; tf=0.1d0; dt=(tf-ti)/dfloat(npt)
nt=100

!!!!!!!!!!!!!!!!!!!!!!!!!!!!condições de contorno
do i=1, n-1
  up(i)=i+1
  dw(i+1)=i
end do
up(n)=1
dw(1)=n

!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! estado inicial da rede quadrada com ordenação pseudo-aleatória

do i=1, n
  do j=1,n
    x = rand48(iseed)
      if(x .lt. 0.5d0) then
        spin(i,j) = 1
      else
        spin(i,j) = -1
      end if
  end do
end do

write(*,*) spin

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop da termalização
T=ti
do tstep=1, npt
!  if(mod(npt,(tstep/100)).eq.0) write(*,*) 100-tstep/(1/100),'%'
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!método de Monte Carlo
	do step=1, term
! passos de monte carlo
!	stepbin=(pmed/10)
	  do i=1, n
	    do j=1, n
	      e = -spin(i,j)*(spin(dw(i),j)+spin(up(i),j)+spin(i,dw(j))+spin(i,up(j)))
		    if(e .gt. 0.0d0) then
		      spin(i,j) = -spin(i,j)
		    else
		      x = rand48(iseed)
		      w = dexp(2.d0*e/T)
		      if(x .lt. w) then
		        spin(i,j) = -spin(i,j)
			  end if
	    	end if
	    end do
	  end do
	end do

!!!!!!!!!!!!!! obtendo as medidas

!		H = -spin(i,j)*(spin(up(i),j)+spin(dw(i),j)+spin(i,up(i))+spin(i,dw(i)))
!		e = e + 0.5d0*(H/dfloat(nn))


!cálculo da energia

!magnetização

!mag1=mag1+dabs(H/dfloat(pmed))

!cálculo da magnetização

!***************************************************************************************

   mag=0.d0
   ei=0.d0
   mag2=0.d0
   ei2=0.d0
!   if (mod(step,1000) .eq. 0) then
   
do step=1,nmed
tmag=0.d0  
H=0.d0
!mag(1)=0
med=0
!************************************************************************************
      do i=1,n
         do j=1,n
            tmag=tmag+spin(i,j)
            H=H-spin(i,j)*(spin(dw(i),j)+spin(up(i),j)+spin(i,dw(j))+spin(i,up(j)))
         end do
!mag(i)=mag(i)+dabs(tmag)/dfloat(nbin)
      end do
!************************************************************************************
!*************MAGNETIZAÇÃO E ENERGIA INTERNA*****************************************

      mag=mag+dabs(tmag)/dfloat(nn)
      mag2=mag2+tmag*(tmag/dfloat(nn))/dfloat(nn)
      ei=ei+H/dfloat(nn)
      ei2=ei2+H*(H/dfloat(nn))/dfloat(nn)
!mag1=0
!do i=1, nb
!  mag1=mag1+mag(i)/dfloat(nb)
!end do

!************************************************************************************
!Fazendo dois passos extras de monte-carlo para aumentar o espaço amostral
      do i=1,n
         do j=1,n
            e=-spin(i,j)*(spin(dw(i),j)+spin(up(i),j)+spin(i,dw(j))+spin(i,up(j)))
            if (e .gt. 0.d0) then
               spin(i,j)=-spin(i,j)
            else
               x=rand48(iseed)
               w=dexp(2.d0*e/t)
               if(x .lt. w) spin(i,j)=-spin(i,j)
            end if
         end do
      end do

!______________________________________________________________________________________
      do i=1,n
         do j=1,n
            e=-spin(i,j)*(spin(dw(i),j)+spin(up(i),j)+spin(i,dw(j))+spin(i,up(j)))
            if (e .gt. 0.d0) then
               spin(i,j)=-spin(i,j)
            else
               x=rand48(iseed)
               w=dexp(2.d0*e/t)
               if(x .lt. w) spin(i,j)=-spin(i,j)
            end if
         end do
      end do
   end do
!**************************************************************************************
!   end if

!		do k=1, pmed

!	write(1,*) mag2, T
!		if (mod(k, 10) .eq. 0) then

!			mag(nb) = mag(nb) + sum(spin)/stepbin
		
!		end if
!		end do

!*********Calculando Magnetização e Energia Interna
   mag=mag/dfloat(nmed)
   ei=ei/dfloat(nmed)
   e2=e**2 
   e4=e**4
   binder=1-(e2/e4)
   sus=((mag2)-(mag**2))/T*KbeVK
   cesp=((ei2)-(ei**2))/(T**2)*KbeVK
!****************************************************************************************
   write(11,*) t,mag
   write(12,*) t,ei
   write(13,*) t,sus
   write(14,*) t,cesp

   T=T+dt

   write(*,*) tstep

!cálculo das médias


!do i=1, pmed
!	if (mod(i, 10) .eq. 0) then
!		mag(i) = mag(i) +
!	end if
!end do

end do

end program
!*********************************************************************************************************************************************!


!!!!!!!!!!!!!!!!!!! função geradora de numeros pseudo-aleatórios
FUNCTION rand48(seed)

INTEGER*2 seed(3)
INTEGER*4 i1,i2,i3,i11,i21,i31,i12,i22,i13
INTEGER*4 E66D,DEEC,FFFF
PARAMETER(E66D=58989, DEEC=57068, FFFF=65535)
REAL*8 rand48

i1=seed(1)
IF(i1.LT.0) i1=i1+65536
i2=seed(2)
IF(i2.LT.0) i2=i2+65536
i3=seed(3)
IF(i3.LT.0) i3=i3+65536

i11=i1*E66D
i21=i2*E66D
i31=i3*E66D
i12=i1*DEEC
i22=i2*DEEC
i13=i1*5
i1=IAND(i11,FFFF)+11
i11=ISHFT(i11,-16)+ISHFT(i1,-16)
i1=IAND(i1,FFFF)
i11=i11+IAND(i21,FFFF)+IAND(i12,FFFF)
i2=IAND(i11,FFFF)
i3=ISHFT(i11,-16)+ISHFT(i21,-16)+ISHFT(i12,-16)+IAND(i31,FFFF)+IAND(i22,FFFF)+IAND(i13,FFFF)
i3=IAND(i3,FFFF)

rand48=i3*1.52587890625D-05+i2*2.328306D-10

IF(i1.GE.32768) i1=i1-65536
seed(1)=i1
IF(i2.GE.32768) i2=i2-65536
seed(2)=i2
IF(i3.GE.32768) i3=i3-65536
seed(3)=i3

RETURN
END
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! fim da função

