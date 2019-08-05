program ising

!Solução numérica via monte-carlo para o modelo Ising 2D puro

implicit none

integer i,j,tstep,step,l,n,nt,nterm,nmed
parameter (n=20) !tamanho da rede = nxn
parameter (l=n*n) !tamanho da rede
parameter (nterm=10000) !Numero de passos para termalizar
parameter (nmed=10000) !Numero de passos para cálculo da média
parameter (nt=100) !Numero de pontos no eixo da temperatura
integer*2 iseed(3)
integer spin(n,n),up(n),dw(n)
real*8 ti,tf,t,dt !temperatura
real*8 mag,sus,ei,cv !magnetização, susceptibilidade, energia interna e calor específico
real*8 mag2,ei2 !media do quadrado de sz e do quadrado de H
real*8 tmag,tmag2,x,p,e,rand48

open(11,file='1-mag-n20.dat')
open(12,file='2-sus-n20.dat')
open(13,file='3-enint-n20.dat')
open(14,file='4-calesp-n20.dat')
open(15,file='5-configuration-n20.dat')

iseed(1)=1145; iseed(2)=2345; iseed(3)=9205
ti=4.d0; tf=0.1d0; dT=(tf-ti)/dfloat(nt)

!Estado inicial da rede antes da termalização
!Todos para cima
!do i=1,n
!   do j=1,n
!      spin(i,j)=1
!   end do
!end do
!Orientação inicial aleatória
do i=1,n
   do j=1,n
      x=rand48(iseed)      
      if (x .lt. 0.5d0) then
         spin(i,j)=1
      else
         spin(i,j)=-1
      end if
   end do
end do

!condições de contorno
do i=1,n-1
  up(i)=i+1
  dw(i+1)=i
end do
up(n)=1
dw(1)=n

!loop na temperatura: parametros intensivos em função da temperatura
t=ti
do tstep=1,nt

!Método de monte-carlo: algoritmo metrópolis
   do step=1,nterm !termalização
      do i=1,n
         do j=1,n
            e=-spin(i,j)*(spin(dw(i),j)+spin(up(i),j)+spin(i,dw(j))+spin(i,up(j)))
!ao virar o spin, a variação de energia é -2*e
!se e é positivo, a variação de e ao virar o spin será negativa, e para menor energia a mudança é aceita
            if (e .gt. 0.d0) then
               spin(i,j)=-spin(i,j)
            else
!caso contrário, a mudança será aceita se, ao sortearmos um numero aleatório, este for menor que dexp(-2.d0*e/T)
               x=rand48(iseed)
               p=dexp(2.d0*e/t)
               if(x .lt. p) spin(i,j)=-spin(i,j)
            end if
         end do
      end do
   end do

!calculo das medias sobre nterm configurações termalizadas
   mag=0.d0
   ei=0.d0
   mag2=0.d0
   ei2=0.d0
   do step=1,nmed
      tmag=0.d0
      e=0.d0
      do i=1,n
         do j=1,n
            tmag=tmag+spin(i,j)
            e=e-spin(i,j)*(spin(dw(i),j)+spin(up(i),j)+spin(i,dw(j))+spin(i,up(j)))
         end do
      end do
      mag=mag+dabs(tmag)/dfloat(l)
      mag2=mag2+tmag*(tmag/dfloat(l))/dfloat(l)
      ei=ei+e/dfloat(l)
      ei2=ei2+e*(e/dfloat(l))/dfloat(l)

!Fazendo um passo extra de monte-carlo para aumentar o espaço amostral
      do i=1,n
         do j=1,n
            e=-spin(i,j)*(spin(dw(i),j)+spin(up(i),j)+spin(i,dw(j))+spin(i,up(j)))
            if (e .gt. 0.d0) then
               spin(i,j)=-spin(i,j)
            else
               x=rand48(iseed)
               p=dexp(2.d0*e/t)
               if(x .lt. p) spin(i,j)=-spin(i,j)
            end if
         end do
      end do
   end do

!Calculando Magnetização e Energia Interna
   mag=mag/dfloat(nmed)
   ei=ei/dfloat(nmed)

!Calculando funções resposta: Susceptibilidade e Calor Específico
   mag2=mag2/dfloat(nmed)
   ei2=ei2/dfloat(nmed)
   sus=dfloat(l)*(mag2-mag**2)/t
   cv=dfloat(l)*(ei2-ei**2)/t/t

   write(11,*) t,mag
   write(12,*) t,sus
   write(13,*) t,ei
   write(14,*) t,cv

   t=t+dt

   write(*,*) tstep

end do

end program

!subrotina geradora de numeros pseudo-aleatórios
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


   
