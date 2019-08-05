      Include 'Param'

      Dimension x(3),f0(3,Nat)

      Character(20) name

      Open(10,File='coord.d')

      Open(12,File='indices')

      Open(13,File='forces0')

      Open(14,File='Kij')



      read(12,*)nmove,icart,ipm



      read(10,*)name

      read(10,100) i1,i2,i3,i4

 100  Format(4I6)

      read(10,101) r1,r2

 101  Format(2E20.11)

      read(10,102) r1,r2,r3

 102  Format(3E20.11)

 103  Format(2I5,3E20.11,I3)

 104  Format(I5,3E20.11)

 105  Format(4I15)



c *** Ler forcas antigas

      Do iat=1,nat

         read(13,*) i1,f0(1,iat),f0(2,iat),f0(3,iat)

      Enddo

c *** posicoes

      Do iat=1,nat

         read(10,103) i1,i2,x(1),x(2),x(3),i3

      Enddo



c *** velocidades

      Do iat=1,nat

         read(10,104) i1,x(1),x(2),x(3)

      Enddo



c *** aceleracoes (forcas)



      Do iat=1,nat

         read(10,104) i1,x(1),x(2),x(3)

         write(14,*) x(1)-f0(1,iat)

         write(14,*) x(2)-f0(2,iat)

         write(14,*) x(3)-f0(3,iat)

      Enddo



      Do iset=1,2

         Do iat=1,nat

            read(10,104) i1,x(1),x(2),x(3)

         Enddo

      enddo



      read(10,105) i1,i2,i3,i4



      end















      

