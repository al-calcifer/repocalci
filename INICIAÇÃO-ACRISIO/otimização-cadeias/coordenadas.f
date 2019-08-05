      Include 'Param'

      Dimension x(3)

      Character(20) name

      Open(10,File='coord.input')

      Open(11,File='coord.d')

      Open(12,File='indices')

c      Open(13,File='forces0')



      read(12,*)nmove,icart,ipm



      read(10,*)name

      write(11,*)name

      read(10,*) i1,i2,i3,i4

      write(11,*) i1,i2,i3,i4

      read(10,*) r1,r2

      write(11,*) r1,r2

      read(10,*) r1,r2,r3

      write(11,*) r1,r2,r3



c *** posicoes

      Do iat=1,nat

         read(10,*) i1,i2,x(1),x(2),x(3),i3

         If(iat.eq.nmove) Then

            x(icart)=x(icart)+(-1)**(ipm)*deltar

         EndIf

         write(11,*) i1,i2,x(1),x(2),x(3),i3

      Enddo



c *** velocidades

      Do iat=1,nat

         read(10,*) i1,x(1),x(2),x(3)

         write(11,*) i1,x(1),x(2),x(3)

      Enddo



c *** aceleracoes (forcas)



      Do iat=1,nat

         read(10,*) i1,x(1),x(2),x(3)

c         If(iat.eq.nmove) write(13,*) x(icart)

         write(11,*) i1,0.0,0.0,0.0

      Enddo



      Do iset=1,2

         Do iat=1,nat

            read(10,*) i1,x(1),x(2),x(3)

            write(11,*) i1,x(1),x(2),x(3)

         Enddo

      enddo



      end















      

