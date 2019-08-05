      Parameter (nat=40, pi=3.141592654, ac=2.456, cgraf=3.35)
      Dimension r(16*nat,3)
      Dimension s(16*nat,3)
      Dimension t(16*nat,3)
      Dimension u(16*nat,3)
      Open(10,File='cnt_xyz.dat')
      Open(11,File='coord-0.d')
      Print*,'(n,m)?'
      read*,n,m
      Print*,'clat?'
      read*,clat


      d=ac*Sqrt(Float(n*n+m*m+n*m))/pi

      ax=2.*(d+cgraf)
      ay=Sqrt(3.)*ax/2.

      Read(10,*)
      Read(10,*)
      Read(10,*)
      write (11,*)'(',n,',',m,')'
      write (11,111)16*Nat,0,0,0
 111  Format(4I6)
      write (11,112)0.0,0.0
 112  Format (2E20.11)
      write (11,113)ax,ay,4*clat
 113  Format (3E20.11)
      
      Do iat=1,nat
         read(10,*) x,y,z
	 z=z*clat/(Sqrt(3.)*ac)
         r(iat,1)=x
         r(iat,2)=y
         r(iat,3)=z
         r(nat+iat,1)=x
         r(nat+iat,2)=y
         r(nat+iat,3)=z+clat
         r(2*nat+iat,1)=x
         r(2*nat+iat,2)=y
         r(2*nat+iat,3)=z+2*clat
         r(3*nat+iat,1)=x
         r(3*nat+iat,2)=y
         r(3*nat+iat,3)=z+3*clat

         r(4*nat+iat,1)=x+ax/2.
         r(4*nat+iat,2)=y
         r(4*nat+iat,3)=z
         r(5*nat+iat,1)=x+ax/2.
         r(5*nat+iat,2)=y
         r(5*nat+iat,3)=z+clat
         r(6*nat+iat,1)=x+ax/2.
         r(6*nat+iat,2)=y
         r(6*nat+iat,3)=z+2*clat
         r(7*nat+iat,1)=x+ax/2.
         r(7*nat+iat,2)=y
         r(7*nat+iat,3)=z+3*clat

         r(8*nat+iat,1)=x+ax/4.
         r(8*nat+iat,2)=y+ay/2.
         r(8*nat+iat,3)=z
         r(9*nat+iat,1)=x+ax/4.
         r(9*nat+iat,2)=y+ay/2.
         r(9*nat+iat,3)=z+clat
         r(10*nat+iat,1)=x+ax/4.
         r(10*nat+iat,2)=y+ay/2.
         r(10*nat+iat,3)=z+2*clat
         r(11*nat+iat,1)=x+ax/4.
         r(11*nat+iat,2)=y+ay/2.
         r(11*nat+iat,3)=z+3*clat

         r(12*nat+iat,1)=x+3.*ax/4.
         r(12*nat+iat,2)=y+ay/2.
         r(12*nat+iat,3)=z
         r(13*nat+iat,1)=x+3.*ax/4.
         r(13*nat+iat,2)=y+ay/2.
         r(13*nat+iat,3)=z+clat
         r(14*nat+iat,1)=x+3.*ax/4.
         r(14*nat+iat,2)=y+ay/2.
         r(14*nat+iat,3)=z+2*clat
         r(15*nat+iat,1)=x+3.*ax/4.
         r(15*nat+iat,2)=y+ay/2.
         r(15*nat+iat,3)=z+3*clat


      Enddo

      
      Do iat=1,16*nat
        if( r(iat,1) .lt. 0.0 ) then
          r(iat,1)=r(iat,1)+ax
        endif
        if( r(iat,2) .lt. 0.0 ) then
          r(iat,2)=r(iat,2)+ay
        endif
      Enddo

      Do iat=1,16*nat
         write(11,114)iat, 6, r(iat,1),r(iat,2),r(iat,3), 1
      Enddo
 114  Format(2I5,3E20.11,I3)   

      do iwrite=1,4
      Do iat=1,16*nat
         write(11,115) iat, 0.0,0.0,0.0
      Enddo
      Enddo
 115  Format(I5,3E20.11)   

      End

   
         

