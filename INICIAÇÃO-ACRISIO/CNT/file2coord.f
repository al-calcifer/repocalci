      Parameter (pi=3.141592654)
      integer nat
      character*2 C
      dimension Q(100000,3)

      Read(5,*) nat
      Read(5,*)
      
      ax=1.0E+01
      ay=1.0E+01
      az=10.0      
      
      write (6,*) 'ptm'
      write (6,111)nat,0,0,0
 111  Format(4I6)
      write (6,112)0.0,1.0E-2
 112  Format (2E20.11)
      write (6,113)ax,ay,az
 113  Format (3E20.11)
      Do iat=1,nat
        Read(5,*) C, Q(iat,1),Q(iat,2),Q(iat,3)
        if (C=='C') then
         write(6,114) iat, 6, Q(iat,1),Q(iat,2),Q(iat,3), 1
        else
         write(6,114) iat, 18, Q(iat,1),Q(iat,2),Q(iat,3), 1 
        endif
      Enddo
 114  Format(2I5,3E20.11,I3)

      do iwrite=1,4
      Do iat=1,nat
         write(6,115) iat, 0.0,0.0,0.0
      Enddo
      Enddo
 115  Format(I5,3E20.11)   

      End


