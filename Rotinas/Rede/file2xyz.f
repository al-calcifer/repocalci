      integer nat,A,B,C
      Double Precision ax,ay,az,x,y,z
      Double Precision dx,dy,dz,delta,V,t1,t2,P1,P2,P3

      Read(5,*) 
      Read(5,*) nat,r,s,t
      Read(5,*) t1,t2
      Read(5,*) ax,ay,az
      
      write (6,*) nat
      write (6,*) '  '
      
      
      Do i=1,nat
        Read(5,*) iat, zat, x,y,z, g
        if (zat.eq.6) Write(6,114) 'C', x,y,z
        if (zat.eq.18) Write(6,114) 'Ar', x,y,z
        if (zat.eq.1) Write(6,114) 'H', x,y,z
      Enddo
 114  Format(A2,3F20.11)

      End
