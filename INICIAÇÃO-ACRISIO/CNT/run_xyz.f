c     Altera os parâmetro de rede de tal forma de que o volume
c     permaneça fixo. A variacao eh dada como 0.001 A  na dire-
c     cao axial (az=az+-0.01)
      Parameter (pi=3.141592654)
      integer nat,r,s,t,i,iwrite
      Double Precision ax,ay,az,Vrate,x,y,z,A,B,C
      Double Precision dx,dy,dz,delta,V,t1,t2,P1,P2,P3

      Open(11,File='coord-1.d')
      Open(12,File='coord.d')
      Open(13,File='aux3')
      Read(13,*) W, A, B, C, E
      Read(11,*) 
      Read(11,*) nat,r,s,t
      Read(11,*) t1,t2
      Read(11,*) ax,ay,az
      

      write (12,*) 'bundle'
      write (12,111) nat,r,s,t
 111  Format(4I6)
      write (12,112) t1,t2
 112  Format (2E20.11)
      write (12,113) A, B, C
 113  Format (3E20.11)
      
      
      Do i=1,nat
        Read(11,*) iat, zat, x,y,z, g
        Write(12,114) iat, 6, x,y,z, 1
      Enddo
 114  Format(2I5,3E20.11,I3)

      Do iwrite=1,4
        Do i=1,nat
           Read(11,*) iat, P1, P2, P3
           Write(12,115) iat, P1, P2, P3
        Enddo
      Enddo
 115  Format(I5,3E20.11)

      End
