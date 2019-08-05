c     Altera os parâmetro de rede ax e ay. 
c     A variacao eh dada como 'delta' (Variação Igual)
c     Nao ha rescala das coordenadas.
      Integer nat,A,B,C
      Integer r,s,t,zat,g
      Double Precision ax,ay,az,Vrate
      Double Precision dx,dy,dz,delta,V,t1,t2,P1,P2,P3
c      Open(10,File='input-0.dat') 
      Open(11,File='coord.d')
      Open(12,File='coord2.d')
c      Read(10,*)
c      Read(10,*) n,m
c      Read(10,*) A,B,C
      Read(11,*) 
      Read(11,*) nat,r,s,t
      Read(11,*) t1,t2
      Read(11,*) ax,ay,az
ccc Defina aqui as modificações nos parâmetros de rede cccc      
      V=ax*ay*az
      dz=0.0d0
      Include 'D.dat'
      dx=delta
      dy=delta
ccc Elas serão modificadas abaixo ccccc           
      ax=ax+dx
      ay=ay+dy
      az=az
c      Write(6,*) ax, dx
       

      write (12,*) 'bundle'
      write (12,111) nat,r,s,t
 111  Format(4I6)
      write (12,112) t1,t2
 112  Format (2E20.11)
      write (12,113) ax,ay,az
 113  Format (3E20.11)
      
      
      Do i=1,nat
        Read(11,*) iat, zat, x,y,z, g
        Write(12,114) iat, zat, x,y,z, g
      Enddo
 114  Format(2I5,3E20.11,I3)

      Do iwrite=1,4
        Do i=1,nat
           Read(11,*) iat, P1, P2, P3
           P1=0.
           P2=0.
           P3=0.
           Write(12,115) iat, P1, P2, P3
        Enddo
      Enddo
 115  Format(I5,3E20.11)

      End
