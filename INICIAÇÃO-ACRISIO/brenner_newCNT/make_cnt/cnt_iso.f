      Parameter (pi=3.141592654)
      
      Integer W,nat,A,B,C,n,m,flag,Ncells 
      dimension P(100000,3),Q(100000,3)
      Double Precision latt,ax,ay,az
      Character nome1,nome2
      Character c1
      Open(10,File='cnt_xyz.dat')
      Open(11,File='coord_iso.d')
c      Open(12,File='input-0.dat')

      Read(10,*)
      Read(10,*) nome1,latt,nome2,Ncells 
      Read(10,*) nat

c      Write(6,*) 'ax,ay?(az=Ncells*latt)'
c      Read(5,*) ax,ay
      ax=1.0E20
      ay=1.0E20
      az=Ncells*latt
c      write(6,*) ax,ay,az 

56    write (11,*) 'isolado'
      write (11,111)nat,0,0,0
 111  Format(4I6)
      write (11,112)0.0,5.0E-1
 112  Format (2E20.11)
      write (11,113)ax,ay,az
 113  Format (3E20.11)
      
      Do iat=1,nat
         read(10,*) c1,x,y,z
	 P(iat,1)=x
         P(iat,2)=y
         P(iat,3)=z
      Enddo
      C=1
      Z=2*nat
      Do l=1,C
         Do k=1,nat 
            Q((l-1)*nat+k,1)=P(k,1)
            Q((l-1)*nat+k,2)=P(k,2)
            Q((l-1)*nat+k,3)=P(k,3)+(l-1)*(az/C)
         Enddo
      Enddo
c  114  Format(2I5,3F20.11,I3)
      Do iat=1,nat
c       write(11,114)iat, 6, Q(iat,1),Q(iat,2),Q(iat,3), 1
        write(11,114)iat, 6, Q(iat,1),Q(iat,2),Q(iat,3), 1
      Enddo
 114  Format(2I5,3E20.11,I3)

      do iwrite=1,4
      Do iat=1,nat
         write(11,115) iat, 0.0,0.0,0.0
      Enddo
      Enddo
 115  Format(I5,3E20.11)   
      
      write(6,*)
      write(6,*) ' Normal end. '
      write(6,*) ' ax= ',ax,' ay= ',ay,' az= ',az
      write(6,*) ' Output file=coord_iso.d '


      End

   
         


