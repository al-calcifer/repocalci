      Integer nat,A,B,C
      Integer r,s,t,Nmedia, Ncarbon,g(30000)
      Double Precision ax,ay,az,latt,nc,ptnc,Lc,zc
      Double Precision t1,t2,P1,P2,P3,rho,dcc,dhc
      Double Precision x(30000),y(30000),z(30000)
      Integer iat(30000),zat(30000)
!      parameter(Ncarbon=8)
c      Open(10,File='input-0.dat')
      Open(11,File='coord_iso.d')
      Open(12,File='coord_filled.d')
!	  Open(13,File='coord_filled.xyz')
 
c Defining initial density...
      rho=0.02
c 
	  dhc=1.31
	  dcc=1.42
	  
      
      Read(11,*) 
      Read(11,*) nat,r,s,t
      Read(11,*) t1,t2
      Read(11,*) ax,ay,az


c Reading carbon atoms      
      Do i=1,nat
       Read(11,*) iat(i),zat(i),x(i),y(i),z(i),g(i)
      Enddo
      
      P1=0
      P2=0
      P3=0
      Do i=1,nat
        P1=P1+x(i)/nat
        P2=P2+y(i)/nat
        P3=P3+z(i)/nat
      Enddo
      Do i=1,nat
        x(i)=x(i)-P1
        y(i)=y(i)-P2
        z(i)=z(i)-P3
      Enddo
      d=0.
      Do i=1,nat
       d=d+(2./nat)*sqrt(x(i)**2+y(i)**2)
      Enddo
c Defining box size...
      ax=50+d
      ay=50+d
c

c      Write(6,*) 'Number of medium particles?'
c      Read(5,*) Nmedia
c      Write(6,*) 'Dimension box? (ax,ay)'
c      Read(5,*) ax,ay
      
      Write(6,*) P1,P2,P3,d
      P1=ax*ay*az
      P2=3.1415*((d/2)**2)*az
c Defining Nmedia...
      Nmedia=NINT((P1-P2)*rho)
      
c      Write(6,116) 'Density=', Nmedia/(P1-P2)

  150 write(*,*) 'Número de átomos de carbono na cadeia:'
  	  read(*,*) Ncarbon


      write (12,*) 'ptm'
      write (12,111) nat+Ncarbon+2,r,s,t
 111  Format(4I6)
      write (12,112) t1,t2
 112  Format (2E20.11)
      write (12,113) ax,ay,az
 113  Format (3E20.11)
      
      Do i=1,nat
        Write(12,114) iat(i), zat(i), x(i),y(i),z(i), g(i)
      Enddo
 114  Format(2I5,3E20.11,I3)
      d24=(d/2.)**2
      Do i=1,Nmedia
 120    x(i)=(2*rand(0)-1)*(ax/2.)
        y(i)=(2*rand(0)-1)*(ay/2.)
        dlinha=x(i)**2+y(i)**2
        if(dlinha.lt.(d24*1.9)) go to 120
        z(i)=(2*rand(0)-1)*(az/2.)
      Enddo
      
         
!      Do i=1,Nmedia
!        Write(12,114) nat+i,18, x(i),y(i),z(i), 1
!		Write(13,*) 'Ar', x(i),y(i),z(i)
!      Enddo

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      nc=Ncarbon-1
!      ptnc=1.31+chainz
      Lc=dhc+dhc+((Ncarbon-1)*dcc)
!      zc=dhc+(Ncarbon-1)*dcc
      write(*,*) 'Lc=', Lc

!imprimindo a cadeia 
	!primeiro hidrogênio

!	  if (Lc+8 .lt. az) then	
!     Write(12,114) nat+1,1, 0.00,0.00,0.00-0.5*Lc, 1
!	  Write(13,*) 'H', 0.00,0.00,0.00

	!cadeia de carbono

	 	Do i=1,Ncarbon
            zc=dhc+(i-1)*dcc-0.5*Lc
			Write(12,114) nat+1+i,6, 0.00,0.00,zc, 1
		Enddo

	!último hidrogênio
	  
!	  	Write(12,114) nat+Ncarbon+2,1, 0.00,0.00,Lc-0.5*Lc, 1
!	  else if (Lc+8 .ge. az) then
!		write(*,*) 'insira menos átomos de carbono'
!		go to 150 
		
!	  end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      Do iwrite=1,4
        Do i=1,nat+Ncarbon+2
           Write(12,115) i, 0.000, 0.000, 0.000

        Enddo
      Enddo
 
 115  Format(I5,3E20.11)
 116  Format(1A20,F20.11)

      End
