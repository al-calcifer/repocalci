      Parameter(PI=3.1415)
      Integer i,nat
      Double Precision dx,dy,dz,theta,phi,deltar
      Double Precision delta
      Integer seed1,seed2
     
      nat=50
      delta=-0.08d0
      seed1=150
      seed2=34
      Do i=1,nat
cccc  Gerar deslocamento aleatorio
      deltar=0.05d0
      theta=rand(0)*PI*rand(i)
c      theta=rand(0)*PI
      phi=rand(INT(delta*1000)+seed1+seed2)*2*PI*rand(i)
c      phi=rand(0)*2*PI
      dx=deltar*SIN(theta)*COS(phi)
      dy=deltar*SIN(theta)*SIN(phi)
      dz=deltar*COS(theta)
      Write(6,*) dx,dy,dz,theta,phi
       
      Enddo
      End
