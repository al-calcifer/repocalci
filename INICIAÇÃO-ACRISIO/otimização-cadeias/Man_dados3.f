c     Transforma o arquivo Emin-Vfix em plot
      Parameter (unit1=160.217)
      Double Precision mp,me,mv,mp2,me2,mv2,mec,mec2
      Integer i,cas     
      Double Precision p(700),e(700),v(700),e_C,e2(700)
      Character nom,nom2
      Open(10,File='pressure') 
      Open(11,File='hat0')     

c      Read(11,*) nom,nom2,e_C
c      Read(11,*) 
      mp=0.
      me=0.
      mv=0.
      mec=0.
      Do i=1,40
        Read(10,*) ind,p(i),e(i),v(i),e2(i)
        mp=mp+p(i)/40
        me=me+e(i)/40
        mv=mv+v(i)/40
        mec=mec+e2(i)/40
      Enddo
      cas=0
      Do i=1,40
        if ((p(i).gt.(mp-1.0)).and.(p(i).lt.(mp+1.0))) then
         cas=cas+1
         mp2=mp2+p(i)
         me2=me2+e(i)
         mv2=mv2+v(i)
         mec2=mec2+e2(i)
        endif
      Enddo
      mp=mp2/cas
      me=me2/cas
      mv=mv2/cas
      mec=mec2/cas

      cas=0
      mp2=0.
      Do i=1,40
        if ((p(i).gt.(mp-1.0)).and.(p(i).lt.(mp+1.0))) then
         cas=cas+1
         mp2=mp2+(p(i)-mp)**2
        endif
      Enddo
      mp2=sqrt(mp2/(cas-1))


      Write(6,*) mp,mv,me,mec,mec+(mp/unit1)*mv,mp2
      
      End
