c     Procura az min
c     do sistema estudado
c     Cria novo input com ax e az min para
      Parameter (unit1=160.217)
      Integer n,m,f,g,h,flag, iat, zat, nat, ind
      Double Precision X(4),Y(4),Z(4),Emin(4)
      Double Precision A(4),B(4),C(4),E(4),deriv1
      Double Precision ver1,ver2(4)
      Character nome, nome2
       
      Open(10,File='teste-Emin')
      Open(16,File='indice')

      Do i=1,2
       Read(10,*) X(i),Y(i),Z(i)
       Read(10,*) nome, nome2, Emin(i)
       E(i)=Emin(i)
       A(i)=(X(i)**2)*Z(i)
       Read(10,*) ver2(i),ver1
      Enddo
      Read(16,*) ind
      Close(16)
      Open(16,File='indice')
      deriv1=-(E(2)-E(1))/(A(2)-A(1))
      ver1=0.5*(ver2(2)+ver2(1))
      Write(6,*) ind, deriv1*unit1,0.5*(E(2)+E(1)),0.5*(A(2)+A(1)),ver1
      
      End

ccccccccccccccccccccccccccccccccccccccccccccc
