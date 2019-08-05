
      real*8      xdos
      integer     ifail,ip,iprint,neigen,iprintstep
      integer     iik,Nntb,Nident,idos
      integer     NE_yli,Nat,ierr
       common /integ/xdos(1024),ifail,ip,iprint,neigen,iprintstep,
     .            iik,Nntb(natx),Nident(natx,nnx),idos(1024),
     .            NE_yli,Nat,ierr 

      real*8      tbpar,p,X02AAF,summa,xkt,ecl
      real*8      HR,E,VR
      real*8      xnaapurit,scale,rcut,basis
      real*8      E_pot,ed_F,ann
      real*8      xnopeudet,E_kin,dist,vec1,vec2
      real*8      temp,seed,xscale,Emintb,Emax,E_scale
      real*8      fv1,fv2
      real*8      V_mkp,apu
      real*8      DH,FHF,capu2,capu3,capu4
      real*8      wsum,dwsum
      real*8      vatb
       common / reall/tbpar(6),p,X02AAF,summa,xkt(10,3),ecl,
     .            HR(nmax,nmax),E(nmax),VR(nmax,nmax),
     .            xnaapurit(natx,nnx,3),scale,rcut,basis(3,3),
     .            E_pot,ed_F(natx,3),ann,
     .            xnopeudet(natx,3),E_kin,dist,vec1(3),vec2(3),
     .            temp,seed,xscale,Emintb,Emax,E_scale, 
     .            fv1(nmax),fv2(nmax), 
     .            V_mkp(3),apu, 
     .            DH(4,4,3),FHF(natx,3),capu2,capu3,capu4, 
     .            wsum(natx),dwsum(natx) 
     .       ,vatb(nmax,nmax) 

c       real*8 Am,rc,ro,xnc,xm,xmc,do,dc,aa1,aa2 
c     .,f0,f1,f2,f3,f4,s0,s1,s2,s3,t0,t1,t2,t3  
c       common / par/ Am,rc,ro,xnc,xm,xmc,do,dc,aa1,aa2 
c     .,f0,f1,f2,f3,f4,s0,s1,s2,s3,t0,t1,t2,t3  
cleon: the next two lines moved into tight_bind.f
c carbon
c       data tbpar/-2.99,3.71,-5.0,4.7,5.5,-1.55/ 
cleon
