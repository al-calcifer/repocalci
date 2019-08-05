
      INTEGER iv,jv, kliend
      COMMON/eatint/iv(nmabig),jv(nmabig), kliend

      REAL*8 eps,sig
      REAL*8 RMAXLJ, RSLJ
      REAL*8 RSPL,RSPLS
      REAL*8 XMM,XMMS 
      REAL*8 XM,C2
      REAL*8 C3,pvdw,rrs 
      COMMON/eatd /eps(ntypes,ntypes),sig(ntypes,ntypes)
     &,RMAXLJ(ntypes,ntypes), RSLJ(ntypes,ntypes)
     &,RSPL(ntypes,ntypes),RSPLS(ntypes,ntypes)
     &,XMM(ntypes,ntypes),XMMS(ntypes,ntypes) 
     &,XM(ntypes,ntypes),C2(ntypes,ntypes)
     &,C3(ntypes,ntypes),pvdw,rrs(3) 

      REAL*8 epss,sigs
      REAL*8 surf ,tau 
      COMMON/csurf/epss(ntypes,ntypes),sigs(ntypes,ntypes)
     &,surf ,tau(ntypes) 

      INTEGER ndir 
      COMMON/ncsurf/ndir 

      REAL*8 dellj,vlook
      REAL*8 dlook 
      COMMON/xlook/dellj,vlook(10000,ntypes,ntypes)
     &,dlook(10000,ntypes,ntypes) 
