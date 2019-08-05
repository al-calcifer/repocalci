      subroutine setr2

c     Sets Nordsieck parameter r2 from the forces if r2 is all zero.
c     The intention is to reduce initial transients.

c     Requires the forces to be set before the call.

      implicit none
      include 'common_files.inc'

c local

c     atom index
      integer i,ii

c     index
      integer i1

c executable

c     see whether all moving atoms have zero r2
      if(nma.le.0)return
      do 100 ii=1,nma
         i=mlist(ii)
         if(r2(i,1).ne.0. .or. r2(i,2).ne.0. .or. r2(i,3).ne.0.)return
 100  continue

c     set Nordsieck parameter r2
      do 300 ii=1,nma
         i=mlist(ii)
         do 200 i1=1,3
            r2(i,i1)=deltsq/(econv*xmass(ktype(i)))*rnp(i,i1)
 200     continue
 300  continue

      return
      end
