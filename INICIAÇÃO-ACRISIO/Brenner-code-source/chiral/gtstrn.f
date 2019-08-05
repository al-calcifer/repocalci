      subroutine gtstrn(tubdim,tubmax,tubr,tubp,tubpct,
     & tublo,tubhi,totlt,strain)

c     Find the strain in a set of nanotubes.

c     This subroutine assumes that each tube acts as a linear spring and
c     that the tubes' stiffness is proportional to their radius.  In other
c     words, it computes the total length of the tube from the static
c     equilibrium condition
c
c                sum_tube [ r_tube * (totlt - l_tube)/l_tube ] = 0

      implicit none

c input

c     declared number of nanotubes
      integer tubdim
c     actual number of nanotubes
      integer tubmax
c     tube radius and period length
      real tubr(tubdim),tubp(tubdim)
c     number of periods to use
      integer tubpct(tubdim)

c output

c     shortest tube
      integer tublo
c     longest tube
      integer tubhi
c     equilibrium length
      real totlt
c     strain
      real strain

c local

c     nanotube index
      integer tub

c     length of a tube
      real l
c     shortest length
      real llo
c     longest length
      real lhi

c     partial sums of the equilibrium condition
      real*8 sum1,sum2

c     find shortest and longest tube, and find partial sums
      l=tubp(1)*tubpct(1)
      sum1=tubr(1)
      sum2=tubr(1)/l
      tublo=1
      llo=l
      tubhi=1
      lhi=l
      do 100 tub=2,tubmax
         l=tubp(tub)*tubpct(tub)
         sum1=sum1+tubr(tub)
         sum2=sum2+tubr(tub)/l
         if(l.lt.llo)then
            tublo=tub
            llo=l
         endif
         if(l.gt.lhi)then
            tubhi=tub
            lhi=l
         endif
 100  continue

c     compute the net length
      totlt=sum1/sum2

c     find the maximum strain
      strain=max((lhi-totlt)/lhi,(totlt-llo)/llo)

      return
      end
