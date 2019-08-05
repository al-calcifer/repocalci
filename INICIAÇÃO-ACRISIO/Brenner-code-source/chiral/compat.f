      logical function compat(
     & pridim,prival,
     & n1,m1,n2,m2,
     & fac1,fac2,
     & mult1,mult2)

c     See whether two nanotubes are length compatible.
c     Returns true when a common period can be formed.

c     If they are length compatible, then the length of the common period is:
c     mult1 * [length of tube 1's period] = mult2 * [length of tube 2's period]

      implicit none

c input

c     declared number of primes
      integer pridim
c     prime values
      integer prival(pridim)

c     the chiral vector of tube 1
      integer n1,m1
c     the chiral vector of tube 2
      integer n2,m2

c work

c     factorizations of numbers
      integer fac1(pridim),fac2(pridim)

c output

c     the number of periods of tube 1 needed to make the common period
      integer mult1
c     the number of periods of tube 2 needed to make the common period
      integer mult2

c local

c     translation vectors
      integer tn1,tm1,tn2,tm2

c     last prime present in the factorizations
      integer primx1,primx2

c     the square lengths of the translation vector in internal units
      integer t2

c     prime index
      integer pri

c statement functions

c     coordinates n,m scaled up by a factor 3 to eliminate fractions for atoms
      integer u1,v1,u2,v2

c     dot product in internal units in terms of these scaled-up coordinates
      integer dot
      dot(u1,v1,u2,v2)=2*u1*u2+2*v1*v2+u1*v2+v1*u2

c executable

c     find the translation vectors
      call trans(pridim,prival,n1,m1,fac1,fac2,tn1,tm1)
      call trans(pridim,prival,n2,m2,fac1,fac2,tn2,tm2)

c     square length of first translational vector in internal units
      t2=dot(tn1,tm1,tn1,tm1)
c     factor it
      call factor(pridim,prival,t2,fac1,primx1)

c     square length of second translational vector in internal units
      t2=dot(tn2,tm2,tn2,tm2)
c     factor it
      call factor(pridim,prival,t2,fac2,primx2)

c     initialize multipliers
      mult1=1
      mult2=1

c     see whether the ratio is one of squares and find multipliers
      do 100 pri=1,max(primx1,primx2)
         if(pri.gt.primx1)fac1(pri)=0
         if(pri.gt.primx2)fac2(pri)=0
         if(mod(fac1(pri)-fac2(pri),2).ne.0)goto 800
         if(fac1(pri).gt.fac2(pri))
     &    mult2=mult2*prival(pri)**((fac1(pri)-fac2(pri))/2)
         if(fac2(pri).gt.fac1(pri))
     &    mult1=mult1*prival(pri)**((fac2(pri)-fac1(pri))/2)
 100  continue

c     done
      compat=.true.
      return

c     not length compatible
 800  compat=.false.
      mult1=0
      mult2=0
      return
      end
