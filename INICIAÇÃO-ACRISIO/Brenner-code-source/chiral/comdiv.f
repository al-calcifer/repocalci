      integer function comdiv(
     & pridim,prival,
     & n1,n2,
     & fac1,fac2)

c     Return the largest common divisor of two natural numbers.

      implicit none

c input

c     declared number of primes
      integer pridim
c     prime values
      integer prival(pridim)

c     the numbers for which to find the largest common divisor
      integer n1,n2

c work

c     factorizations of the numbers
      integer fac1(pridim),fac2(pridim)

c local

c     last prime present in the factorizations
      integer primx1,primx2

c     prime index
      integer pri

c executable

c     factor the numbers
      call factor(pridim,prival,n1,fac1,primx1)
      call factor(pridim,prival,n2,fac2,primx2)

c     collect the common factors
      comdiv=1
      do 100 pri=1,min(primx1,primx2)
         comdiv=comdiv*prival(pri)**min(fac1(pri),fac2(pri))
 100  continue

      return
      end
