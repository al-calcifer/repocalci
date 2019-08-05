      subroutine trans(
     & pridim,prival,
     & n,m,
     & fac1,fac2,
     & tn,tm)

c     Finds the translation (axial period) vector given the chiral
c     (circumference) vector.

c     Both vectors are in the usual basis:
c     - the first basis vector extends from the cell center that acts as
c       origin to an adjacent cell center;
c     - the second basis vector equals the first rotated 60 degrees clockwise.

      implicit none

c input

c     declared number of primes
      integer pridim
c     prime values
      integer prival(pridim)

c     the chiral vector
      integer n,m

c work

c     factorizations of numbers
      integer fac1(pridim),fac2(pridim)

c output

c     the translation vector
      integer tn,tm

c local

c     prime index
      integer pri

c     function that returns the largest common divisor
      integer comdiv
      external comdiv

c     temporary
      integer itmp

c executable

c     find a translation vector by rotating the chiral vector 90 degrees
      tn=n+2*m
      tm=-2*n-m

c     eliminate common factors in tn and tm
c     (this could be done quicker by looking for common factors in m and n
c      themselves, plus a factor 3 if n-m is divisible by 3.)

      itmp=comdiv(pridim,prival,tn,tm,fac1,fac2)
      tn=tn/itmp
      tm=tm/itmp

      return
      end
