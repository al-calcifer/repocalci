      subroutine prifnd(pridim,prival)

c     finds the first pridim primes

      implicit none

c input

c     declared number of primes
      integer pridim

c output

c     prime values
      integer prival(pridim)

c local

c     number of primes found so far
      integer primax
c     prime index
      integer pri

c     integer index
      integer i

c executable

c     first prime is 2
      primax=1
      prival(primax)=2
      i=2

c     loop over all integers

 100  i=i+1
c     try to find a factor for i
      do 110 pri=1,primax
         if(prival(pri)**2.gt.i)goto 120
         if(i/prival(pri)*prival(pri).eq.i)goto 100
 110  continue
 120  primax=primax+1
      prival(primax)=i
      if(primax.lt.pridim)goto 100

      return
      end
