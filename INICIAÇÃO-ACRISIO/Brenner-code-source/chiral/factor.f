      subroutine factor(pridim,prival,nval,fac,primx)

c     factors a number

      implicit none

c input

c     declared number of primes
      integer pridim
c     prime values
      integer prival(pridim)
c     number to be factored
      integer nval

c output

c     the power with which each prime is present, defined up to primx
      integer fac(pridim)
c     last prime present
      integer primx

c local

c     number to factor
      integer n
c     prime index
      integer pri

c executable

c     restrict to absolute value
      n=nval
      if(n.lt.0)n=-n

c     initialize the prime number
      pri=0

c     0 and 1 return no factors
      if(n.le.1)goto 300

c     try all available primes
 100  pri=pri+1

c     out of primes?
      if(pri.gt.pridim)
     & call out('*** PROGRAM ERROR: out of primes!')

c     factor out the prime as many times as possible
      fac(pri)=0
 200  if(n/prival(pri)*prival(pri).ne.n)goto 100
      fac(pri)=fac(pri)+1
      n=n/prival(pri)
      if(n.ne.1)goto 200

c     done
 300  primx=pri
      return
      end
