      subroutine sett(kdim,kmax,temp,an,r1n,r3,r4)

c     Set the temperature to a given value.

c     Uses random number generator gennor from netlib.

      implicit none

c input

c     declared number of atoms
      integer kdim
c     actual number of atoms
      integer kmax
c     temperature to set
      real temp
c     atom number
      integer an(kdim)

c input/output

c     second Nordsieck parameter /Delta t (atom velocities)
      real r1n(kdim,3)
c     fourth Nordsieck parameter
      real r3(kdim,3)
c     fifth Nordsieck parameter
      real r4(kdim,3)

c local

c     Boltzman, Avogadro in SI units
      real boltz,avogad
      parameter (boltz=1.380662e-23,avogad=6.022045e23)

c     root mean square velocity
      real vrms

c     atom index
      integer k

c     index
      integer i

c     normal random number with given average and standard deviation
      real gennor
      external gennor

c     include atom masses
      include 'atommass.inc'

c executable

      do 100 k=1,kmax

c        find the root-mean-square velocity in Angstrom/fsec
         if(an(k).lt.1 .or. an(k).gt.109)
     &    call out('*** illegal atom number!')
         vrms=sqrt(temp*boltz/(am(an(k))*1.e-3)*avogad*1.e-10)

c        set the velocities, and zero the now wrong Nordsieck parameters
         do 50 i=1,3
            r1n(k,i)=0.
            if(vrms.ne.0.)r1n(k,i)=vrms*gennor(0.,1.)
            r3(k,i)=0.
            r4(k,i)=0.
 50      continue

 100  continue

      return
      end
