      program chtemp

c     Change the temperature in a coord.d file.

      implicit none

c coord.d data

c     declared number of atoms
      integer kdim
      parameter (kdim=100000)
c     actual number of atoms
      integer kmax
c     atom index
      integer k

c     file header line and its actual length
      character*(80) head
      integer headlt

c     initially: 0 / if xmol was called before the file was written: 3
      integer idum
c     probably: number of rigid atoms not to be included in adaptive
c     time step changes
      integer nra
c     probably: number of Langevin atoms not to be included in adaptive
c     time step changes
      integer nla

c     total time of evolution
      real ttime
c     time step
      real delta

c     size of the periodic box
      real cube(3)

c     atom number
      integer an(kdim)

c     thermostat response:  0: moving  1: moving and thermostated  2: rigid
      integer itr(kdim)

c     first Nordsieck parameter (atom positions)
      real r0(kdim,3)
c     second Nordsieck parameter /delta (atom velocities)
      real r1n(kdim,3)
c     third Nordsieck parameter (atom accelerations * 0.5 delta^2)
      real r2(kdim,3)
c     fourth Nordsieck parameter
      real r3(kdim,3)
c     fifth Nordsieck parameter
      real r4(kdim,3)

c     nonzero if there are random numbers attached to the file
      integer havran
c     random numbers data
      integer randat(4)
c     nonzero if there is a periodic box strain matrix attached
      integer havmat
c     periodic box strain matrix
      real mat(3,3,2)

c other

c     temperature to give the tube
      real temp

c     input line
      character*80 line

c     file name
      character*80 fil
      integer fillt

c executable statements

c     initialize the library
      call libini('chtemp',8)

c     get input file data and output file name
      call coordg(1,kdim,
     & fil,fillt,
     & head,headlt,
     & kmax,idum,nra,nla,
     & ttime,delta,
     & cube,
     & an,itr,r0,r1n,r2,r3,r4,
     & havran,randat,havmat,mat)

c     ask for the temperature
      line='Enter the temperature in K: 0'
 220  call in(line)
      if(line.eq.'quit')goto 9000
      read(line,*,end=230,err=230)temp
      goto 250
 230  line='*** Invalid input.  Enter a nonnegative number: 0'
      goto 220
 250  if(temp.lt.0.)goto 230

c     set the velocities consistent with the temperature
      call sett(kdim,kmax,temp,an,r1n,r3,r4)

c     write the new file
      call coordw(kdim,
     & fil(1:fillt),
     & head(1:headlt),
     & kmax,idum,nra,nla,
     & ttime,delta,
     & cube,
     & an,itr,r0,r1n,r2,r3,r4,
     & havran,randat,havmat,mat)

      call exit(0)
 9000 call exit(1)
      end
