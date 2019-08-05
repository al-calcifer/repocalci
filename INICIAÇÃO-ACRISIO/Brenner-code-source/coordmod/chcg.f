      program chcg

c     Center the center of gravity (mass, really) in a coord.d-type file.

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

c     atom masses
      include '../util/atommass.inc'

c     mass and momentum sums
      real mass,p(3)
      data mass/0./,p/3*0./

c     shift
      real shift(3)

c     new position
      real rnew

c     position changes due to passing a periodic box boundary
      integer flip(3)

c     input line
      character*80 line

c     file name
      character*80 fil
      integer fillt

c     index
      integer i

c executable statements

c     initialize the library
      call libini('chcg',8)

c     get input file data and output file name
      call coordg(1,kdim,
     & fil,fillt,
     & head,headlt,
     & kmax,idum,nra,nla,
     & ttime,delta,
     & cube,
     & an,itr,r0,r1n,r2,r3,r4,
     & havran,randat,havmat,mat)

c     compute center of gravity
      do 500 k=1,kmax
         if(an(k).lt.1 .or. an(k).gt.109)
     &    call out('*** invalid atom number!')
         mass=mass+am(an(k))
         do 450 i=1,3
            p(i)=p(i)+am(an(k))*r0(k,i)
 450     continue
 500  continue
      do 510 i=1,3
         shift(i)=-p(i)/mass
 510  continue
      write(line,520)shift
 520  format('Shift vector:',3f9.2)
      call out(line(1:40))

c     see whether any atoms are going to change position
      do 610 i=1,3
         flip(i)=0
 610  continue
      do 650 k=1,kmax
         do 640 i=1,3
            rnew=r0(k,i)+shift(i)
            if(anint(rnew/cube(i)).ne.0.)flip(i)=flip(i)+1
 640     continue
 650  continue

c     write warnings
      do 800 i=1,3
         if(flip(i).eq.0)goto 800
         write(line,750)flip(i),char(119+i)
 750     format('Warning:',i6,' atoms will cross the periodic box ',
     &    a1,'-boundaries and be relocated.')
         call out(line(1:79))
 800  continue

c     ask to back out
      if(flip(1)+flip(2)+flip(3).gt.0)then
         line='Proceed anyway? (y/n): y'
 920     call in(line)
         if(line.eq.'n')goto 9000
         if(line.ne.'y')then
            line='*** Enter y or n: y'
            goto 920
         endif
      endif

c     change them
      do 1200 k=1,kmax
         do 1190 i=1,3
            rnew=r0(k,i)+shift(i)
            r0(k,i)=rnew-anint(rnew/cube(i))*cube(i)
 1190    continue
 1200  continue

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
