      program chmult

c     Increase the period sizes by integer multiples.

      implicit none

c coord.d data

c     declared number of atoms
      integer kdim
      parameter (kdim=100000)
c     actual number of atoms
      integer kmax,kmax2
c     atom index
      integer k,k0

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

c     period multipliers
      integer mult(3)

c     box indices
      integer ix,iy,iz

c     index
      integer i

c     input line
      character*80 line

c     file name
      character*80 fil
      integer fillt

c executable statements

c     initialize the library
      call libini('chmult',8)

c     get input file data and output file name
      call coordg(1,kdim,
     & fil,fillt,
     & head,headlt,
     & kmax,idum,nra,nla,
     & ttime,delta,
     & cube,
     & an,itr,r0,r1n,r2,r3,r4,
     & havran,randat,havmat,mat)

c     read the period multipliers
      line='Enter the magnification factors of the periods: 1,1,1'
 220  call in(line)
      if(line.eq.'quit')goto 9000
      read(line,*,err=230,end=230)mult
      goto 250
 230  line='*** Invalid multiplier.  Enter 3 natural numbers: 0,0,0'
      goto 220
 250  if(mult(1).le.0 .or. mult(2).le.0 .or. mult(3).le.0)goto 230
      if(mult(1).eq.1 .and. mult(2).eq.1 .and. mult(3).eq.1)
     & call out('*** No changes!')

c     find new number of atoms
      kmax2=kmax*mult(1)*mult(2)*mult(3)
      write(line,310)kmax2
 310  format('New number of atoms:',i6)
      call out(line(1:26))
      if(kmax2.gt.kdim)call out('*** Too many atoms!')

c     displace the current atoms to the bottom of the new box
      do 500 i=1,3
         if(mult(i).eq.1)goto 500
         if(abs(cube(i)).gt.1.e6)call out('*** period in the '//
     &    char(119+i)//'-direction is too big to be multiplied!')
         do 490 k=1,kmax
            r0(k,i)=r0(k,i)-(mult(i)-1)*.5*cube(i)
 490     continue
 500  continue

c     create the atoms
      do 1000 ix=1,mult(1)
         do 990 iy=1,mult(2)
            do 980 iz=1,mult(3)
               do 970 k0=1,kmax
                  k=k0+((ix-1)+mult(1)*((iy-1)+mult(2)*(iz-1)))*kmax
                  an(k)=an(k0)
                  itr(k)=itr(k0)
                  r0(k,1)=r0(k0,1)+(ix-1)*cube(1)
                  r0(k,2)=r0(k0,2)+(iy-1)*cube(2)
                  r0(k,3)=r0(k0,3)+(iz-1)*cube(3)
                  do 960 i=1,3
                     r1n(k,i)=r1n(k0,i)
                     r2(k,i)=r2(k0,i)
                     r3(k,i)=r3(k0,i)
                     r4(k,i)=r4(k0,i)
 960              continue
 970           continue
 980        continue
 990     continue
 1000 continue

c     new kmax
      kmax=kmax2

c     reset positions
      do 1100 i=1,3
         cube(i)=mult(i)*cube(i)
         do 1090 k=1,kmax
            r0(k,i)=r0(k,i)-anint(r0(k,i)/cube(i))*cube(i)
 1090    continue
 1100 continue

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
