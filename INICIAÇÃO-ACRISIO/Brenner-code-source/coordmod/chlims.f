      program chlims

c     Print out the atom position limits of a coord.d file.

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

c     input line
      character*80 line

c     file name
      character*80 fil
      integer fillt

c     index
      integer i

c     position limits
      real lo,hi

c     constants
      real inf
      parameter (inf=1.e20)

c executable statements

c     initialize the library
      call libini('chlims',8)

c     get input file data and output file name
      call coordg(0,kdim,
     & fil,fillt,
     & head,headlt,
     & kmax,idum,nra,nla,
     & ttime,delta,
     & cube,
     & an,itr,r0,r1n,r2,r3,r4,
     & havran,randat,havmat,mat)

c     find the Cartesian atom position limits
      do 1000 i=1,3

         lo=inf
         hi=-inf
         do 500 k=1,kmax
            lo=min(lo,r0(k,i))
            hi=max(hi,r0(k,i))
 500     continue

         write(line,510)
     &    char(119+i),min(99999.99,.5*cube(i)),lo,char(119+i),hi
 510     format('Box size: |',a1,'| <',f9.2,'.  Actual limits:',
     &    f9.2,' < ',a1,' <',f9.2)
         call out(line(1:65))

 1000 continue

c     find the cylindrical limits
      lo=inf
      hi=0.
      do 1100 k=1,kmax
         lo=min(lo,r0(k,1)**2+r0(k,2)**2)
         hi=max(hi,r0(k,1)**2+r0(k,2)**2)
 1100 continue
      write(line,1110)sqrt(lo),sqrt(hi)
 1110 format('Actual radial position limits:',f9.2,' < r <',f9.2)
      call out(line(1:54))

      call exit(0)
 9000 call exit(1)
      end
