      program custom

c     Custom modifications to coord.d files

      implicit none

c coord.d data

c     declared number of atoms
      integer kdim
      parameter (kdim=100000)
c     actual number of atoms
      integer kmax
      data kmax/0/
c     atom index
      integer k

c     file header line and its actual length
      character*(80) head
      integer headlt
      data head/'My own coord.d file!'/,headlt/20/

c     initially: 0 / if xmol was called before the file was written: 3
      integer idum
      data idum/0/
c     probably: number of rigid atoms not to be included in adaptive
c     time step changes
      integer nra
      data nra/0/
c     probably: number of Langevin atoms not to be included in adaptive
c     time step changes
      integer nla
      data nla/0/

c     total time of evolution
      real ttime
      data ttime/0./
c     time step
      real delta
      data delta/0.5/

c     size of the periodic box
      real cube(3)
      data cube/1.e20,1.e20,1.e20/

c     atom number
      integer an(kdim)
      data an/kdim*0/

c     thermostat response:  0: moving  1: moving and thermostated  2: rigid
      integer itr(kdim)
      data itr/kdim*0/

c     first Nordsieck parameter (atom positions)
      real r0(kdim,3)
      data r0/(3*kdim)*0./
c     second Nordsieck parameter /delta (atom velocities)
      real r1n(kdim,3)
      data r1n/(3*kdim)*0./
c     third Nordsieck parameter (atom accelerations * 0.5 delta^2)
      real r2(kdim,3)
      data r2/(3*kdim)*0./
c     fourth Nordsieck parameter
      real r3(kdim,3)
      data r3/(3*kdim)*0./
c     fifth Nordsieck parameter
      real r4(kdim,3)
      data r4/(3*kdim)*0./

c     nonzero if there are random numbers attached to the file
      integer havran
      data havran/0/
c     random numbers data
      integer randat(4)
      data randat/4*0/
c     nonzero if there is a periodic box strain matrix attached
      integer havmat
      data havmat/0/
c     periodic box strain matrix
      real mat(3,3,2)
      data mat/18*0./

c other

c     input line
      character*80 line

c     file name
      character*80 fil
      integer fillt
      data fil/'coord.d'/,fillt/7/

c     C-C bond distance; 1.42[1] Angstrom for sheets (1.44 for tubes?)
      real acc
      data acc/1.43d0/

c     random numbers (uncomment and add ../lib/ranlib.a to the g77 command)

c     ranf() returns uniformly distributed random number between 0 and 1
c     real ranf
c     external ranf

c     gennor(average,standard_deviation) returns normal random numbers
c     real gennor
c     external gennor


c     constants
      real inf,pi
      parameter (inf=1.e30)

c executable statements

c     initialize pi
      pi=4.*atan(1.)

c     initialize the library
      call libini('custom',8)

c     get input file data and output file name
      call coordg(1,kdim,
     & fil,fillt,
     & head,headlt,
     & kmax,idum,nra,nla,
     & ttime,delta,
     & cube,
     & an,itr,r0,r1n,r2,r3,r4,
     & havran,randat,havmat,mat)


c     MAKE THE DESIRED CHANGES TO THE DATA HERE:



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
