      program getxyz

c     Create an xyz plot file from a .d file

c     Note that, apparently, initially in rasmol:
c     x is to the right;
c     y is down;
c     z is into the screen.

      implicit none

c coord.d data

c     declared number of atoms
      integer kdim
      parameter (kdim=100000)
c     actual number of atoms
      integer kmax

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

c     whether a file exists
      logical exists

c executable statements

c     initialize the library
      call libini('getxyz',8)

c     get input file data
      call coordg(0,kdim,
     & fil,fillt,
     & head,headlt,
     & kmax,idum,nra,nla,
     & ttime,delta,
     & cube,
     & an,itr,r0,r1n,r2,r3,r4,
     & havran,randat,havmat,mat)

c     ask for the file to create
      line='Enter the file to create: '//fil(1:fillt-1)//'xyz'
 160  call in(line)
      if(line.eq.'quit')goto 9000
c     set the file name and its length
      fil=line
      fillt=index(fil,' ')-1
c     check for length of 80 or more characters
      if(fillt.lt.0)then
         line='*** File name is too long.  Re-enter: quit'
         goto 160
      endif
c     check for blank spaces in the name
      if(fil(fillt+1:).ne.' ' .or. line.eq.' ')then
         line='*** File names may not contain spaces.  Re-enter: quit'
         goto 160
      endif
c     add .xyz file type if is missing and no other exists
      if(index(fil(1:fillt),'.').lt.1 .and. fillt.le.76)then
         fil=fil(1:fillt)//'.xyz'
         fillt=fillt+4
      endif
c     check for required .xyz file type
      if(fil(max(1,fillt-3):fillt).ne.'.xyz')then
         line='*** Missing .xyz file type.  Re-enter: '//
     &    fil(1:fillt)//'.xyz'
         goto 160
      endif
c     see whether the file exists
      inquire(file=fil(1:fillt),exist=exists,err=170)
      goto 180
 170  line='*** Unable to search for the file. Re-enter: quit'
      goto 160
 180  if(exists)then
         line='    file already exists; OK to overwrite it? (y/n): y'
 190     call in(line)
         if(line.eq.'n')then
            line='    Enter another file name: quit'
            goto 160
         else if(line.ne.'y')then
            line='*** enter y or n: y'
            goto 190
         endif
      endif

c     write the xyz file
      call xyzw(fil(1:fillt),kdim,kmax,an,r0)

      call exit(0)
 9000 call exit(1)
      end
