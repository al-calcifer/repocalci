      subroutine coordg(getout,kdim,
     & fil,fillt,
     & head,headlt,
     & kmax,idum,nra,nla,
     & ttime,delta,
     & cube,
     & an,itr,r0,r1n,r2,r3,r4,
     & havran,randat,havmat,mat)

c     Get the data from a user-specified coord.d-type input file.

c     If getout is nonzero, also get the name of the coord.d-type output
c     file to create.

c     Writes a backup file if input and output files are the same.

      implicit none

c input

c     nonzero to also get the output file name
      integer getout

c     declared number of atoms
      integer kdim

c output

c     name of the output file and its length
      character*(80) fil
      integer fillt

c     file header line and its length
      character*(80) head
      integer headlt

c     actual number of atoms
      integer kmax
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
c     second Nordsieck parameter /Delta t (atom velocities)
      real r1n(kdim,3)
c     third Nordsieck parameter (atom accelerations * 0.5 dt^2)
      real r2(kdim,3)
c     fourth Nordsieck parameter
      real r3(kdim,3)
c     fifth Nordsieck parameter
      real r4(kdim,3)

c     nonzero if there are random numbers attached
      integer havran
c     random numbers data
      integer randat(4)
c     nonzero if there is a periodic box strain matrix attached
      integer havmat
c     periodic box strain matrix
      real mat(3,3,2)

c local

c     input file name and its length
      character*(80) ifil
      integer ifillt

c     whether a file exists
      logical exists

c     input line
      character*(80) line

c executable

c     ask for the coord.d-type file to read
      line='Enter the coord.d-type file to read: quit'
 120  call in(line)
      if(line.eq.'quit')goto 9000
c     set the file name and its length
      ifil=line
      ifillt=index(ifil,' ')-1
c     check for length of 80 or more characters
      if(ifillt.lt.0)then
         line='*** File name is too long.  Re-enter: quit'
         goto 120
      endif
c     check for blank spaces in the name
      if(ifil(ifillt+1:).ne.' ' .or. line.eq.' ')then
         line='*** File names may not contain spaces.  Re-enter: quit'
         goto 120
      endif
c     add .d file type if is missing and no other exists
      if(index(ifil(1:ifillt),'.').lt.1 .and. ifillt.le.76)then
         ifil=ifil(1:ifillt)//'.d'
         ifillt=ifillt+2
      endif
c     check for required .d file type
      if(ifil(max(1,ifillt-1):ifillt).ne.'.d')then
         line='*** Missing .d file type.  Re-enter: '//
     &    ifil(1:ifillt)//'.d'
         goto 120
      endif
c     see whether there is enough space for a .bak file type
      if(ifillt.gt.78)then
         line='*** File name is too long.  Re-enter: quit'
         goto 120
      endif
c     see whether the file exists
      inquire(file=ifil(1:ifillt),exist=exists,err=130)
      goto 140
 130  call out('*** Unable to search for the file; check disk!')
 140  if(.not.exists)then
         line='*** File does not exist.  Re-enter: quit'
         goto 120
      endif

c     read the file
      call coordr(kdim,
     & ifil(1:ifillt),
     & head,headlt,
     & kmax,idum,nra,nla,
     & ttime,delta,
     & cube,
     & an,itr,r0,r1n,r2,r3,r4,
     & havran,randat,havmat,mat)

c     initialize the output file name
      fil=ifil
      fillt=ifillt

c     done if no output file needed
      if(getout.eq.0)goto 8000

c     ask for the file to create
      line='Enter the file to create: '//fil(1:fillt)
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
c     add .d file type if is missing and no other exists
      if(index(fil(1:fillt),'.').lt.1 .and. fillt.le.78)then
         fil=fil(1:fillt)//'.d'
         fillt=fillt+2
      endif
c     check for required .d file type
      if(fil(max(1,fillt-1):fillt).ne.'.d')then
         line='*** Missing .d file type.  Re-enter: '//
     &    fil(1:fillt)//'.d'
         goto 160
      endif
c     see whether the output file is the input one
      if(fil(1:fillt).eq.ifil(1:ifillt))then
c        write a backup file
         call coordw(kdim,
     &    ifil(1:ifillt-1)//'bak',
     &    head(1:headlt),
     &    kmax,idum,nra,nla,
     &    ttime,delta,
     &    cube,
     &    an,itr,r0,r1n,r2,r3,r4,
     &    havran,randat,havmat,mat)
         goto 8000
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

c     all done
 8000 return

c     abort
 9000 call exit(1)
      end
