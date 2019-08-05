      program chbox

c     Change the periodic box size in a coord.d-type file.

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

c     new box size
      real cubnew(3)

c     position limits
      real lo,hi

c     whether a warning was issued
      logical warn

c     constants
      real inf
      parameter (inf=1.e20)

c executable statements

c     initialize the library
      call libini('chbox',8)

c     get input file data and output file name
      call coordg(1,kdim,
     & fil,fillt,
     & head,headlt,
     & kmax,idum,nra,nla,
     & ttime,delta,
     & cube,
     & an,itr,r0,r1n,r2,r3,r4,
     & havran,randat,havmat,mat)

c     check old box size
      if(cube(1).le.0. .or. cube(2).le.0. .or. cube(3).le.0.)
     & call out('*** invalid periodic box!')
      call out('Current box size:')
      write(line,110)
     & min(99999.99,cube(1)),
     & min(99999.99,cube(2)),
     & min(99999.99,cube(3))
 110  format('Current box size:',3f9.2)
      call out(line(1:44))

c     read the new box size
      line='Enter new periodic box size (use 0 for no change): 0,0,0'
 220  call in(line)
      if(line.eq.'quit')goto 9000
      read(line,*,err=230,end=230)cubnew
      goto 250
 230  line='*** Invalid box size.  Enter 3 nonnegative numbers: 0,0,0'
      goto 220
 250  if(cubnew(1).eq.0. .and. cubnew(2).eq.0. .and. cubnew(3).eq.0.)
     & call out('*** no changes entered!')
      if(cubnew(1).lt.0. .or. cubnew(2).lt.0. .or. cubnew(3).lt.0.)
     & goto 230

c     no warning yet
      warn=.false.

c     check the new cube size
      do 1000 i=1,3

c        write the number out as in coordw.d and re-read
         write(line,310)cubnew(i)
 310     format(e20.11)
         read(line,*,err=320,end=320)cubnew(i)
         goto 350
 320     call out('*** PROGRAM ERROR: failed internal read!')
 350     continue

c        no change
         if(cubnew(i).eq.0.)cubnew(i)=cube(i)

c        no problem if the new value is at least as big as the old
         if(cubnew(i).ge.cube(i))goto 1000

c        check whether any atoms will change position coordinates
         lo=inf
         hi=-inf
         do 500 k=1,kmax
            lo=min(lo,r0(k,i))
            hi=max(hi,r0(k,i))
 500     continue
         if(anint(lo/cubnew(i)).ne.0.)then
            write(line,510)-0.5*cubnew(i)-lo,char(119+i)
 510        format('*** Atoms are extending',f9.2,
     &       ' before the box in the ',a1,'-direction;')
            call out(line(1:67))
            call out2('    these atoms will relocate!')
            warn=.true.
         endif
         if(anint(hi/cubnew(i)).ne.0.)then
            write(line,520)hi-0.5*cubnew(i),char(119+i)
 520        format('*** Atoms are extending',f9.2,
     &       ' behind the box in the ',a1,'-direction;')
            call out(line(1:67))
            call out2('    these atoms will relocate!')
            warn=.true.
         endif

 1000 continue

      if(cubnew(1).gt.cube(1) .or. cubnew(2).gt.cube(2)
     & .or. cubnew(3).gt.cube(3))then
         call out(' ')
         call out(
     &    '*** Warning: this subroutine may have unintended results:')
         call out('    Atom sequences that are sticking through '//
     &    'the sides of the periodic box')
         write(line,1110)
     &    min(99999.99,0.5*cube(1)),
     &    min(99999.99,0.5*cube(2)),
     &    min(99999.99,0.5*cube(3))
 1110    format('       |x| <',f9.2,', |y| <',f9.2,', |z| <',f9.2,',')
         call out(line(1:54))
         call out('will be broken up.')
         warn=.true.
      endif

      if(cubnew(1).eq.cube(1) .and. cubnew(2).eq.cube(2)
     & .and. cubnew(3).eq.cube(3))
     & call out('*** Box size is unchanged!')

c     ask to back out
      if(warn)then
         line='Proceed anyway? (y/n): y'
 1120    call in(line)
         if(line.eq.'n')goto 9000
         if(line.ne.'y')then
            line='*** Enter y or n: y'
            goto 1120
         endif
      endif

c     reset the positions
      do 2000 i=1,3
         if(cubnew(i).ge.cube(i))goto 2000
         do 1500 k=1,kmax
            r0(k,i)=r0(k,i)-anint(r0(k,i)/cubnew(i))*cubnew(i)
 1500    continue
 2000 continue

c     write the new file
      call coordw(kdim,
     & fil(1:fillt),
     & head(1:headlt),
     & kmax,idum,nra,nla,
     & ttime,delta,
     & cubnew,
     & an,itr,r0,r1n,r2,r3,r4,
     & havran,randat,havmat,mat)

      call exit(0)
 9000 call exit(1)
      end
