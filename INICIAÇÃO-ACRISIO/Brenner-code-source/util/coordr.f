      subroutine coordr(kdim,
     & filnam,
     & head,headlt,
     & kmax,idum,nra,nla,
     & ttime,delta,
     & cube,
     & an,itr,r0,r1n,r2,r3,r4,
     & havran,randat,havmat,mat)

c     Read file filnam in generalized coord.d format.  Uses unit 3.

      implicit none

c input

c     declared number of atoms
      integer kdim

c     name of the file to read
      character*(*) filnam

c output

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

c     local version of the file name
      character*(40) filnm
      integer filnml

c     atom index
      integer k,kk

c     indices
      integer i,j

c executable

c     put file name in local storage (needed for gnu fortran)
      filnm=filnam
      filnml=index(filnm,' ')-1
      if(filnml.lt.1)filnml=40

c     open the file, replacing any pre-existing one
      open(3,file=filnam,status='old',err=900)

c     read the header line
      read(3,110,err=910,end=915)head
 110  format(a80)
c     find its truncated length
      do 120 i=80,1,-1
         headlt=i
         if(head(i:i).gt.' ' .and. head(i:i).le.'~')goto 121
 120  continue
 121  continue

c     read the rest of the header
      read(3,*,err=910,end=915)kmax,idum,nra,nla
      read(3,*,err=910,end=915)ttime,delta
      read(3,*,err=910,end=915)cube
      if(cube(1).le.0. .or. cube(2).le.0. .or. cube(3).le.0.)
     & call out('*** invalid periodic box!')

c     read atom index, atom number, position, and thermostat response
      do 130 k=1,kmax
         read(3,*,err=910,end=915)kk,an(k),(r0(k,i),i=1,3),itr(k)
 130  continue
c     read velocities and remaining Nordsieck parameters
      do 140 k=1,kmax
         read(3,*,err=910,end=915)kk,(r1n(k,i),i=1,3)
 140  continue
      do 150 k=1,kmax
         read(3,*,err=910,end=915)kk,(r2(k,i),i=1,3)
 150  continue
      do 160 k=1,kmax
         read(3,*,err=910,end=915)kk,(r3(k,i),i=1,3)
 160  continue
      do 170 k=1,kmax
         read(3,*,err=910,end=915)kk,(r4(k,i),i=1,3)
 170  continue

c     make sure the positions are as close to the origin as possible
      do 200 i=1,3
         do 190 k=1,kmax
            r0(k,i)=r0(k,i)-anint(r0(k,i)/cube(i))*cube(i)
 190     continue
 200  continue

c     initialize optional attachments
      havran=0
      havmat=0

c     read random numbers attached, if any
      read(3,*,err=910,end=800)randat
      havran=1

c     read periodic box strain matrix, if any
      read(3,*,err=910,end=800)(mat(1,j,1),j=1,3)
      do 210 i=2,3
         read(3,*,err=910,end=915)(mat(i,j,1),j=1,3)
 210  continue
      do 220 i=1,3
         read(3,*,err=910,end=915)(mat(i,j,2),j=1,3)
 220  continue
      havmat=1

c     close file
 800  close(3,err=920)

      call out('File '//filnm(1:filnml)//' read')
      return

 900  call out('*** unable to access file '//filnm(1:filnml))
      call out('    Check file and directory!')
 910  call out('*** unable to read file '//filnm(1:filnml))
      call out('    Check file permissions!')
 915  call out('*** premature end to file '//filnm(1:filnml))
      call out('    Check file and final empty line!')
 920  call out('*** unable to close file '//filnm(1:filnml))
      call out('    Check disk!')
      return
      end
