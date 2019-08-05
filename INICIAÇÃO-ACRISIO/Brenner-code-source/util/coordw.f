      subroutine coordw(kdim,
     & filnam,
     & head,
     & kmax,idum,nra,nla,
     & ttime,delta,
     & cube,
     & an,itr,r0,r1n,r2,r3,r4,
     & havran,randat,havmat,mat)

c     Create file filnam in generalized coord.d format.  Uses unit 3.

      implicit none

c input

c     declared number of atoms
      integer kdim

c     name of the file to create, overwritten if it already exists
      character*(*) filnam

c     file header line
      character*(*) head

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
      integer k

c     indices
      integer i,j

c executable

c     put file name in local storage (needed for gnu fortran)
      filnm=filnam
      filnml=index(filnm,' ')-1
      if(filnml.lt.1)filnml=40

c     open the file, replacing any pre-existing one
      call opennw(3,filnam)

c     write the header
      write(3,110,err=910)head
 110  format(a)
      write(3,120,err=910)kmax,idum,nra,nla
 120  format(4i6)
      write(3,130,err=910)ttime,delta
      write(3,130,err=910)cube
 130  format(3e20.11)

c     write atom index, atom number, position, and thermostat response
      write(3,140,err=910)
     & (k,an(k),r0(k,1),r0(k,2),r0(k,3),itr(k),k=1,kmax)
 140  format(2i5,3e16.7,i3)
c     write velocities and remaining Nordsieck parameters
      write(3,150,err=910)(k,r1n(k,1),r1n(k,2),r1n(k,3),k=1,kmax)
      write(3,150,err=910)(k,r2(k,1),r2(k,2),r2(k,3),k=1,kmax)
      write(3,150,err=910)(k,r3(k,1),r3(k,2),r3(k,3),k=1,kmax)
      write(3,150,err=910)(k,r4(k,1),r4(k,2),r4(k,3),k=1,kmax)
 150  format(i5,3e16.7)

c     write random numbers
      if(havran.eq.0)goto 200
      write(3,160,err=910)randat
 160  format(4i15)

c     write periodic box strain matrix
      if(havmat.eq.0)goto 200
      write(3,170,err=910)((mat(i,j,1),j=1,3),i=1,3)
      write(3,170,err=910)((mat(i,j,2),j=1,3),i=1,3)
 170  format(g14.7,',',g14.7,',',g14.7)

c     close file
 200  close(3,err=920)

      call out('File '//filnm(1:filnml)//' generated')
      return

 910  call out('*** unable to write to file '//filnm(1:filnml))
      call out('    Check disk space, permissions, and quota!')
 920  call out('*** unable to close file '//filnm(1:filnml))
      call out('    Check disk space, permissions, and quota!')
      return
      end
