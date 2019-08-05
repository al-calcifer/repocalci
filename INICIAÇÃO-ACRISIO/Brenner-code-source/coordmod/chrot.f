      program chrot

c     Rotate the atoms in a coord.d file.

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

c     rotation vector and its length
      real rotvec(3),rotabs

c     rotation angle
      real rotang

c     vectors normal to the rotation vector
      real vn(3),vnstar(3)

c     index
      integer i

c     temporary
      real tmp

c     constants
      real pi

c executable statements

c     initialize pi
      pi=4.*atan(1.)

c     initialize the library
      call libini('chrot',8)

c     get input file data and output file name
      call coordg(1,kdim,
     & fil,fillt,
     & head,headlt,
     & kmax,idum,nra,nla,
     & ttime,delta,
     & cube,
     & an,itr,r0,r1n,r2,r3,r4,
     & havran,randat,havmat,mat)

c     read the desired rotation vector
      line='Enter the rotation vector: 0,0,1'
 220  call in(line)
      if(line.eq.'quit')goto 9000
      read(line,*,err=230,end=230)rotvec
      goto 250
 230  line='*** Invalid vector.  Enter 3 numbers: 0,0,1'
      goto 220
 250  rotabs=sqrt(rotvec(1)**2+rotvec(2)**2+rotvec(3)**2)
      if(rotabs.eq.0.)call out('*** Vector is zero!')
      if(abs(rotabs-1.).gt..001)call out(
     & '*** Warning: vector normalized to length 1')
      rotvec(1)=rotvec(1)/rotabs
      rotvec(2)=rotvec(2)/rotabs
      rotvec(3)=rotvec(3)/rotabs

c     read the desired rotation angle
      line='Enter the rotation angle in degrees: 90'
 320  call in(line)
      if(line.eq.'quit')goto 9000
      read(line,*,err=330,end=330)rotang
      goto 350
 330  line='*** Invalid angle.  Enter a number'
      goto 320
 350  if(rotang.eq.0.)call out('*** Angle is zero!')
      rotang=rotang*pi/180.

c     write a warning and give the user the chance to back out
      call out(' ')
      call out('Warning: this subroutine may have unintended results:')
      call out('The atom images that are rotated are the ones that '//
     & 'are in the periodic')
      call out('box around the origin,')
      write(line,410)
     &    min(99999.99,0.5*cube(1)),
     &    min(99999.99,0.5*cube(2)),
     &    min(99999.99,0.5*cube(3))
 410  format('       |x| <',f9.2,', |y| <',f9.2,', |z| <',f9.2,',')
      call out(line(1:54))
      call out('hence atom sequences that stick through those '//
     & 'boundaries will be broken up.')
      line='Proceed anyway? (y/n): y'
 420  call in(line)
      if(line.eq.'n')goto 9000
      if(line.ne.'y')then
         line='*** Enter y or n: y'
         goto 420
      endif

c     execute the rotation
      do 1000 k=1,kmax

c        component of r0 normal to the rotation vector
         tmp=0.
         do 510 i=1,3
            tmp=tmp+r0(k,i)*rotvec(i)
 510     continue
         do 520 i=1,3
            vn(i)=r0(k,i)-tmp*rotvec(i)
 520     continue

c        cross product of r0 and the rotation vector
         vnstar(1)=rotvec(2)*r0(k,3)-rotvec(3)*r0(k,2)
         vnstar(2)=rotvec(3)*r0(k,1)-rotvec(1)*r0(k,3)
         vnstar(3)=rotvec(1)*r0(k,2)-rotvec(2)*r0(k,1)

c        find the rotated vector
         do 530 i=1,3
            r0(k,i)=r0(k,i)+vn(i)*(cos(rotang)-1.)+vnstar(i)*sin(rotang)
            r0(k,i)=r0(k,i)-anint(r0(k,i)/cube(i))*cube(i)
 530     continue

 1000 continue

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
