      program chfind

c     Find an atom, bond, or hexagon closest to a given position.

c     See subroutine ../util/find.f for algorithm used.

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

c     C-C bond distance; 1.42[1] Angstrom for sheets (1.44 for tubes?)
      real acc
      data acc/1.43d0/
c     allowed relative deviation from  this nominal value
      real accerr
      data accerr/0.1/

c     desired z-location
      real zpos
c     k-values of atoms 1 through 4 nearest the user's desired location
      integer kvals(4)
c     hexagon cell center nearest the user's desired location
      real hexx,hexy,hexz
c     error code returned by subroutine find
      integer ier

c     generic coordinates and changes in coordinates
      real x,y,z,r,th,dx,dy,dz

c     angle with the z-axis
      real alp

c     index
      integer i

c     temporary
      real tmp

c     constants
      real pi

c     position reduction functions
      real arg,redx,redy,redz
      redx(arg)=arg-anint(arg/cube(1))*cube(1)
      redy(arg)=arg-anint(arg/cube(2))*cube(2)
      redz(arg)=arg-anint(arg/cube(3))*cube(3)

c executable statements

c     initialize pi
      pi=4.*atan(1.)

c     initialize the library
      call libini('chfind',8)

c     get input file data and output file name
      call coordg(0,kdim,
     & fil,fillt,
     & head,headlt,
     & kmax,idum,nra,nla,
     & ttime,delta,
     & cube,
     & an,itr,r0,r1n,r2,r3,r4,
     & havran,randat,havmat,mat)

c     put all atoms in the nearest xy-cylinder
      do 220 k=1,kmax
         r0(k,1)=redx(r0(k,1))
         r0(k,2)=redy(r0(k,2))
 220  continue

c     ask for the allowed deviation from the nominal nanotube dimensions
      line='Enter the allowed relative deviation from nominal '//
     & 'tube dimensions:'
      write(line(68:72),410)accerr
 410  format(f5.2)
 420  call in(line)
      read(line,*,err=430,end=430)tmp
      goto 450
 430  line=
     & '*** Unacceptable input.  Enter a number between 0.02 and 0.9:'
      write(line(63:67),410)accerr
      goto 420
 450  if(tmp.lt.0.02 .or. tmp.gt.0.9)goto 430
      accerr=tmp

c     find the atom, bonds, and hexagon center closest to a desired location

 1000 call find(
     & kdim,kmax,cube,an,r0,
     & 'quit',
     & acc,accerr,
     & zpos,kvals,hexx,hexy,hexz,ier)
      if(ier.eq.-1)goto 8000

c     print closest atom
      if(ier.gt.0)then
         write(line,1110)kvals(1)
 1110    format('Closest carbon atom is atom',i6)
         call out(line(1:33))
         x=r0(kvals(1),1)
         y=r0(kvals(1),2)
         z=zpos+redz(r0(kvals(1),3)-zpos)
         r=sqrt(x**2+y**2)
         th=0.
         if(r.ne.0.)th=atan2(y,x)*(180./pi)
         write(line,1120)x,y,z,r,th
 1120    format('   position:    x:',f9.2,', y:',f9.2,', z:',f9.2,
     &    ', r:',f9.2,', theta:',f5.0)
         call out(line(1:78))
      endif

c     print bonds
      do 2000 i=2,min(ier,4)
         write(line,1510)i-1,kvals(1),kvals(i)
 1510    format('Bond ',i1,' between atoms',i6,' and',i6)
         call out(line(1:36))
         x=.5*(r0(kvals(1),1)+r0(kvals(i),1))
         y=.5*(r0(kvals(1),2)+r0(kvals(i),2))
         z=r0(kvals(1),3)-.5*redz(r0(kvals(1),3)-r0(kvals(i),3))
         z=zpos+redz(z-zpos)
         r=sqrt(x**2+y**2)
         th=0.
         if(r.ne.0.)th=atan2(y,x)*(180./pi)
         write(line,1120)x,y,z,r,th
         call out(line(1:78))
         dx=r0(kvals(i),1)-r0(kvals(1),1)
         dy=r0(kvals(i),2)-r0(kvals(1),2)
         dz=redz(r0(kvals(i),3)-r0(kvals(1),3))
         alp=atan2(sqrt(dx**2+dy**2),dz)*(180./pi)
         if(-x*dy+y*dx.lt.0.)alp=-alp
         write(line,1530)alp
 1530    format('   angle with z-axis:',f5.0)
         call out(line(1:25))
 2000 continue

      if(ier.ge.5)then
         call out('Closest hexagon')
         x=hexx
         y=hexy
         z=zpos+redz(hexz-zpos)
         r=sqrt(x**2+y**2)
         th=0.
         if(r.ne.0.)th=atan2(y,x)*(180./pi)
         write(line,1120)x,y,z,r,th
         call out(line(1:78))
      endif

c     repeat
      goto 1000

 8000 call exit(0)
      end
