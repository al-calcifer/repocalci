      program defect

c     Create 7557 defects in carbon nanotubes

c     The tubes must be described in a file with a coord.d format, and
c     be centered around the z-axis.

      implicit none

c coord.d data

c     declared number of atoms
      integer kdim
      parameter (kdim=100000)
c     actual number of atoms
      integer kmax
c     atom index
      integer k,k1,k2

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

c     C-C bond distance; 1.42[1] Angstrom for sheets (1.44 for tubes?)
      real acc
      data acc/1.43d0/
c     allowed relative deviation from  this nominal value
      real accerr
      data accerr/0.1/
c     bond reduction factor for the defect
      real accfac
      data accfac/.95/

c     whether the atom has been moved
      integer moved(kdim)

c     count of the number of defects added
      integer defct

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

c     menu choice
      integer choice

c     input line
      character*80 line

c     file name
      character*80 fil
      integer fillt

c     what bailing out will do (quit or done)
      character*4 outstr

c     index
      integer i

c     temporary
      real tmp

c     constants
      real inf,pi
      parameter (inf=1.e30)

c     position reduction functions
      real arg,redx,redy,redz
      redx(arg)=arg-anint(arg/cube(1))*cube(1)
      redy(arg)=arg-anint(arg/cube(2))*cube(2)
      redz(arg)=arg-anint(arg/cube(3))*cube(3)

c executable statements

c     initialize pi
      pi=4.*atan(1.)

c     initialize the library
      call libini('defects',8)

c     get input file data and output file name
      call coordg(1,kdim,
     & fil,fillt,
     & head,headlt,
     & kmax,idum,nra,nla,
     & ttime,delta,
     & cube,
     & an,itr,r0,r1n,r2,r3,r4,
     & havran,randat,havmat,mat)

c     mark all atoms as not moved yet
      do 210 k=1,kmax
         moved(k)=0
 210  continue

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
     & '*** Unacceptable input.  Enter a number between 0.02 and 0.3:'
      write(line(63:67),410)accerr
      goto 420
 450  if(tmp.lt.0.02 .or. tmp.gt.0.3)goto 430
      accerr=tmp

c     add defects

      defct=0

 1000 continue
      call out(' ')
      outstr='quit'
      if(defct.gt.0)outstr='done'

c     ask for a location and find its closest atom, bonds, and hexagon center
      call find(
     & kdim,kmax,cube,an,r0,
     & outstr,
     & acc,accerr,
     & zpos,kvals,hexx,hexy,hexz,ier)
      if(ier.eq.-1)goto 8000
      if(ier.lt.2)goto 1000
      do 1100 i=1,min(4,ier)
         if(moved(kvals(i)).ne.0)then
            line='*** There is already a defect here.  Hit return:'
            call in(line)
            goto 1000
         endif
 1100 continue

c     ask user feedback on the found bonds

c     start a little menu
      call out(
     & '--------------------------------------------------------------')

c     choice 0 is for the user to bail out
      call out('0: None is acceptable')

      do 3000 i=2,min(4,ier)
         x=.5*(r0(kvals(1),1)+r0(kvals(i),1))
         y=.5*(r0(kvals(1),2)+r0(kvals(i),2))
         z=r0(kvals(1),3)-.5*redz(r0(kvals(1),3)-r0(kvals(i),3))
         z=zpos+redz(z-zpos)
         r=sqrt(x**2+y**2)
         th=0.
         if(r.ne.0.)th=atan2(y,x)*(180./pi)
         write(line,2510)i-1,kvals(1),kvals(i),z,r,th
 2510    format(i1,': Possible atom pair',i5,' and',i5,
     &    '   z, r, theta:',f9.2,f9.2,f6.0)
         call out(line(1:74))
         dx=r0(kvals(i),1)-r0(kvals(1),1)
         dy=r0(kvals(i),2)-r0(kvals(1),2)
         dz=redz(r0(kvals(i),3)-r0(kvals(1),3))
         alp=atan2(sqrt(dx**2+dy**2),dz)*(180./pi)
         if(-x*dy+y*dx.lt.0.)alp=-alp
         write(line,2520)alp
 2520    format('   angle with the z-axis of the bond to be rotated:',
     &    f6.0)
         call out(line(1:56))
 3000 continue

c     ask the menu choice
      line='Enter a choice from 0 to 3 from the menu above: 1'
      line(26:26)=char(48+min(4,ier)-1)
 3320 call in(line)
      read(line,*,err=3330,end=3330)choice
      goto 3350
 3330 line='*** a choice from 0 to 3 must be entered: 1'
      line(24:24)=char(48+min(4,ier)-1)
      goto 3320
 3350 if(choice.lt.0 .or. choice.gt.min(4,ier)-1)goto 3330
      if(choice.eq.0)goto 1000

c     create the defect

c     flip the link 90 degrees in the cylinder surface
c     (using the in-plane vector (dx,dy,dz) x (x,y,0) normal to the bond)
      k1=kvals(1)
      k2=kvals(choice+1)
      x=.5*(r0(k1,1)+r0(k2,1))
      y=.5*(r0(k1,2)+r0(k2,2))
      z=r0(k1,3)-.5*redz(r0(k1,3)-r0(k2,3))
      dx=.5*(r0(k2,1)-r0(k1,1))
      dy=.5*(r0(k2,2)-r0(k1,2))
      dz=.5*redz(r0(k2,3)-r0(k1,3))
      tmp=sqrt(x**2+y**2)
      if(tmp.eq.0.)then
         line='*** The bond is on the axis!  Hit return:'
         call in(line)
         goto 1000
      endif
      tmp=accfac/tmp
      r0(k1,1)=x-y*dz*tmp
      r0(k1,2)=y+x*dz*tmp
      r0(k1,3)=z-(x*dy-y*dx)*tmp
      r0(k2,1)=x+y*dz*tmp
      r0(k2,2)=y-x*dz*tmp
      r0(k2,3)=z+(x*dy-y*dx)*tmp

c     the other Nordsieck components will no longer be meaningful, zero them
      do 4100 i=1,3
         r1n(k1,i)=0.
         r1n(k2,i)=0.
         r2(k1,i)=0.
         r2(k2,i)=0.
         r3(k1,i)=0.
         r3(k2,i)=0.
         r4(k1,i)=0.
         r4(k2,i)=0.
 4100 continue

c     mark both atoms as moved
      moved(k1)=1
      moved(k2)=1

c     success
      write(line,4210)k1,k2
 4210 format('Flipped atoms',i5,' and',i5,' over 90 degrees.')
      call out(line(1:44))
      defct=defct+1
      goto 1000

 8000 if(defct.le.0.)goto 9000
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
