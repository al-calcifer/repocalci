      subroutine find(
     & kdim,kmax,cube,an,r0,
     & outstr,
     & acc,accerr,
     & zpos,kvals,hexx,hexy,hexz,ier)

c     Ask the user for a location and find the closest atom, bond set,
c     and hexagon center to this position.

c     Results vary with the value of error code ier returned
c     If ier=-1, the user entered quit or done
c     If ier>=1, atom 1 will be the atom closest to the user's desired location
c     If ier>=2, atom 2 will be the 2nd closest, bonded to atom 1
c     If ier>=3, atom 3 will be the 3th closest, bonded to atom 1
c     If ier>=4, atom 4 will be the 4th closest, bonded to atom 1
c     If ier=5, the closest hexagon center has been found

c     This subroutine uses polar coordinates, hence it ignores any
c     periodicity in the x- and y-directions.

c     Programming notes:

c     First the subroutine finds the carbon atom 1 closest to the user's
c     desired point.

c     Then it finds the next closest atom 2 to the desired point, using a
c     distance formula with an added penalty that enforces that atom A
c     has a bond with C when on a hexagonal mesh.  As long as the atoms
c     are close to hexagonal, atom 2 will at least be almost second
c     closest in the normal sense.

c     Then it finds atom 3 being third closest, using penalties for
c     being nearest atoms 1 and at the right position compared to bond
c     1-2.  From that, it finds the closest hexagon center by
c     intersecting lines normal to connections 1-2 and 1-3 in the
c     1-2-3-plane.

c     It also finds an atom 4 closest to the expected third bond
c     direction from atom 1.

      implicit none

c input

c     declared number of atoms
      integer kdim
c     actual number of atoms
      integer kmax
c     size of the periodic box
      real cube(3)
c     atom number
      integer an(kdim)
c     first Nordsieck parameter (atom positions)
      real r0(kdim,3)

c     whether abort is 'quit' or 'done'
      character*4 outstr 

c     C-C bond distance; 1.42[1] Angstrom for sheets (1.44 for tubes?)
      real acc
c     allowed relative deviation from  this nominal value
      real accerr

c output

c     desired z-location
      real zpos
c     k-values of atoms 1 through 4 nearest the user's desired location
      integer kvals(4)
c     hexagon cell center nearest the user's desired location
      real hexx,hexy,hexz
c     error code (see the starting comments for more)
      integer ier

c local

c     location
      real rpos,thpos,xpos,ypos

c     atom closest to the desired loaction
      integer k1
c     atoms bonded with atom 1, in order of closeness to the desired location
      integer k2,k3,k4

c     atom z-limits
      real z1,z2

c     atom r-limits near the desired z
      real zr1,zr2

c     "distance" (see the programming notes) and its minimum value
      real dis,dismin

c     vectors from one point to another
      real a1,a2,a3,b1,b2,b3,c1,c2,c3
c     some dot products of these vectors
      real aa,ab,bb
c     the hexagon center position is given as:
c     (center of bond 1-2) + lam * (component of bond 1-3 normal to bond 1-2)
      real lam
c     square length of component of bond 1-3 normal to bond 1-2
      real betbet

c     atom index
      integer k

c     input line
      character*80 line

c     constants
      real inf,pi
      parameter (inf=1.d20)

c     position reduction function
      real arg,redz
      redz(arg)=arg-anint(arg/cube(3))*cube(3)

c executable

c     initialize pi
      pi=4.*atan(1.)

c     initialize the error code
      ier=0

c     print out the atom z-limits
 1200 z1=inf
      z2=-inf
      do 1210 k=1,kmax
         if(an(k).ne.6)goto 1210
         z1=min(z1,r0(k,3))
         z2=max(z2,r0(k,3))
 1210 continue
      if(z1.eq.inf)call out('*** No carbon atoms!')
      write(line,1211)z1,z2
 1211 format('Axial atom position limits:',2f9.2)
      call out(line(1:45))

c     get the desired z-location from the user
      line='Enter the desired z-position: '//outstr
 1220 call in(line)
      if(line.eq.'quit' .or. line.eq.'done')goto 9000
      read(line,*,end=1230,err=1230)zpos
      goto 1250
 1230 line='*** Invalid number.  Re-enter: '//outstr
      goto 1220
 1250 continue

c     print the radial atom limits near the desired z-location.
 1400 zr1=inf
      zr2=0.
      do 1410 k=1,kmax
         if(an(k).ne.6)goto 1410
         if(abs(redz(zpos-r0(k,3))).gt.acc*(1.+accerr))goto 1410
         zr1=min(zr1,r0(k,1)**2+r0(k,2)**2)
         zr2=max(zr2,r0(k,1)**2+r0(k,2)**2)
 1410  continue
      zr1=sqrt(zr1)
      zr2=sqrt(zr2)
      if(zr2.eq.0.)then
         call out('*** No atoms near this z-position.  ')
      else
         write(line,1411)zr1,zr2
 1411    format('Radial atom position limits near this z:',2f9.2)
         call out(line(1:58))
      endif

c     get the desired radial location from the user
      line='Enter the desired r-position: backup'
 1420 call in(line)
      if(line.eq.'quit' .or. line.eq.'done')goto 9000
      if(line.eq.'backup')goto 1200
      read(line,*,end=1430,err=1430)rpos
      goto 1450
 1430 line='*** Invalid number.  Enter a positive number: backup'
      goto 1420
 1450 if(rpos.le.0.)goto 1430

c     get the desired angular position from the user
 1500 line='Enter the desired theta-position: backup'
 1520 call in(line)
      if(line.eq.'quit' .or. line.eq.'done')goto 9000
      if(line.eq.'backup')goto 1400
      read(line,*,end=1530,err=1530)thpos
      goto 1550
 1530 line='*** Invalid number.  Re-enter: 0'
      goto 1520
 1550 thpos=thpos*(pi/180.)

c     find carbon atom 1 nearest to the desired location
      dismin=inf
      do 2100 k=1,kmax
         if(an(k).ne.6)goto 2100
         a1=rpos*cos(thpos)-r0(k,1)
         a2=rpos*sin(thpos)-r0(k,2)
         a3=redz(zpos-r0(k,3))
         dis=
     &    sqrt(a1**2+a2**2+a3**2)
         if(dis.ge.dismin)goto 2100
         dismin=dis
         k1=k
 2100 continue
      if(k1.eq.0)then
         line='*** No atoms!  Hit return:'
         goto 9100
      endif
      ier=1

c     find atom 2 second closest, with a penalty term being the distance 1-2
      dismin=inf
      do 2200 k=1,kmax
         if(k.eq.k1 .or. an(k).ne.6)goto 2200
         a1=rpos*cos(thpos)-r0(k,1)
         a2=rpos*sin(thpos)-r0(k,2)
         a3=redz(zpos-r0(k,3))
         b1=r0(k1,1)-r0(k,1)
         b2=r0(k1,2)-r0(k,2)
         b3=redz(r0(k1,3)-r0(k,3))
         dis=
     &    sqrt(a1**2+a2**2+a3**2)+
     &    sqrt(b1**2+b2**2+b3**2)
         if(dis.ge.dismin)goto 2200
         dismin=dis
         k2=k
 2200 continue
      if(k2.eq.0)then
         line='*** No second atom! Hit return:'
         goto 9100
      endif
      dis=sqrt(redz(r0(k1,3)-r0(k2,3))**2+
     & (r0(k1,1)-r0(k2,1))**2+(r0(k1,2)-r0(k2,2))**2)
      if(abs(dis-acc).gt.accerr)then
         line='*** The 2nd atom is not at the right distance '//
     &    'from atom 1.  Hit return:'
         goto 9100
      endif
      ier=2

c     find atom 3 next closest, with penalties
      dismin=inf
      do 2300 k=1,kmax
         if(k.eq.k1 .or. k.eq.k2 .or. an(k).ne.6)goto 2300
         a1=rpos*cos(thpos)-r0(k,1)
         a2=rpos*sin(thpos)-r0(k,2)
         a3=redz(zpos-r0(k,3))
         b1=r0(k1,1)-r0(k,1)
         b2=r0(k1,2)-r0(k,2)
         b3=redz(r0(k1,3)-r0(k,3))
         c1=r0(k2,1)-r0(k1,1)
         c2=r0(k2,2)-r0(k1,2)
         c3=redz(r0(k2,3)-r0(k1,3))
         dis=
     &    sqrt(a1**2+a2**2+a3**2)+
     &    sqrt(b1**2+b2**2+b3**2)+
     &    abs(acc-sqrt(max(0.,2.*(b1*c1+b2*c2+b3*c3))))
         if(dis.ge.dismin)goto 2300
         dismin=dis
         k3=k
 2300 continue
      if(k3.eq.0)then
         line='*** No third atom!  Hit return:'
         goto 9100
      endif
      dis=sqrt(redz(r0(k1,3)-r0(k3,3))**2+
     & (r0(k1,1)-r0(k3,1))**2+(r0(k1,2)-r0(k3,2))**2)
      if(abs(dis-acc).gt.accerr)then
         line='*** The 3th atom is not at the right distance '//
     &    'from atom 1.  Hit return:'
         goto 9100
      endif
      ier=3

c     expected position of atom 4
      xpos=3.*r0(k1,1)-r0(k2,1)-r0(k3,1)
      ypos=3.*r0(k1,2)-r0(k2,2)-r0(k3,2)
      zpos=3.*r0(k1,3)-r0(k2,3)-r0(k3,3)

c     find atom 4
      dismin=inf
      do 2400 k=1,kmax
         if(k.eq.k1 .or. k.eq.k2 .or. k.eq.k3 .or. an(k).ne.6)goto 2400
         a1=xpos-r0(k,1)
         a2=ypos-r0(k,2)
         a3=redz(zpos-r0(k,3))
         dis=
     &    sqrt(a1**2+a2**2+a3**2)
         if(dis.ge.dismin)goto 2400
         dismin=dis
         k4=k
 2400 continue
      if(k4.eq.0)then
         line='*** No fourth atom!  Hit return:'
         goto 9100
      endif
      dis=sqrt(redz(r0(k1,3)-r0(k4,3))**2+
     & (r0(k1,1)-r0(k4,1))**2+(r0(k1,2)-r0(k4,2))**2)
      if(abs(dis-acc).gt.accerr)then
         line='*** The 4th atom is not at the right distance '//
     &    'from atom 1.  Hit return:'
         goto 9100
      endif
      ier=4

c     find the hexagon center
      a1=r0(k2,1)-r0(k1,1)
      a2=r0(k2,2)-r0(k1,2)
      a3=redz(r0(k2,3)-r0(k1,3))
      b1=r0(k3,1)-r0(k1,1)
      b2=r0(k3,2)-r0(k1,2)
      b3=redz(r0(k3,3)-r0(k1,3))
      aa=a1**2+a2**2+a3**2
      bb=b1**2+b2**2+b3**2
      ab=a1*b1+a2*b2+a3*b3
      betbet=bb-ab**2/aa
      if(abs(betbet-(cos(30./180.*pi)*acc)**2).gt.2.*acc*accerr
     & .or. betbet.lt.0.125*acc**2)then
         line='*** Deviation from hexagonal cells is too large.  '//
     &    'Hit return:'
         goto 9100
      endif
      lam=.5*(bb-ab)/betbet
      hexx=r0(k1,1)+.5*a1+lam*(b1-ab*a1/aa)
      hexy=r0(k1,2)+.5*a2+lam*(b2-ab*a2/aa)
      hexz=r0(k1,3)+.5*a3+lam*(b3-ab*a3/aa)
      dis=sqrt(redz(r0(k1,3)-hexz)**2+
     & (r0(k1,1)-hexx)**2+(r0(k1,2)-hexy)**2)
      if(abs(dis-acc).gt.accerr)then
         line='*** The hexagon center is not at the right distance '//
     &    'from atom 1.  Hit return:'
         goto 9100
      endif
      dis=sqrt(redz(r0(k2,3)-hexz)**2+
     & (r0(k2,1)-hexx)**2+(r0(k2,2)-hexy)**2)
      if(abs(dis-acc).gt.accerr)then
         line='*** The hexagon center is not at the right distance '//
     &    'from atom 2.  Hit return:'
         goto 9100
      endif
      dis=sqrt(redz(r0(k3,3)-hexz)**2+
     & (r0(k3,1)-hexx)**2+(r0(k3,2)-hexy)**2)
      if(abs(dis-acc).gt.accerr)then
         line='*** The hexagon center is not at the right distance '//
     &    'from atom 3.  Hit return:'
         goto 9100
      endif
      ier=5

c     all done
      goto 9900

c     user quit
 9000 ier=-1
      goto 9900

c     done, but some notice pending
 9100 call in(line)
      goto 9900

c     copy the k-values in the array
 9900 kvals(1)=k1
      kvals(2)=k2
      kvals(3)=k3
      kvals(4)=k4

      return
      end
