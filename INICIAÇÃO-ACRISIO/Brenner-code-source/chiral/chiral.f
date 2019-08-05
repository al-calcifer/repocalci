      program chiral

c     Creates single and multi-wall nanotubes
c     Can also be used to merge coord.d files.

c     If more than one nanotube is entered, the program determines
c     whether the periods have compatible lengths and, if so, the
c     program will find a common axial period for all tubes.

c     If no exact common period exists, the program can make some attempt
c     to minimize maximum strain given a user-selected number of periods
c     for the tube with the longest period.  There is no proof that the
c     minimum strain is indeed achieved.

c     Produces file coord.d for running the Brenner code.  Any existing
c     file with that name will be overwritten without warning.


c     Programming notes:

c     Vectors are expressed in terms of a basis in which the first
c     vector extends from a cell center to a neighboring cell center,
c     and the second vector is rotated 60 degrees clockwise from the
c     first.  In this basis, cell centers have integer coordinates and
c     atoms have coordinates that are multiples of 1/3.  Coordinates are
c     indicated as n,m.

c     This program uses an internal unit of length that makes dot products
c     integer for vectors with coordinates a multiple of 1/3.  Thus dot
c     products are evaluated exactly for atom positions.


      implicit none


c dimensions

c     C-C bond distance; 1.42[1] Angstrom for sheets (1.44 for tubes?)
      real acc
      data acc/1.42d0/
c     cell center spacing
      real a
c     internal unit of length; equals sqrt(a**2/18)
      real l


c primes

c     declared number of primes (increase if the program tells you)
      integer pridim
      parameter (pridim=5000)
c     prime index
      integer pri
c     prime values
      integer prival(pridim)


c nanotube storage

c     declared number of nanotubes
      integer tubdim
      parameter (tubdim=100)
c     actual number of nanotubes
      integer tubmax
      data tubmax/0/
c     nanotube index
      integer tub,tub2

c     source of the tube: 1=computer, 2=read from a .d file
      integer tubsrc(tubdim)
c     chiral (perimeter) vector in terms of the program's basis
      integer tubcn(tubdim),tubcm(tubdim)
c     translational (axial period) vector in terms of the basis
      integer tubtn(tubdim),tubtm(tubdim)
c     radius and period length in physical units
      real tubr(tubdim),tubp(tubdim)
c     radius and period length stretching factors
      real tubrf(tubdim),tubpf(tubdim)
c     number of periods to use
      integer tubpct(tubdim)
c     tube displacement vector and degrees angular rotation around the z-axis
      real tubdis(tubdim,3),tubang(tubdim)
c     positions of first and last atoms of the tube in the bulk atom storage
      integer tubat1(tubdim),tubat2(tubdim)


c bulk atom storage (with data for only one period of each tube.)

c     declared number of atoms
      integer atdim
      parameter (atdim=100000)
c     actual number of atoms
      integer atmax
      data atmax/0/
c     atom index
      integer at

c     atom "coordinates" (dot products with the chiral and translation vectors)
      integer atc(atdim),att(atdim)
c     atom positions as read from .d files
      real atx(atdim),aty(atdim),atz(atdim)
c     values of atom numbers and motion switches read from .d files
      integer atano(atdim),atitr(atdim)

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

c     temperature to give the tube
      real temp

c     total single or multi-wall nanotube length
      real totlt
c     total number of atoms
      integer totat

c     a table of nanotube length compatibilities
      integer ctdim
      parameter (ctdim=15)
      character*1 ctable(ctdim,0:ctdim)
      data ctable/(ctdim*(ctdim+1))*' '/
c     characters used to indicate length-compatible tubes
      character*94 chstr

c     reference tube
      integer tubref
c     shortest tube
      integer tublo
c     longest tube
      integer tubhi
c     strain
      real strain
c     amount of periods of the reference tube to take
      integer perct,perct1,perct2

c     the square lengths of chiral and translation vectors in internal uits
      integer c2,t2

c     atom vector dotted with chiral and translation vectors
      integer ca,ta

c     coordinates in the basis described at the start
      integer n,m,n2,m2

c     search range for atoms in the period
      integer nlim

c     whether the tubes are length compatible
      logical ltcom

c     whether we want to correct tube lengths to the periodic box size
      logical ltcorr

c     length correction factor
      real ltfac

c     factorizations of numbers
      integer fac1(pridim),fac2(pridim)
c     last prime present in the factorizations
      integer primx1,primx2
c     common power
      integer pow

c     number of global axial periods to create
      integer globct

c     radius to search for and the allowed error
      real srchr,srchdr
c     actual error and its lowest value so far
      real srcher,srchlo
c     length compatibility
      integer lcom,lcomlo

c     file name
      character*80 fil
      integer fillt

c     whether the file exists
      logical exists

c     index
      integer i

c     output line counter, for paging
      integer linect

c     temporaries
      real tmp
      integer itmp,itmp2

c     atom sort step size
      integer atint
c     whether we changed positions
      integer change

c     input line
      character*80 line

c     character index
      integer ich

c     function that determines whether two tubes are length compatible
      logical compat
      external compat
c     number of periods to make the tubes the same length, if compatible
      integer mult1,mult2

c     function that returns the largest common divisor
      integer comdiv
      external comdiv

c constants

c     pi, infinity
      real pi,inf
      parameter (inf=1.e30)

c statement functions

c     coordinates n,m scaled up by a factor 3 to eliminate fractions for atoms
      integer u1,v1,u2,v2

c     dot product in internal units in terms of these scaled-up coordinates
      integer dot
      dot(u1,v1,u2,v2)=2*u1*u2+2*v1*v2+u1*v2+v1*u2


c executable statements

c     initialize pi
      pi=4.*atan(1.)

c     initialize the library
      call libini('chiral',8)

c     initialize the primes
      call prifnd(pridim,prival)
      write(line,10)prival(pridim)
 10   format('Maximum prime:',i6)
      call out(line(1:20))

c     ask for bond length
      line='Enter C-C bond length in Angstroms: 1.23456'
      write(line(37:43),110)acc
 110  format(f7.5)
 120  call in(line)
      read(line,*,end=130,err=130)tmp
      goto 150
 130  line='*** Invalid input.  Enter a positive number: 1.23456'
      write(line(46:52),110)acc
      goto 120
 150  if(tmp.le.0.)goto 130
      acc=tmp

c     cell center spacing
      a=sqrt(3.)*acc
c     internal unit of length
      l=sqrt(a**2/18.)

c     ask for the temperature
      line='Enter the temperature in K: 0'
 220  call in(line)
      read(line,*,end=230,err=230)temp
      goto 250
 230  line='*** Invalid input.  Enter a nonnegative number: 0'
      goto 220
 250  if(temp.lt.0.)goto 230

c     ask for the time step
      line='Enter the time step to put in coord.d: 0.5'
 260  call in(line)
      read(line,*,end=270,err=270)delta
      goto 280
 270  line='*** time step is invalid.  Enter a positive number: 0.5'
      goto 260
 280  if(delta.le.0.)goto 270

c     print a table of nanotube length compatibilities
      call out(' ')
      call out('Nanotube length compatibility table, without mirrors:')
      chstr='^_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'//
     & '1234567890`~!@#$%&*()-+={}[]|:;"''<>.,/? '
      ich=0
      do 350 n=1,ctdim
         do 340 m=0,n
c           already found compatibility group?
            if(ctable(n,m).ne.' ')goto 340
c           next character for the new compatbility group
            ich=ich+1
            ctable(n,m)=chstr(ich:ich)
            if(ctable(n,m).eq.' ')goto 351
c           see what other tubes are compatible with it
            do 330 n2=n,ctdim
               do 320 m2=0,n2
                  if(ctable(n2,m2).ne.' ')goto 320
                  if(compat(
     &             pridim,prival,
     &             n,m,n2,m2,
     &             fac1,fac2,
     &             mult1,mult2))ctable(n2,m2)=ctable(n,m)
 320           continue
 330        continue
 340     continue
 350  continue
 351  continue
      write(line,361)(m,m=0,ctdim)
 361  format('n\\m:',25i3,' ')
      call out(line(1:5+3*(ctdim+1)))
      do 370 n=1,ctdim
         write(line,362)n,(ctable(n,m),m=0,n)
 362     format(i2,'  ',25a3)
         call out2(line(1:4+3*(ctdim+1)))
 370  continue

c     initialize the number of nanotubes
      tubmax=0


c     get the data for each tube

 1000 continue

c     write summary of the tubes defined so far
      call out(' ')
      if(tubmax.gt.0)then
         call out('Radii of the tubes defined so far:')
         ich=0
         do 1030 tub=1,tubmax
            write(line(ich+1:ich+10),1020)tubr(tub)
 1020       format(f10.2)
            ich=ich+10
            if(tub.eq.tubmax.or.ich+10.gt.79)then
               call out(line(1:ich))
               ich=0
            endif
 1030    continue
         call out(' ')
      endif

c     check for storage
      if(tubmax.ge.tubdim)then
         line=
     &    '*** Out of storage; will process tubes so far.  Hit Return:'
         call in(line)
         if(line.eq.'undo')then
            atmax=atmax-(tubat2(tubmax)-tubat1(tubmax)+1)
            tubmax=tubmax-1
            goto 1000
         endif
         goto 5000
      endif

c     ask what to do next using a little menu
      line='Choice: vec'
      call out('Enter one of:')
      call out('        vec    (add a tube with a given chiral vector)')
      call out('        rad    (search for a tube of a desired radius')
      call out(
     & '        file   (adds the data from a coord.d-type file)')
      if(tubmax.gt.0)then
         call out('        undo   (to delete the last tube again)')
         call out('        done   (to process the tubes so far)')
         line='Choice: done'
      endif
      call out('        quit   (aborts program)')
 1050 call in(line)

c     process choice quit
      if(line.eq.'quit')goto 9000

c     process choice done
      if(line.eq.'done')goto 5000

c     process choice undo
      if(line.eq.'undo')then
         if(tubmax.gt.0)then
            atmax=atmax-(tubat2(tubmax)-tubat1(tubmax)+1)
            tubmax=tubmax-1
         endif
         goto 1000
      endif

c     process choice file
      if(line.eq.'file')then

c        ask for the file to read
         line='Enter the coord.d type file to read: cancel'
 1060    call in(line)
         if(line.eq.'cancel')goto 1000
         fil=line
         fillt=index(fil,' ')-1
         if(fillt.lt.1)then
            line='*** File name is too long.  Re-enter: cancel'
            goto 1060
         endif
         if(fil(fillt+1:).ne.' ')then
            line=
     &       '*** File names may not contain spaces.  Re-enter: cancel'
            goto 1060
         endif
         if(index(fil(1:fillt),'.d').lt.1)then
            line='*** Missing .d file type.  Re-enter: '//
     &       fil(1:fillt)//'.d'
            goto 1060
         endif
         inquire(file=fil(1:fillt),exist=exists,err=1070)
         goto 1080
 1070    call out(
     &    '*** Unable to search for the file; check name and disk!')
 1080    if(.not.exists)then
            line='*** File does not exist.  Re-enter: cancel'
            goto 1060
         endif

c        read the file
         call coordr(kdim,
     &    fil(1:fillt),
     &    head,headlt,
     &    kmax,idum,nra,nla,
     &    ttime,delta,
     &    cube,
     &    an,itr,r0,r1n,r2,r3,r4,
     &    havran,randat,havmat,mat)
         if(kmax.lt.1)call out('*** No atoms in file!')
         if(atmax+kmax.gt.atdim)
     &    call out('*** bulk atom storage exceeded!')

c        add to the data
         tubmax=tubmax+1
         tub=tubmax
         tubsrc(tub)=2
         tubp(tub)=cube(3)
         if(tubp(tub).le.0.)
     &    call out('*** axial period in file is nonpositive!')
         tubr(tub)=0.
         tubat1(tub)=atmax+1
         tubat2(tub)=atmax+kmax
         itmp=0
         do 1090 k=1,kmax
            atx(atmax+k)=r0(k,1)
            aty(atmax+k)=r0(k,2)
            atz(atmax+k)=r0(k,3)
            atano(atmax+k)=an(k)
            atitr(atmax+k)=itr(k)
            if(an(k).ne.6)goto 1090
            itmp=itmp+1
            tubr(tub)=tubr(tub)+atx(atmax+k)**2+aty(atmax+k)**2
 1090    continue
         tubr(tub)=max(.5*acc,sqrt(tubr(tub)/(itmp+1.e-7)))
         atmax=atmax+kmax
         goto 1500

      endif

c     process radius choice
      if(line.eq.'rad')then

c        ask for the radius to find and its tolerance
 1110    line='Enter the desired radius and allowable error: cancel'
 1120    call in(line)
         if(line.eq.'cancel')goto 1000
         read(line,*,end=1130,err=1130)srchr,srchdr
         goto 1150
 1130    line=
     &    '*** Invalid numbers.  Enter r,dr with r > dr > 0: cancel'
         goto 1120
 1150    if(srchdr.le.0 .or. srchr.le.srchdr)goto 1130

c        list all matching tubes
         call out('List of matching tubes and their error as '//
     &    'percent of allowed:')
         call out('Length-compatible tubes are marked with a *')
         linect=0
         n2=0
         m2=0
         srchlo=inf
         lcomlo=0
         ich=0
         do 1180 n=1,99
            do 1170 m=0,n
c              find error in radius
               c2=dot(3*n,3*m,3*n,3*m)
               srcher=sqrt(c2+0.)*l/(2.*pi)-srchr
c              skip if unacceptable
               if(abs(srcher).gt.srchdr)goto 1169
c              write the found possible chiral vector
               write(line(ich+1:ich+15),1160)
     &          n,m,min(99,max(-99,nint(srcher/srchdr*100.)))
 1160          format(i2,',',i2,' (',i3,'%)   ')
               ich=ich+15
c              see whether the tube is length compatible with the earlier ones
               lcom=0
               do 1165 tub=1,tubmax
                  if(.not.compat(
     &             pridim,prival,
     &             tubcn(tub),tubcm(tub),n,m,
     &             fac1,fac2,
     &             mult1,mult2))goto 1166
 1165          continue
               lcom=1
               line(ich-2:ich-2)='*'
c              done if the tube is worse than the one we already have
 1166          if(lcom.lt.lcomlo)goto 1169
               if(lcom.eq.lcomlo .and. abs(srcher).gt.srchlo)goto 1169
               n2=n
               m2=m
               srchlo=abs(srcher)
               lcomlo=lcom
c              dump the line to screen
 1169          if(ich.gt.0 .and. (ich+15.gt.79 .or. m.eq.n))then
                  if(linect.eq.20)then
                     line='hit return for more:'
                     call in(line)
                     if(line.eq.'quit' .or. line.eq.'q')goto 1181
                     linect=0
                  endif
                  call out(line(1:ich))
                  linect=linect+1
                  ich=0
               endif
 1170       continue
 1180    continue
 1181    continue

c        if no tubes found, repeat
         if(n2.eq.0)then
            line='No matching tubes found.  Hit return:'
            goto 1110
         endif

c        go ask for the chiral vector to use
         line='Enter the desired chiral vector: '
         write(line(34:38),1190)n2,m2
 1190    format(i2,',',i2)
         goto 1220

      endif

c     the only remaining valid choice is vec

      if(line.ne.'vec')then
         line='*** Invalid choice.  Enter an exact keyword from the '//
     &    'menu above: done'
         if(tubmax.eq.0)line(66:69)='quit'
         goto 1050
      endif

c     process chiral vector choice

 1200 line='Enter the chiral vector n,m: cancel'
 1220 call in(line)
      if(line.eq.'cancel')goto 1000
      read(line,*,end=1230,err=1230)n,m
      goto 1250
 1230 line='*** Invalid chiral vector.  Enter n,m with n > 0, '//
     & 'm >= 0: cancel'
      goto 1220
 1250 if(n.eq.0. .and. m.eq.0)goto 1230
      if(n.le.0 .or. m.lt.0)then
         do 1251 i=1,5
            n2=n
            m2=m
            n=-m2
            m=n2+m2
            if(n.gt.0 .and. m.ge.0)then
               line='*** enter n>0, m>=0:'
               write(line(22:79),*)n,',',m
               goto 1220
            endif
 1251    continue
         call out('*** PROGRAM ERROR: unable to convert chiral vector!')
      endif

c     new tube number
      tubmax=tubmax+1
      tub=tubmax
      tubsrc(tub)=1

c     store the chiral vector
      tubcn(tub)=n
      tubcm(tub)=m

c     find the translation (axial) vector
      call trans(
     & pridim,prival,
     & tubcn(tub),tubcm(tub),
     & fac1,fac2,
     & tubtn(tub),tubtm(tub))
      write(line,1260)tubtn(tub),tubtm(tub)
 1260 format('Translation (axial) vector:',2i4)
      call out(line(1:35))

c     the square lengths of chiral and translation vectors in internal units
      c2=dot(3*tubcn(tub),3*tubcm(tub),3*tubcn(tub),3*tubcm(tub))
      t2=dot(3*tubtn(tub),3*tubtm(tub),3*tubtn(tub),3*tubtm(tub))

c     search for atoms in the unit period and put in the atom storage

c     initialize the storage range in the atom data base
      tubat1(tub)=atmax+1
      tubat2(tub)=atmax

c     how far away we should be searching for atoms in the unit period
      nlim=(sqrt(c2+t2+0.)+1.)/3.+1.1

c     search the cell centers in the range
      do 1400 n=-nlim,nlim
         do 1390 m=-nlim,nlim

c           atom at 1/3,1/3 coordinates from the current cell center:

c           dot atom position with chiral and translation vectors
            ca=dot(3*tubcn(tub),3*tubcm(tub),3*n+1,3*m+1)
            ta=dot(3*tubtn(tub),3*tubtm(tub),3*n+1,3*m+1)
c           add atom if in the period
            if(ca.ge.0 .and. ta.ge.0 .and. ca.lt.c2 .and. ta.lt.t2)then
               atmax=atmax+1
               if(atmax.gt.atdim)call out('*** Out of storage!')
               atc(atmax)=ca
               att(atmax)=ta
               tubat2(tub)=atmax
            endif

c           atom at -1/3,-1/3 coordinates from the current cell center:

c           dot atom position with chiral and translation vectors
            ca=dot(3*tubcn(tub),3*tubcm(tub),3*n-1,3*m-1)
            ta=dot(3*tubtn(tub),3*tubtm(tub),3*n-1,3*m-1)
c           add atom if in the period
            if(ca.ge.0 .and. ta.ge.0 .and. ca.lt.c2 .and. ta.lt.t2)then
               atmax=atmax+1
               if(atmax.gt.atdim)call out('*** Out of storage!')
               atc(atmax)=ca
               att(atmax)=ta
               tubat2(tub)=atmax
            endif

 1390    continue
 1400 continue

c     print total number of atoms
      totat=tubat2(tub)-tubat1(tub)+1
      write(line,1410)totat
 1410 format('Atoms per period:',i5)
      call out(line(1:22))
      if(totat.lt.4)call out('*** PROGRAM ERROR: too few atoms!')

c     sort the atoms with the perimeter going fastest (just to be neat)
      atint=totat/2
 1420 change=0
      do 1430 at=tubat1(tub),tubat2(tub)-atint
         if(att(at).gt.att(at+atint) .or.
     &    (att(at).eq.att(at+atint) .and. atc(at).gt.atc(at+atint)))then
            itmp=atc(at)
            atc(at)=atc(at+atint)
            atc(at+atint)=itmp
            itmp=att(at)
            att(at)=att(at+atint)
            att(at+atint)=itmp
            change=1
         endif
 1430 continue
      if(change.ne.0)goto 1420
      atint=atint/2
      if(atint.ge.1)goto 1420

c     compute period and radius
      tubp(tub)=sqrt(t2+0.)*l
      tubr(tub)=sqrt(c2+0.)*l/(2.*pi)

c     check total number of atoms by comparing cylinder and total atom areas
      tmp=tubr(tub)*2.*pi*tubp(tub)/(.75*a*acc*totat)
      write(line,1440)tmp-1.
 1440 format('Tube area error:',e11.2)
      call out(line(1:27))
      if(abs(tmp-1.).gt.0.1/totat)
     & call out('*** PROGRAM ERROR: areas do not match!')

c     read nanotube length stretching factor
 1500 line='Enter the length stretching factor: 1'
 1520 call in(line)
      read(line,*,end=1530,err=1530)tubpf(tub)
      goto 1550
 1530 line=
     & '*** Invalid input.  Enter a positive number: 1'
      goto 1520
 1550 if(tubpf(tub).le.0.)goto 1530
      tubp(tub)=tubp(tub)*tubpf(tub)
      write(line,1560)min(99999.99,tubp(tub))
 1560 format('Nanotube axial period length:',f12.5)
      call out(line(1:41))

c     read nanotube radius stretching factor
      line='Enter the radius stretching factor: 1'
 1620 call in(line)
      read(line,*,end=1630,err=1630)tubrf(tub)
      goto 1650
 1630 line=
     & '*** Invalid input.  Enter a positive number: 1'
      goto 1620
 1650 if(tubrf(tub).le.0.)goto 1630
      tubr(tub)=tubr(tub)*tubrf(tub)
      write(line,1660)tubr(tub)
 1660 format('Nanotube radius:',f12.5)
      call out(line(1:28))

c     read axial displacement
      line='Enter any x, y, z (axial) tube displacements: 0,0,0'
 1720 call in(line)
      read(line,*,end=1730,err=1730)(tubdis(tub,i),i=1,3)
      goto 1750
 1730 line='*** Invalid input.  Enter three numbers: 0,0,0'
      goto 1720
 1750 continue

c     read degrees angular rotation
      line='Enter degrees angular rotation aroud the z-axis: 0'
 1820 call in(line)
      read(line,*,end=1830,err=1830)tubang(tub)
      goto 1850
 1830 line='*** Invalid input.  Enter a number: 0'
      goto 1820
 1850 tubang(tub)=tubang(tub)*pi/180.

c     go prompt for next nanotube
      goto 1000


c     Find the period counts and total length for the collection of nanotubes

 5000 if(tubmax.le.0)goto 9000
      call out(' ')
      call out(
     & '==============================================================')
      call out('Finalizing the tube(s)')

c     for a single tube, simply ask for the number of periods
      if(tubmax.le.1)then
         write(line,5010)tubat2(1)-tubat1(1)+1
 5010    format('Atoms per period:',i5)
         call out(line(1:22))
c        no multiple images in an interaction region of radius 3
         itmp=min(3.*2./tubp(1)+1.,9.)
         line='Enter number of axial periods: '//char(itmp+48)
 5020    call in(line)
         read(line,*,end=5030,err=5030)tubpct(1)
         goto 5050
 5030    line='*** Invalid input.  Enter a natural number: '//
     &    char(itmp+48)
         goto 5020
 5050    if(tubpct(1).le.0)goto 5030
         totlt=tubpct(1)*tubp(1)
         ltcom=.true.
         goto 8000
      endif

c     see whether the tubes are length-compatible

c     check for obvious incompatibility
      if(tubsrc(1).ne.1)goto 6000
      do 5100 tub=2,tubmax
         if(tubsrc(tub).ne.1)goto 6000
         if(tubpf(tub).ne.tubpf(tub-1))goto 6000
 5100 continue

c     start by assuming a single period of the first tube
      tubpct(1)=1

c     see whether the other tubes are compatible with it
      do 5200 tub=2,tubmax
         if(.not.compat(
     &    pridim,prival,
     &    tubcn(1),tubcm(1),tubcn(tub),tubcm(tub),
     &    fac1,fac2,
     &    mult1,mult2))goto 6000
c        modify number of periods for all tubes so far to be compatible
         itmp=comdiv(pridim,prival,tubpct(1),mult1,fac1,fac2)
         tubpct(tub)=mult2*(tubpct(1)/itmp)
         mult1=mult1/itmp
         do 5150 tub2=1,tub-1
            tubpct(tub2)=tubpct(tub2)*mult1
 5150    continue
 5200 continue

c     find number of atoms in a global period
      totat=0
      do 5210 tub=1,tubmax
         totat=totat+(tubat2(tub)-tubat1(tub)+1)*tubpct(tub)
 5210 continue
      write(line,5220)totat
 5220 format('Tubes are length-compatible; atoms per global period:',i5)
      call out(line(1:58))

c     ask whether to use this period, or an approximate one instead
      line='Do you want to use this compatible period? (y/n): y'
 5230 call in(line)
      if(line.eq.'n')then
         goto 6010
      else if(line.ne.'y')then
         line='*** Invalid input; enter y or n: y'
         goto 5230
      endif

c     set remaining info
      itmp=min(3.*2./(tubp(1)*tubpct(1))+1.,9.)
      line='Enter number of global periods: '//char(itmp+48)
 5320 call in(line)
      read(line,*,end=5330,err=5330)globct
      goto 5350
 5330 line='*** Invalid input.  Enter a natural number: '//char(itmp+48)
      goto 5320
 5350 if(globct.le.0)goto 5330
      do 5370 tub=1,tubmax
         tubpct(tub)=tubpct(tub)*globct
 5370 continue
      totlt=tubpct(1)*tubp(1)
      ltcom=.true.
      goto 8000

c     handle total length for tubes that are not length compatible

 6000 call out('Tubes are not length compatible')
 6010 ltcom=.false.

c     initialize all period counts to 1 and initialize total length
      totlt=0.
      do 6050 tub=1,tubmax
         tubpct(tub)=1
         totlt=max(totlt,tubp(tub))
 6050 continue

c     ask whether we want to equalize lengths
      if(totlt.ge.99999.99)goto 6115
      line='Try to minimize length differences? (y/n): y'
 6110 call in(line)
      if(line.eq.'y')goto 6190
      if(line.ne.'n')then
         line='*** Invalid answer.  Answer y or n: y'
         goto 6110
      endif
 6115 if(tubmax.gt.99)goto 8000
      do 6160 tub=1,tubmax
         line='Enter number of periods to use of tube  : 1'
         write(line(39:40),6120)tub
 6120    format(i2)
 6130    call in(line)
         read(line,*,err=6140,end=6140)tubpct(tub)
         goto 6150
 6140    line=
     &    '*** Invalid number of periods.  Enter a natural number: 1'
         goto 6130
 6150    if(tubpct(tub).lt.1)goto 6140
         totlt=max(totlt,tubpct(tub)*tubp(tub))
 6160 continue
      goto 8000
 6190 continue

c     take the reference tube as the longest one
      call gtstrn(tubdim,tubmax,tubr,tubp,tubpct,
     & tublo,tubref,totlt,strain)
      write(line,6195)tubp(tubref)
 6195 format('Reference tube single period length:',f10.2)
      call out(line(1:46))

c     initialize the period range
      perct1=1
      perct2=2

 6200 line='Enter range of periods to explore; equal ends: '
      write(line(48:),*)perct1,',',perct2
 6220 call in(line)
      read(line,*,end=6230,err=6230)itmp,itmp2
      goto 6250
 6230 line=
     & '*** Invalid input.  Enter n1,n2 with 0 < n1 <= n2: 1,2'
      goto 6220
 6250 if(itmp2.lt.itmp .or. itmp.lt.1)goto 6230
      perct1=itmp
      perct2=itmp2

c     explore the range

      do 6500 perct=perct1,perct2

c        initialize period counts to closest match
         do 6300 tub=1,tubmax
            tubpct(tub)=nint(perct*tubp(tubref)/tubp(tub))
 6300    continue

c        find strain
         call gtstrn(tubdim,tubmax,tubr,tubp,tubpct,
     &    tublo,tubhi,totlt,strain)

c        iterate to try to reduce the strain
 6400    change=0

c        try to reduce the strain by making the shortest tube longer
         if(tublo.eq.tubref)goto 6450
         tub=tublo
         tubpct(tub)=tubpct(tub)+1
         tmp=strain
         call gtstrn(tubdim,tubmax,tubr,tubp,tubpct,
     &    tublo,tubhi,totlt,strain)
         if(tmp.le.strain)then
            tubpct(tub)=tubpct(tub)-1
            call gtstrn(tubdim,tubmax,tubr,tubp,tubpct,
     &       tublo,tubhi,totlt,strain)
         else
            change=1
         endif

c        try to reduce the strain by making the longest tube shorter
 6450    if(tubhi.eq.tubref)goto 6490
         if(tubpct(tubhi).eq.1)goto 6490
         tub=tubhi
         tubpct(tub)=tubpct(tub)-1
         tmp=strain
         call gtstrn(tubdim,tubmax,tubr,tubp,tubpct,
     &    tublo,tubhi,totlt,strain)
         if(tmp.le.strain)then
            tubpct(tub)=tubpct(tub)+1
            call gtstrn(tubdim,tubmax,tubr,tubp,tubpct,
     &       tublo,tubhi,totlt,strain)
         else
            change=1
         endif

 6490    if(change.ne.0)goto 6400
         write(line,6491)perct,totlt,strain
 6491    format('periods:',i4,'   length:',f10.2,
     &    '   maximum strain:',f7.3)
         call out(line(1:57))

 6500 continue

c     explore another range
      if(perct1.ne.perct2)goto 6200

c     write the data of the tubes to coord.d

 8000 if(tubmax.le.0)goto 9000

c     see whether we want to correct to the same length
      ltcorr=.false.
      if(.not.ltcom)then
         line='Correct tube lengths to periodix box size? (y/n): y'
 8020    call in(line)
         if(line.eq.'y')then
            call out('tubes will be corrected to the axial box size')
            ltcorr=.true.
         else if(line.eq.'n')then
            totlt=0.
            do 8040 tub=1,tubmax
               if(tubp(tub).lt.99999.99)
     &          totlt=max(totlt,tubpct(tub)*tubp(tub))
 8040       continue
            if(totlt.eq.0.)totlt=tubpct(1)*tubp(1)
         else
            line='*** Invalid input.  Enter y or n: y'
            goto 8020
         endif
      endif

c     find the minimum and maximum tube radii
      tublo=1
      tubhi=1
      do 8100 tub=2,tubmax
         if(tubr(tub).lt.tubr(tublo))tublo=tub
         if(tubr(tub).gt.tubr(tubhi))tubhi=tub
 8100 continue

c     create the file header
      write(head,8110)tubr(tublo),tubr(tubhi)
 8110 format('Smallest and largest radii:',2f7.2)
      headlt=41

c     ask for the box size
      line='Enter the axial periodic box size:'
      write(line(36:),*)totlt
 8160 call in(line)
      read(line,*,err=8170,end=8170)cube(3)
      goto 8180
 8170 line='*** Enter a positive number:'
      write(line(30:),*)totlt
      goto 8160
 8180 if(cube(3).le.0.)goto 8170
      cube(1)=1.e20
      cube(2)=1.e20

c     find number of atoms
      kmax=0
      do 8200 tub=1,tubmax
         kmax=kmax+(tubat2(tub)-tubat1(tub)+1)*tubpct(tub)
 8200 continue
      write(line,8210)kmax
 8210 format('Total number of atoms:',i5)
      call out(line(1:27))
      if(kmax.gt.kdim)
     & call out('*** too many atoms in the periodic box!')

c     set the trivial coord.d data
      do 8220 k=1,kmax
         an(k)=6
         itr(k)=1
         do 8215 i=1,3
            r1n(k,i)=0.
            r2(k,i)=0.
            r3(k,i)=0.
            r4(k,i)=0.
 8215    continue
 8220 continue

c     find the atom positions
      k=0
      do 8300 tub=1,tubmax
         ltfac=1.
         if(ltcorr)ltfac=cube(3)/(tubp(tub)*tubpct(tub))
         write(line,8230)tub,tubpct(tub),ltfac-1.
 8230    format('Tube:',i4,'    periods:',i4,'    strain:',f8.4)
         call out(line(1:44))

         if(tubsrc(tub).eq.1)then

c           process computed tubes
            c2=dot(3*tubcn(tub),3*tubcm(tub),3*tubcn(tub),3*tubcm(tub))
            t2=dot(3*tubtn(tub),3*tubtm(tub),3*tubtn(tub),3*tubtm(tub))
            do 8250 perct=1,tubpct(tub)
               do 8240 at=tubat1(tub),tubat2(tub)
                  k=k+1
                  r0(k,3)=tubdis(tub,3)
     &             +(perct-1)*tubp(tub)*ltfac
     &             +att(at)*tubp(tub)/t2*ltfac
     &             -0.5*tubpct(tub)*tubp(tub)*ltfac
                  r0(k,1)=tubdis(tub,1)+
     &             tubr(tub)*cos(atc(at)*2.*pi/c2+tubang(tub))
                  r0(k,2)=tubdis(tub,2)+
     &             tubr(tub)*sin(atc(at)*2.*pi/c2+tubang(tub))
 8240          continue
 8250       continue

         else

c           process read-in tubes
 8260       do 8290 perct=1,tubpct(tub)
               do 8280 at=tubat1(tub),tubat2(tub)
                  k=k+1
                  r0(k,3)=tubdis(tub,3)
     &             +(perct-1)*tubp(tub)*ltfac
     &             +atz(at)*tubpf(tub)*ltfac
     &             -0.5*(tubpct(tub)-1)*tubp(tub)
                  r0(k,1)=tubdis(tub,1)
     &             +atx(at)*tubrf(tub)*cos(tubang(tub))
     &             -aty(at)*tubrf(tub)*sin(tubang(tub))
                  r0(k,2)=tubdis(tub,2)
     &             +atx(at)*tubrf(tub)*sin(tubang(tub))
     &             +aty(at)*tubrf(tub)*cos(tubang(tub))
                  an(k)=atano(at)
                  itr(k)=atitr(at)
 8280          continue
 8290       continue

         endif
 8300 continue

c     set the velocities consistent with the temperature
      if(temp.ne.0.)call sett(kdim,kmax,temp,an,r1n,r3,r4)

c     create coord.d
      havran=0
      havmat=0
      call coordw(kdim,
     & 'coord.d',
     & head(1:headlt),
     & kmax,0,0,0,
     & 0.,delta,
     & cube,
     & an,itr,r0,r1n,r2,r3,r4,
     & havran,randat,havmat,mat)

      call exit(0)
 9000 call exit(1)
      end
