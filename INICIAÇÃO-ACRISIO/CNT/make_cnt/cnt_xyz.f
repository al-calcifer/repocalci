c
c   Produce Cartesian coordinates of the isolated tube.
c   Modified for MLC and SGL-group people; please let me know 
c   if you give this program to other people.    
c
c                                Susumu Saito   September 13, 2003
c
c
c   Input: (n,m)
c
c---*----1----*----2----*----3----*----4----*----5----*----6----*----7
c
      implicit real*8(a-h,o-z)
      parameter(nlatp=1000,i1p=40)
      parameter(natp=nlatp*2*i1p)
      dimension xlat(nlatp), ylat(nlatp), xat(natp), yat(natp)
      dimension x3d(natp), y3d(natp), z3d(natp)
      integer Ncells
c
      eps = 1.d-7
c
      open(26,file='cnt_xyz.dat')
      open(16,file='cntxyzDETAILED.txt')
c
c In-plane lattice constant of the graphite (A) (Iwanami, Rikagaku-jiten)
      alat = 2.456d0
c
      write(6,*)' (n,m) ?'
      read(5,*) n,m
      write(6,*)' Ncells=?'
      read(5,*) Ncells
c
      max_nm = max( iabs(n) , iabs(m) )
      do 11 j = 1, max_nm
        if( (n/j)*j .eq. n   .and.   (m/j)*j .eq. m ) then
          n1 = n/j
          m1 = m/j
          i1 = j
        endif
   11 continue
c
      nz =      n1 + 2*m1
      mz = (-2)*n1 -   m1
c
      max_nzmz = max( iabs(nz), iabs(mz) )
      do 12 j = 1, max_nzmz
        if( (nz/j)*j .eq. nz   .and.   (mz/j)*j .eq. mz ) then
          n2 = nz / j
          m2 = mz / j
          i2 = j
        endif
   12 continue
c
      write(16,*) ' (n,m)      = (', n, ',', m, ') = '
      write(16,*) ' (n1,m1)*i1 = (', n1, ',', m1, ')*', i1
      write(16,*)
      write(16,*) ' (n2,m2)*i2 = (', n2, ',', m2, ')*', i2
c
ccc   write(6,*) ' (n,m)      = (', n, ',', m, ') = '
ccc   write(6,*) ' (n1,m1)*i1 = (', n1, ',', m1, ')*', i1
ccc   write(6,*)
ccc   write(6,*) ' (n2,m2)*i2 = (', n2, ',', m2, ')*', i2
c
      if( i1 .gt. i1p ) then
        write(16,*) ' *** i1 > i1p *** '
        write(6,*)  ' *** i1 > i1p *** '
        stop
      endif
c
c Now generates lattice points and C-atom coordinates on a flat plane
c
      if( n1+n2 .lt. 2 ) then
        write(6,*)' n1+n2 = ', n1+n2, ' < 2; check initial (n,m)'
        stop
      endif
      if( m1 .lt. m2 ) then
        write(6,*)' m1, m2 = ', m1, m2, ' check initial (n,m)'
        stop
      endif
c
      ax = dsqrt( 3.d0 ) * alat / 2.d0
      ay =                 alat / 2.d0
      bx = dsqrt( 3.d0 ) * alat / 2.d0
      by =               - alat / 2.d0
c
      r1x = dble( n1 )*ax + dble( m1 )*bx
      r1y = dble( n1 )*ay + dble( m1 )*by
      r1  = dsqrt( r1x*r1x + r1y*r1y )
c
      r2x = dble( n2 )*ax + dble( m2 )*bx
      r2y = dble( n2 )*ay + dble( m2 )*by
      r2  = dsqrt( r2x*r2x + r2y*r2y )
c
      r1r2 = r1x*r2x + r1y*r2y
c
      write(16,*)
      write(16,*) ' r1*r2 (inner product) = ', r1r2
      write(16,*) ' r1, r2 = ', r1, r2
ccc   write(6,*)
ccc   write(6,*) ' r1*r2 (inner product) = ', r1r2
ccc   write(6,*) ' r1, r2 = ', r1, r2
      if( r1r2 .gt. eps ) stop ' r1r2 too large'
c
      costh = dsqrt(3.d0)*dble(n1+m1) / 
     &        ( 2.d0*dsqrt( dble( n1*n1 + n1*m1 + m1*m1 ) ) ) 
      sinth = dble( n1 - m1 ) / 
     &        ( 2.d0*dsqrt( dble( n1*n1 + n1*m1 + m1*m1 ) ) ) 
      one = costh*costh + sinth*sinth
      write(16,*)
      write(16,*) ' cos**2 + sin**2 = ', one
ccc   write(6,*)
ccc   write(6,*) ' cos**2 + sin**2 = ', one
c
      nlat = 0
      nat  = 0
      do 21 i = 1, n1+n2-1
      do 22 j = m2, m1-1 
        x =   dsqrt(3.d0)*dble(i+j)*costh + dble(i-j)*sinth
        y = - dsqrt(3.d0)*dble(i+j)*sinth + dble(i-j)*costh
        x = x * alat / 2.d0
        y = y * alat / 2.d0
        if( x .gt.   -eps      .and.
     &      x .lt. r1+eps      .and.
     &      y .gt.   -eps      .and.
     &      y .lt. r2+eps           )    then
          nlat = nlat + 1
          nat  = nat  + 2
          xlat( nlat ) = x
          ylat( nlat ) = y
          xat( nat-1 ) = x +      costh*alat / dsqrt(3.d0)
          yat( nat-1 ) = y -      sinth*alat / dsqrt(3.d0)
          xat( nat )   = x + 2.d0*costh*alat / dsqrt(3.d0)
          yat( nat )   = y - 2.d0*sinth*alat / dsqrt(3.d0)
        endif
   22 continue
   21 continue
      write(16,*) ' ---- irreducible zone -------- '
      write(16,*) ' # lattice points generated = ', nlat
      write(16,*) ' # lattice points expected  = ', 
     &            2.d0*r1*r2 / ( dsqrt(3.d0)*alat*alat )
      write(16,*)
      write(16,*) ' # atoms generated ' , nat
ccc   write(6,*) ' ---- irreducible cell -------- '
ccc   write(6,*) ' # lattice points generated = ', nlat
ccc   write(6,*) ' # lattice points expected  = ', 
ccc  &           2.d0*r1*r2 / ( dsqrt(3.d0)*alat*alat )
ccc   write(6,*)
ccc   write(6,*) ' # atoms generated ' , nat
c
c Produce atom coordinates in a whole unit cell
c
      if( i1 .gt. 1 ) then
        do 31 i = 2, i1
        do 32 j = 1, nat
          iat = j + nat*( i - 1 )
          xat(iat) = xat(j) + dble( i - 1 )*r1
          yat(iat) = yat(j)
   32   continue
   31   continue
        natom = iat
      else
        natom = nat
      endif  
c
      write(16,*)
      write(16,*) ' Total number of atoms in a unit cell = ', natom
      write(16,*) ' Total number of atoms =',natom*Ncells
      write(6,*)
      write(6,*) ' Total number of atoms in a unit cell = ', natom
      write(6,*) ' Total number of atoms=', natom*Ncells
c
      write(16,1601) ( xat(i), yat(i), i = 1, natom )
 1601 format(2d20.10)
c
c Conversion from 2d (plane) to 3d (tube)
c
      pi = 4.d0 * datan(1.d0)
      radius = r1 * dble( i1 ) / ( 2.d0 * pi )
      do 30 i = 1, natom
        phi = xat(i) / radius
        x3d(i) = radius * dcos( phi )
        y3d(i) = radius * dsin( phi )
        z3d(i) = yat(i)
   30 continue
c
c #26 for tpatom.i4;  #36 for tbprp.i5
c #26 modified for general use
c
      write(26,*) '(n,m)=', n, m
      write(26,*) 'clat=', r2, 'ncells=', Ncells 
ccc   write(36,*) ' (n,m) = ', n, m
ccc   write(36,*) ' clat = ', r2 
      
      write(26,*) natom*Ncells
      write(26,*) ''
ccc   write(36,*) natom
      Do j=0,Ncells-1
       Do i=1,natom
         write(26,*) 'C',x3d(i), y3d(i), z3d(i)+j*r2
       Enddo
      Enddo

ccc   write(26,2601) ( x3d(i), y3d(i), z3d(i), i = 1, natom )
ccc 2601 format(3d20.10)
ccc 3601 format('  1 2 3 4 ', 3d20.10)
c
      write(6,*)
      write(6,*) ' Normal end. '
      write(6,*) ' Tube length = ', r2*Ncells, 'Diameter = ',2.d0*radius
      write(6,*) ' Output file=cnt_xyz.dat '
      write(6,*) ' Details of calculation file=cntxyzDETAILED.txt '
c
c---*----1----*----2----*----3----*----4----*----5----*----6----*----7
      stop
      end
