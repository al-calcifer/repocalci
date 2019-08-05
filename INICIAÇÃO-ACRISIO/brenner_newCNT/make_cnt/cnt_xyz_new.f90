!!
!   Produce Cartesian coordinates of the isolated tube.
!   Modified for MLC and SGL-group people; please let me know 
!   if you give this program to other people.    
!!
!                                Susumu Saito   September 13, 2003
!!
!!
!   Input: (n,m)
!!
!---*----1----*----2----*----3----*----4----*----5----*----6----*----7
!!




      implicit real*8(a-h,o-z)
! 	  parameter( dhc=1.31, dcc=1.42)
      parameter(nlatp=1000,i1p=40)
      parameter(natp=nlatp*2*i1p)
      dimension xlat(nlatp), ylat(nlatp), xat(natp), yat(natp)
      dimension x3d(natp), y3d(natp), z3d(natp)
	  real(kind=8) :: latt1(3)
	  real(kind=8) :: carbonaxis1,carbonaxis2,carbonaxis3
	  real(kind=8) :: x, y, z, dhc, dcc

      integer Ncells, ic

	  data latt1/0.00, 0.00, 1.42/

! , xl(nac), yl(nac), zl(nac)

!!
      eps = 1.d-7
	dhc= 1.31
	dcc= 1.42

!!
	  open(11,file='cnt_xyz_new.xyz')
      open(26,file='cnt_xyz_new.dat')
      open(16,file='cntxyzDETAILED.txt')
!!
! In-plane lattice constant of the graphite (A) (Iwanami, Rikagaku-jiten)
      alat = 2.456d0
!!
      write(6,*)' (n,m) ?'
      read(5,*) n,m
      write(6,*)' Ncells=?'
      read(5,*) Ncells
	  write(6,*) 'numero de atomos de carbono na cadeia:'
	  read(5,*) ncc

nc=ncc-1

!!
      max_nm = max( iabs(n) , iabs(m) )
      do 11 j = 1, max_nm
        if( (n/j)*j .eq. n   .and.   (m/j)*j .eq. m ) then
          n1 = n/j
          m1 = m/j
          i1 = j
        endif
   11 continue
!!
      nz =      n1 + 2*m1
      mz = (-2)*n1 -   m1
!!
      max_nzmz = max( iabs(nz), iabs(mz) )
      do 12 j = 1, max_nzmz
        if( (nz/j)*j .eq. nz   .and.   (mz/j)*j .eq. mz ) then
          n2 = nz / j
          m2 = mz / j
          i2 = j
        endif
   12 continue
!!
      write(16,*) ' (n,m)      = (', n, ',', m, ') = '
      write(16,*) ' (n1,m1)*i1 = (', n1, ',', m1, ')*', i1
      write(16,*)
      write(16,*) ' (n2,m2)*i2 = (', n2, ',', m2, ')*', i2
!!
!!!   write(6,*) ' (n,m)      = (', n, ',', m, ') = '
!!!   write(6,*) ' (n1,m1)*i1 = (', n1, ',', m1, ')*', i1
!!!   write(6,*)
!!!   write(6,*) ' (n2,m2)*i2 = (', n2, ',', m2, ')*', i2
!!
      if( i1 .gt. i1p ) then
        write(16,*) ' *** i1 > i1p *** '
        write(6,*)  ' *** i1 > i1p *** '
        stop
      endif
!!
! Now generates lattice points and C-atom coordinates on a flat plane
!!
      if( n1+n2 .lt. 2 ) then
        write(6,*)' n1+n2 = ', n1+n2, ' < 2; check initial (n,m)'
        stop
      endif
      if( m1 .lt. m2 ) then
        write(6,*)' m1, m2 = ', m1, m2, ' check initial (n,m)'
        stop
      endif
!!
      ax = dsqrt( 3.d0 ) * alat / 2.d0
      ay =                 alat / 2.d0
      bx = dsqrt( 3.d0 ) * alat / 2.d0
      by =               - alat / 2.d0
!!
      r1x = dble( n1 )*ax + dble( m1 )*bx
      r1y = dble( n1 )*ay + dble( m1 )*by
      r1  = dsqrt( r1x*r1x + r1y*r1y )
!!
      r2x = dble( n2 )*ax + dble( m2 )*bx
      r2y = dble( n2 )*ay + dble( m2 )*by
      r2  = dsqrt( r2x*r2x + r2y*r2y )
!!
      r1r2 = r1x*r2x + r1y*r2y
!!
      write(16,*)
      write(16,*) ' r1*r2 (inner product) = ', r1r2
      write(16,*) ' r1, r2 = ', r1, r2
!!!   write(6,*)
!!!   write(6,*) ' r1*r2 (inner product) = ', r1r2
!!!   write(6,*) ' r1, r2 = ', r1, r2
      if( r1r2 .gt. eps ) stop ' r1r2 too large'
!!
      costh = dsqrt(3.d0)*dble(n1+m1) / ( 2.d0*dsqrt( dble( n1*n1 + n1*m1 + m1*m1 ) ) ) 
              
      sinth = dble( n1 - m1 ) / ( 2.d0*dsqrt( dble( n1*n1 + n1*m1 + m1*m1 ) ) ) 
              
      one = costh*costh + sinth*sinth
      write(16,*)
      write(16,*) ' cos**2 + sin**2 = ', one
!!!   write(6,*)
!!!   write(6,*) ' cos**2 + sin**2 = ', one
!!
      nlat = 0
      nat  = 0
      do 21 i = 1, n1+n2-1
      do 22 j = m2, m1-1 
        x =   dsqrt(3.d0)*dble(i+j)*costh + dble(i-j)*sinth
        y = - dsqrt(3.d0)*dble(i+j)*sinth + dble(i-j)*costh
        x = x * alat / 2.d0
        y = y * alat / 2.d0
        if( x .gt.   -eps   .and.  x .lt. r1+eps   .and.  y .gt.   -eps   .and.  y .lt. r2+eps  )    then
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
      write(16,*) ' # lattice points expected  = ', 2.d0*r1*r2 / ( dsqrt(3.d0)*alat*alat )                
      write(16,*)
      write(16,*) ' # atoms generated ' , nat
!!!   write(6,*) ' ---- irreducible cell -------- '
!!!   write(6,*) ' # lattice points generated = ', nlat
!!!   write(6,*) ' # lattice points expected  = ', 
!!!  &           2.d0*r1*r2 / ( dsqrt(3.d0)*alat*alat )
!!!   write(6,*)
!!!   write(6,*) ' # atoms generated ' , nat
!!
! Produce atom coordinates in a whole unit cell
!!
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
!!
      write(16,*)
      write(16,*) ' Total number of atoms in a unit cell = ', natom
      write(16,*) ' Total number of atoms =',natom*Ncells
      write(6,*)
      write(6,*) ' Total number of atoms in a unit cell = ', natom
      write(6,*) ' Total number of atoms=', natom*Ncells
!!
      write(16,1601) ( xat(i), yat(i), i = 1, natom )
 1601 format(2d20.10)
!!
! Conversion from 2d (plane) to 3d (tube)
!!
      pi = 4.d0 * datan(1.d0)
      radius = r1 * dble( i1 ) / ( 2.d0 * pi )

  	write(26,*) 'H', latt1(1), latt1(2), (r2*Ncells/3)
  	write(11,*) 'H', latt1(1), latt1(2), (r2*Ncells/3)
	write(*,*) 'H', latt1(1), latt1(2), (r2*Ncells/3)
	
	do k=0, nc
	carbonaxis1=latt1(1)*k	
	carbonaxis2=latt1(2)*k
	carbonaxis3=latt1(3)*k	

ptnc=1.31+carbonaxis3
ptnh=2*dhc+(nc*dcc)
	
      do 30 i = 1, natom
        phi = xat(i) / radius
        x3d(i) = radius * dcos( phi )
        y3d(i) = radius * dsin( phi )
        z3d(i) = yat(i)
   30 continue
!!
! #26 for tpatom.i4;  #36 for tbprp.i5
! #26 modified for general use
!!
      write(26,*) '(n,m)=', n, m
      write(26,*) 'clat=', r2, 'ncells=', Ncells 
!!!   write(36,*) ' (n,m) = ', n, m
!!!   write(36,*) ' clat = ', r2 

      write(26,*) natom*Ncells
      write(26,*) ''
!!!   write(36,*) natom
      Do j=0,Ncells-1
       Do i=1,natom
         write(26,*) 'C',x3d(i), y3d(i), z3d(i)+j*r2
		 write(11,*) 'C',x3d(i), y3d(i), z3d(i)+j*r2
       Enddo
      Enddo


 	write(26,*) 'C', carbonaxis1, carbonaxis2, (r2*Ncells/3)+ptnc
	write(11,*) 'C', carbonaxis1, carbonaxis2, (r2*Ncells/3)+ptnc
	write(*,*) 'C', carbonaxis1, carbonaxis2, (r2*Ncells/3)+ptnc

	end do


	write(26,*) 'H', carbonaxis1, carbonaxis2, (r2*Ncells/3)+ptnh
	write(11,*) 'H', carbonaxis1, carbonaxis2, (r2*Ncells/3)+ptnh
	write(*,*) 'H', carbonaxis1, carbonaxis2, (r2*Ncells/3)+ptnh



!!!   write(26,2601) ( x3d(i), y3d(i), z3d(i), i = 1, natom )
!!! 2601 format(3d20.10)
!!! 3601 format('  1 2 3 4 ', 3d20.10)
!!
      write(6,*)
      write(6,*) ' Normal end. '
      write(6,*) ' Tube length = ', r2*Ncells, 'Diameter = ',2.d0*radius
      write(6,*) ' Output file=cnt_xyz.dat '
      write(6,*) ' Details of calculation file=cntxyzDETAILED.txt '

		write(6,*) r2*Ncells-(r2*Ncells/3), r2*Ncells-((r2*Ncells/3)+ptnh)
!!
!---*----1----*----2----*----3----*----4----*----5----*----6----*----7
      stop
      end
