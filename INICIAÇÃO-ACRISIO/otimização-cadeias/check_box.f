      subroutine check_box

c     check the periodic box size

      implicit none
      include 'common_files.inc'

c local

c     largest interaction distance that is active
      real*8 rintmx

c     atom indices
      integer i,j

c     type of atom
      integer ki,kj

c executable

c     find largest active interaction distance
      rintmx=0.
      do 200 i=1,np
         ki=ktype(i)
         do 100 j=i,np
            kj=ktype(j)
            if(ki.le.4 .and. kj.le.4)rintmx=max(rintmx,rlist(ki,kj))
            if(ki.le.ktmax .and. kj.le.ktmax)
     &       rintmx=max(rintmx,rslj(ki,kj))
 100     continue
 200  continue
      rintmx=sqrt(rintmx)

c     check box size
      if(
     & cube(1)*.5.lt.rintmx .or.
     & cube(2)*.5.lt.rintmx .or.
     & cube(3)*.5.lt.rintmx)then
         write(*,*)'*** CHECK_BOX: Periodic box size is too small'
         stop
      endif

      return
      end
