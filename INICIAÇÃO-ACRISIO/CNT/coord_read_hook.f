      subroutine coord_read_hook

c     Additional stuff to be done at the end of reading in file coord.d.

      implicit none
      include 'common_files.inc'

c local

c     two real*8 values equivalenced to 4 integers, in order to allow
c     those two real*8 values to be read and written *exactly*
      real*8 long1,long2
      integer*4 words(4)
      equivalence (long1,words(1)),(long2,words(3))

c executable

c     try reading the random number generator from coord.d
      read(11,*,end=20)words
c     copy their values over to storage
      qb10=long1
      qb20=long2
      print*,'Random number generator read'
c     no generator in file
 20   qb10=0.d0
      qb20=0.d0

      return
      end

