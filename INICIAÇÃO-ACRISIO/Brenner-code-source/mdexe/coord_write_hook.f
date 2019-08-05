      subroutine coord_write_hook

c     Additional stuff to be done at the end of writing coord.d

      implicit none
      include 'common_files.inc'

c local

c     two real*8 values equivalenced to 4 integers, in order to allow
c     those two real*8 values to be read and written *exactly*
      real*8 long1,long2
      integer*8 words(4)
      equivalence (long1,words(1)),(long2,words(3))

c executable

c     store the random number generator qb1, qb2 in long1, long2
      long1=qb1
      long2=qb2
c     write out array words, equivalent to long1 and long2
      write(11,10)words
 10   format(4i20)

      return
      end
