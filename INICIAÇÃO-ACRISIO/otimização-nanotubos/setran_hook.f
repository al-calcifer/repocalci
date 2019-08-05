      subroutine setran_hook

c     set the random number generator to exactly what it was.

      implicit none
      include 'common_files.inc'

      if(qb10.ne.0.d0 .or. qb20.ne.0.d0)then
         qb1=qb10
         qb2=qb20
      endif

      return
      end

