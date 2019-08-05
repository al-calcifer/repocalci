      subroutine exit(status)

c     Use this subroutine if your compiler has no build in one.

      implicit none

c input

c     exit status, zero for successful completion
      integer exit

c executable

c     for nonzero status, create a program crash
      if(status.ne.0)print*,1/(status/1234567890)

      stop
      end
