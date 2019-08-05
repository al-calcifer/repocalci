      subroutine args(argdim,argmx,argt,argtl)

      implicit none

c     Get the arguments from the command line.

c     Generic version which simply ignores arguments.

c input

c     declared number of arguments
      integer argdim

c output

c     number of arguments (set to -1 if there are too many arguments)
      integer argmx

c     arguments
      character*80 argt(argdim)
      integer argtl(argdim)

c     cannot read any arguments
      argmx=0

      return
      end
