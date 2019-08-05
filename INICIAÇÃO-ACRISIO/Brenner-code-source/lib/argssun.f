      subroutine args(argdim,argmx,argt,argtl)

      implicit none

c     Get the arguments from the command line.

c     This version for Sun or g77.

c     Trailing whitespace is eliminated.  Empty
c     strings return as a single blank.

c input

c     declared number of arguments
      integer argdim

c output

c     number of arguments (set to -1 if there are too many arguments)
      integer argmx

c     arguments
      character*80 argt(argdim)
      integer argtl(argdim)

c local

c     argument index
      integer arg

c     character index
      integer i

c     returns command line parameters
      integer iargc
      external iargc,getarg

c     get number of arguments
      argmx=iargc()
      if(argmx.gt.argdim)argmx=-1
      if(argmx.le.0)return

c     get the arguments and eliminate trailing whitespace
      do 100 arg=1,argmx
         argt(arg)=' '
         call getarg(arg,argt(arg))
         do 10 i=80,1,-1
            argtl(arg)=i
            if(argt(arg)(i:i).gt.' ' .and. argt(arg)(i:i).le.'~')
     &       goto 100
 10      continue
         argt(arg)(1:1)=' '
 100  continue

      return
      end
