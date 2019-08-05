      subroutine outcon(nocr,bell,string)

c     Use this generic version of outcon if no better option is available.

c     Write a string to the screen.
c     FAILS to suppress the terminal cr-lf if nocr is nonzero.
c     If bell is nonzero, will ring the bell.

      implicit none

c input

c     nonzero to suppress the cr-lf
      integer nocr

c     nonzero to write a bell
      integer bell

c     string to write
      character*(*) string

c formats

c     If the compiler does not follow Fortan carriage control, remove ' ':
   10 format(' ',a)
   30 format(' ',a1,a)

c executable

      if(bell.eq.0)print10,string
      if(bell.ne.0)print30,char(7),string

      return
      end


      subroutine incon(line)

c     Use this generic version of incon if no better option is available.
c     It DOES NOT allow line editing.

c     Read a line from the keyboard.

      implicit none

c input/output

c     input: Prompt to use.  Must be terminated by a colon.
c            Any default must follow the colon and a space
c     output: line read
      character*80 line

c local

c     kept copy of the input line
      character*80 linsav

c     position of the colon, and its original value
      integer icol,icol0

c     length of line
      integer imx

c     character index
      integer i

c executable

c     copy line
      linsav=line

c     find colon
      icol=index(linsav,':')
      icol0=icol
      if(icol.le.0)then
	 call outcon(0,1,'*** INCON: program error: missing colon')
	 call errxit
      endif
      if(icol.gt.77)then
	 call outcon(0,1,'*** INCON: program error: prompt too long')
	 call errxit
      endif
      if(linsav(icol+1:icol+1).ne.' ')then
	 call outcon(0,1,'*** INCON: program error: '//
     &	  'prompt not followed by a blank space')
	 call errxit
      endif

c     search for default
      imx=79
      do 10 i=79,icol+2,-1
	 if(linsav(i:i).ne.' ')goto 11
	 imx=i-1
   10 continue
   11 continue

c     add default to prompt
      if(imx.le.icol+1)goto 200
      if(imx+4.gt.79)then
	 call outcon(0,1,'*** prompt too long')
	 call errxit
      endif
      line(icol:imx+3)=' ['//linsav(icol+2:imx)//']: '
      icol=imx+2

c     print the line
  200 call outcon(1,0,line(1:icol+1))

c     read keyboard
  300 read(*,310,end=320,err=330)line
  310 format(a80)
      goto 400
  320 call outcon(0,1,'*** INICON: lost keyboard')
      call errxit
  330 call outcon(0,1,'*** error during read.  Please re-enter: ')
      goto 300

c     substitute default on blank
  400 if(line.eq.' ' .and. imx.ge.icol0+2)line=linsav(icol0+2:imx)

      return
      end


      subroutine idle(secs)

c     Use this generic version of idle if no better option is available.
c     It DOES NOT really wait to allow the user to read the screen.

c     Wait a number of seconds.

c input

c     number of seconds to wait
      real secs

c local

      if(secs.lt.0.)then
	 call outcon(0,1,'*** IDLE: invalid time')
	 call errxit
      endif

      return
      end
