      subroutine outcon(nocr,bell,string)

c     This version for Unix GNU with a noncanonical window.

c     Write a string to the screen.
c     If nocr is nonzero, suppress the crlf at the end.
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

   10 format(a)
   20 format(a,$)

c executable

      if(bell.ne.0)print20,char(7)

      if(nocr.eq.0)then
	 print10,string
      else
	 print20,string
      endif

      return
      end


      subroutine incon(line)

c     This version for Unix GNU with a noncanonical window (run
c     the executable within con_shell.)

c     Read a line from the keyboard with editing.

      implicit none

c input/output

c     input: Prompt to use.  Must be terminated by a colon.
c            Any default must follow the colon and a space
c     output: line read
      character*80 line

c local

c     buffer of saved previous inputs
      integer bufdim
      parameter (bufdim=10)
      character*80 buffer(bufdim)
      save buffer

c     kept copy of the input line
      character*80 linsav

c     position of the colon
      integer icol

c     length of the user input
      integer lt

c executable

c     copy line
      linsav=line

c     find colon
      icol=index(linsav,':')
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

c     get the user's input with editing

  100 call incone(bufdim,buffer,line,icol+2,lt)

c     do no accept blank input if there is a default
      if(line(1:lt).eq.' ' .and. linsav(icol+2:79).ne.' ')then
	 line=linsav
	 goto 100
      endif

      call outcon(0,0,char(13))

      return
      end


      subroutine incone(bufdim,buffer,line,pos,lt)

c     This version for Unix GNU with a noncanonical window (run
c     the executable within gaseqs_shell.)

c     Read a line from the keyboard with editing.
c     Can also be used to read a single character, by setting pos to -pos.

c     Requires subroutine outcon

c     Leaves the cursor at the start of the line, but does not do
c     a linefeed.

      implicit none

c parameters

c     W: buffer of saved previous inputs
      integer bufdim
      character*80 buffer(bufdim)
c     I: prompt line
c     O: user input
      character*80 line
c     I: |pos| is the first character position in the line that allows editing;
c        if pos is negative a single character will be read
      integer pos
c     O: length of the user's input
      integer lt

c     Notes:
c
c     When line(|pos|:79) is not empty, it is taken as default,
c     which the user can accept by hitting Return, or edit
c     by moving into it with the right arrow or similar.
c
c     When the user enters empty input, lt will be 1 and line blank.

c local

c     position of the cursor in the screen line (.le.80)
      integer i
c     user input range
      integer ilo,ihi

c     index over the characters in the line
      integer j

c     input character as a character and an integer
      character*1 in
      integer ic,ic0

c     backspace characters
      character*80 bsline

c     blanks
      character*80 blanks

c     1 if insert mode is on, 0 if not
      integer insert

c     whether we are waiting for the user to take the default
      logical wait

c     bell count
      integer bell

c     buffer location
      integer bufloc,buf
      save bufloc
      data bufloc/0/

c     buffer string length
      integer buflt

c     error code
      integer ierr

c     function that returns the key pressed
      integer fget
      external fget

c executable

      if(bufloc.eq.0)then
	 do 100 buf=1,bufdim
	    buffer(buf)(1:1)=char(0)
  100	 continue
	 bufloc=bufdim
      endif

      if(abs(pos).lt.1 .or. abs(pos).gt.79)then
	 write(line(1:10),105)abs(pos)
  105	 format(i10)
	 call outcon(0,1,'*** INCONE: invalid pos:'//line(1:10))
	 goto 9999
      endif

c     initialize bs characters, blanks, insert mode
      do 110 j=1,80
	 bsline(j:j)=char(8)
  110 continue
      blanks=' '
      insert=1

c     new buffer location
      bufloc=mod(bufloc,bufdim)+1
      buf=bufloc

c     initialize the line
      ilo=abs(pos)
      ihi=abs(pos)-1
      i=ilo
      wait=.false.
      if(line(abs(pos):79).ne.' ')then
	 do 200 j=79,abs(pos),-1
	    ihi=j
	    if(line(j:j).ne.' ')goto 210
  200	 continue
  210	 continue
	 wait=.true.
      endif

c     write the line
      call outcon(1,0,line(1:79)//bsline(i:79))

c     main loop on the user key presses

c     get the next key
 1000 bell=0
 1001 ic0=-1
      ierr=fget(in)
      if(ierr.ne.0)goto 9990
      ic=ichar(in)

c     Ctrl-C stops
      if(ic.eq.3)goto 9999

c     Escape looks for an escape sequence
      if(ic.eq.27)then

c        get next character
         ic0=27
         ierr=fget(in)
         if(ierr.ne.0)goto 9990
         ic=ichar(in)

c        ignore [ or O second character

         if(in.eq.'[' .or. in.eq.'O')then

            ierr=fget(in)
            if(ierr.ne.0)goto 9990
            ic=ichar(in)

         endif

c        Up
         if(in.eq.'A')goto 8800

c        Down
         if(in.eq.'B')goto 8900

c        Right
         if(in.eq.'C')goto 8600

c        Left
         if(in.eq.'D')goto 8500

c        ignore F1 through F4
         if(in.ge.'P' .and. in.le.'S')goto 8100

c        Ignore rest
         goto 8100

      endif

c     return immediately when pos is negative
      if(pos.lt.0)then
         if(ic0.ne.-1)goto 1000
         line(1:1)=in
         lt=1
         return
      endif

c     Backspace
      if(ic.eq.8)goto 8300

c     Delete, often swapped with backspace in Unix, is the same
      if(ic.eq.127)goto 8300

c     Return or Enter accepts
      if(ic.eq.13)goto 9000

c     Linefeed, often swapped with carriage return in Unix, is the same
      if(ic.eq.10)goto 9000

c     Tab, being a pain, is converted into space
      if(ic.eq.9)then
	 ic=32
	 in=char(ic)
      endif

c     regular characters insert themselves
      if(ic.ge.32 .and. ic.le.126)goto 8200

c     Home on ^A
      if(ic.eq.1)goto 8700

c     End on ^E
      if(ic.eq.5)goto 8750

c     Right on ^F
      if(ic.eq.6)goto 8600

c     Left on ^B
      if(ic.eq.2)goto 8500

c     ignore key
 8100 bell=bell+1
      if(bell.le.2)call outcon(1,0,char(7))
      goto 1001

c     add character to string at position i
 8200 if(wait)then
	 call outcon(1,0,blanks(ilo:ihi)//bsline(ilo:ihi))
	 ihi=ilo-1
	 i=ilo
	 wait=.false.
      endif
      if(i.gt.79)goto 8100
      if(insert.eq.1)then
	 if(ihi.ge.79)goto 8100
	 do 8210 j=ihi,i,-1
	    line(j+1:j+1)=line(j:j)
 8210	 continue
	 line(i:i)=in
	 ihi=ihi+1
	 i=i+1
	 call outcon(1,0,line(i-1:ihi)//bsline(i:ihi))
      else
	 line(i:i)=in
	 if(ihi.lt.i)ihi=i
	 i=i+1
	 call outcon(1,0,in)
      endif
      goto 1000

c     clear string
 8250 wait=.false.
      if(ihi.lt.ilo)goto 1000
      if(i.gt.ilo)call outcon(1,0,bsline(ilo:i-1))
      call outcon(1,0,blanks(ilo:ihi)//bsline(ilo:ihi))
      ihi=ilo-1
      i=ilo
      goto 1000

c     delete character left
 8300 if(i.le.ilo)goto 1000
      do 8310 j=i-1,ihi-1
	 line(j:j)=line(j+1:j+1)
 8310 continue
      i=i-1
      ihi=ihi-1
      call outcon(1,0,bsline(1:1)//line(i:ihi)//' '//bsline(i:ihi+1))
      goto 1000

c     delete character right
 8400 if(wait)goto 8250
      if(i.gt.ihi)goto 1000
      do 8410 j=i,ihi-1
	 line(j:j)=line(j+1:j+1)
 8410 continue
      ihi=ihi-1
      call outcon(1,0,line(i:ihi)//' '//bsline(i:ihi+1))
      goto 1000

c     go left
 8500 if(i.le.ilo)goto 1000
      wait=.false.
      call outcon(1,0,bsline(1:1))
      i=i-1
      goto 1000

c     go right
 8600 wait=.false.
      if(i.gt.ihi)goto 1000
      call outcon(1,0,line(i:i))
      i=i+1
      goto 1000

c     home
 8700 wait=.false.
      if(i.le.ilo)goto 1000
      call outcon(1,0,bsline(ilo:i-1))
      i=ilo
      goto 1000

c     end
 8750 wait=.false.
      if(i.gt.ihi)goto 1000
      call outcon(1,0,line(i:ihi))
      i=ihi+1
      goto 1000

c     up
 8800 if(buf.eq.bufloc)buffer(bufloc)(1:1)=char(0)
      if(buf.eq.bufloc .and. ihi.ge.ilo)
     & buffer(bufloc)=line(ilo:ihi)//char(0)
      buf=mod(buf-2+bufdim,bufdim)+1
      if(buf.eq.bufloc)goto 8890
      buflt=index(buffer(buf),char(0))-1
      if(buflt.le.0)goto 8890
 8850 if(i.gt.ilo)call outcon(1,0,bsline(ilo:i-1))
      if(ihi.ge.ilo)call outcon(1,0,blanks(ilo:ihi)//bsline(ilo:ihi))
      buflt=min(buflt,79-ilo+1)
      ihi=ilo+buflt-1
      i=ilo
      wait=.false.
      if(buflt.eq.0)goto 1000
      line(ilo:ihi)=buffer(buf)(1:buflt)
      call outcon(1,0,line(ilo:ihi)//bsline(ilo:ihi))
      wait=.true.
      goto 1000
 8890 buf=mod(buf,bufdim)+1
      goto 1000

c     down
 8900 if(buf.eq.bufloc)goto 1000
      buf=mod(buf,bufdim)+1
      buflt=index(buffer(buf),char(0))-1
      goto 8850

c     reformat the user's input and put into the buffer if not nil

 9000 if(i.gt.1)call outcon(1,0,bsline(1:i-1))
      lt=ihi-ilo+1
      if(lt.gt.0)then
	 do 9100 j=1,lt
	    line(j:j)=line(j+ilo-1:j+ilo-1)
 9100	 continue
	 line(lt+1:80)=' '
	 buffer(bufloc)=line(1:lt)//char(0)
      else
	 line=' '
	 lt=1
	 bufloc=mod(bufloc-2+bufdim,bufdim)+1
	 buffer(bufloc)(1:1)=char(0)
      endif

      return

 9990 call outcon(0,1,'*** error reading keyboard')

 9999 call errxit
      end


      subroutine idle(secs)

c     This version for Unix GNU.

c     Wait a number of seconds.

c input

c     number of seconds to wait
      real secs

c local

c     run time
      real sec0,sec

      if(secs.lt.0.)then
	 call outcon(0,1,'*** IDLE: invalid time')
	 call errxit
      endif

      call second(sec0)
  100 call second(sec)
      if(sec.ge.sec0 .and. sec-sec0.lt.secs)goto 100

      return
      end
