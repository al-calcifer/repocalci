      subroutine libini(prgnam,iounit)

c     This subroutine should be called at the start of any program
c     that uses the other subroutines in this file.

c     If a recovery file 'prgnam'.rcv exists, subroutine in will use its
c     first lines as user inputs.  This allows error recovery, by
c     copying the log file 'prgnam'.log created during the erroneous run
c     into a file 'prgnam'.rcv and correcting the erroneous input(s).

c     If no recovery file exist, and program arguments were specified,
c     subroutine 'in' will take them as the first user inputs instead.
c     This allows noninteractive use of the program, (if nothing goes
c     wrong with the provided inputs.)  (This feature will not work if
c     argsgen.f is used to link the progam with.)

c     If there are both arguments and a recovery file, the program will
c     terminate in error, since the input source is ambiguous.

      implicit none

c input

c     Program name, or any other name to use for the recovery and log files.
c     May not contain blanks.
      character*(*) prgnam

c     Start of I/O unit range reserved for lib.f.   Set this higher than
c     any other I/O unit used by the program.
      integer iounit

c     Note that lib also reserves a common block name for its use, lib_cm

c common

c     I/O units.  They are already reserved if nonzero.  If in use,
c     io(i) must be io(1)+i-1.  This subroutine sets io(1) and io(2) for
c     a log file and a recovery file
      integer io(10)
c     exit code returned on error
      integer errcod
      common/lib_cm/io,errcod

c local

c     declared number of command line arguments
      integer argdim
      parameter (argdim=10)
c     number of arguments (set to -1 if there are too many arguments)
      integer argmx
c     argument index
      integer arg
c     arguments
      character*80 argt(argdim)
      integer argtl(argdim)

c     whether a recovery file exists
      logical exists

c     local version of prgnam, needed for the gnu compiler
      character*40 prgnm
      integer prgnml

c     index
      integer i

c executable

c     set the exit code returned on error to 1
      errcod=1

c     reserve two I/O units, one for the log file, one for the recovery file
      io(1)=iounit
      if(io(1).lt.1)io(1)=7
      io(2)=-1

c     mark the other I/O units as not in use.
      do 10 i=3,10
         io(i)=0
 10   continue

c     check file name
      if(index(prgnam,' ').gt.0)
     & call out('*** PROGRAM ERROR: log file name has blanks!')

c     create a file name
      prgnm=prgnam
      prgnml=index(prgnm,' ')-1
      if(prgnml.lt.1)prgnml=40

c     open the log file
      if(iounit.lt.1)call out('*** PROGRAM ERROR: invalid I/O unit!')
      call opennw(iounit,prgnm(1:prgnml)//'.log')

c     get the command line arguments
      call args(argdim,argmx,argt,argtl)
      if(argmx.lt.0)
     & call out('*** More than 10 program arguments!')

c     see whether the recovery file exists
      inquire(file=prgnm(1:prgnml)//'.rcv',exist=exists,err=9990)

c     make a recovery file from the command line arguments, if any
      if(argmx.gt.0)then

c        check whether we have ambiguous input sources
         if(exists)call out(
     &    '*** Program arguments are not allowed with a recovery file!')

c        open the file
         io(2)=io(1)+1
         call opennw(io(2),'tmp_'//prgnm(1:prgnml)//'.rcv')

c        write to the file
         do 100 arg=1,argmx
            write(io(2),90,err=9991)argt(arg)(1:argtl(arg))
 90         format('input-->',a)
 100     continue

         close(io(2),err=9992)
         open(io(2),file='tmp_'//prgnm(1:prgnml)//'.rcv',
     &    status='old',err=9993)

      endif

c     open the recovery file, if any
      if(exists)then
         io(2)=io(1)+1
         open(io(2),file=prgnm(1:prgnml)//'.rcv',status='old',err=9996)
      endif

      return

 9990 call out('*** Unable to examine disk:')
      call out('    check directory permissions!')
 9991 call out('*** Unable to write to temporary file tmp_'//
     & prgnm(1:prgnml)//'.rcv:')
      call out('    check disk space!')
 9992 call out('*** Unable to close temporary file tmp_'//
     & prgnm(1:prgnml)//'.rcv:')
      call out('    check disk space!')
 9993 call out('*** Unable to open temporary file tmp_'//
     & prgnm(1:prgnml)//'.rcv:')
      call out('    check disk!')
 9996 call out('*** Unable to open recovery file '//
     & prgnm(1:prgnml)//'.rcv:')
      call out('    check file permissions!')

      return
      end


      subroutine out(str)

c     Prints string str on the screen and writes it to the log file.

c     Rings a bell when the first character of the string is '*'.

c     Terminates program if str contains an exclamation mark '!'.
c     Use subroutine out2 below if that is not desirable.

      implicit none

c input

c     string to write
      character*(*) str

c common

c     I/O units.  They are already reserved if nonzero.
c     If in use, io(i) must be io(1)+i-1
      integer io(10)
c     exit code returned on error
      integer errcod
      common/lib_cm/io,errcod

c executable

c     let out2 write the string
      call out2(str)

c     exit on '!'
      if(index(str,'!').gt.0)call errxit

      return
      end


      subroutine out2(str)

c     version of out that will not terminate the program on exclamation marks

      implicit none

c input

c     string to write
      character*(*) str

c executable

c     print the string, with bell if the first character is '*'
      if(str(1:1).eq.'*')then
         call outcon(0,1,str)
      else
         call outcon(0,0,str)
      endif

c     write the string to the log file
      call outlog(str)

      return
      end



      subroutine outlog(str)

c     Writes string str to the log file.

c     This subroutine is called by out and out2, and should usually not
c     be called directly.

      implicit none

c input

c     string to write
      character*(*) str

c common

c     I/O units.  They are already reserved if nonzero.
c     If in use, io(i) must be io(1)+i-1
      integer io(10)
c     exit code returned on error
      integer errcod
      common/lib_cm/io,errcod

c executable

c     skip if we do not yet have a log file open
      if(io(1).le.0)return

      if(str.ne.' ')then
         write(io(1),10,err=900)str
 10      format(a)
      else
         write(io(1),*,err=900)
      endif

      return

  900 call outcon(0,1,'*** Unable to write to log file:')
      call outcon(0,0,'    check disk space and permissions')
      call errxit
      end


      subroutine in(line)

c     Reads a line from input (normally from the keyboard, but if
c     a recovery file is open, that is read first.)

      implicit none

c input/output

c     input: prompt to use.  Must be terminated by a colon.
c            Any default must follow the colon and a space.
c     output: line read
      character*80 line

c common

c     I/O units.  They are already reserved if nonzero.
c     If in use, io(i) must be io(1)+i-1
      integer io(10)
c     exit code returned on error
      integer errcod
      common/lib_cm/io,errcod

c local

c     character index
      integer i,imx

c     position of colon in prompt
      integer icol,icol3

c     whether there is a default
      logical defalt

c     line from recovery file
      character*88 line2,line3

c executable

c     find colon
      icol=index(line,':')
      if(icol.le.0)
     & call out('*** PROGRAM ERROR: colon missing after prompt!')
      if(icol.gt.77)
     & call out('*** PROGRAM ERROR: prompt too long!')
      if(line(icol+1:icol+1).ne.' ')
     & call out('*** PROGRAM ERROR: colon must be followed by a space!')
      defalt=.false.
      if(line(icol:80).ne.':')defalt=.true.

c     write the prompt to the log file
      call outlog(line(1:icol))

c     read from the recovery file if open
      if(io(2).le.0)goto 100
      line2=' '
 50   line3=line2
      read(io(2),51,err=9992,end=90)line2
 51   format(a88)
      if(line2(1:8).ne.'input-->')goto 50
c     check whether the prompt is the same if present
      icol3=index(line3,':')
      if(line3(1:8).ne.'input-->' .and. icol3.gt.0)then
         if(line(1:icol).ne.line3(1:icol3))then
            call out(
     &       '*** Recovery file prompt does not match current one:')
            call out2('    '//line3(1:min(75,icol3)))
            call out2('    '//line(1:min(75,icol)))
            line3='    Hit Return to continue or enter q to quit:'
            call incon(line3)
            if(line3.eq.'q')call out('Terminated!')
         endif
      endif
      call outcon(0,0,line(1:icol)//' '//line2(9:79+8-icol-1))
      call idle(.3)
      line=line2(9:88)
      if(defalt .and. line.eq.' ')
     & call out('*** missing value in recovery file!')
      goto 110
 90   close(io(2),err=9993)
      io(2)=-1

c     read from the keyboard
 100  call incon(line)
 110  do 200 i=80,1,-1
         imx=i
         if(line(i:i).gt.' ')goto 210
 200  continue
 210  if(defalt .and. line(1:imx).eq.' ')
     & call out('*** PROGRAM ERROR: lost default!')
      call outlog('input-->'//line(1:imx))

      return

 9992 call out('*** Unable to read from recovery file!')
 9993 call out('*** Unable to close recovery file!')
      return
      end


      subroutine errset(i)

c     Sets the error exit status to i.

      implicit none

c input

c     new error code
      integer i

c common

c     I/O units.  They are already reserved if nonzero.
c     If in use, io(i) must be io(1)+i-1
      integer io(10)
c     exit code returned on error
      integer errcod
      common/lib_cm/io,errcod

c executable

c     set the new code
      errcod=i

      return
      end


      subroutine errxit

c     Exits with the set error status.

      implicit none

c common

c     I/O units.  They are already reserved if nonzero.
c     If in use, io(i) must be io(1)+i-1
      integer io(10)
c     exit code returned on error
      integer errcod
      common/lib_cm/io,errcod

c executable

      call exit(errcod)

      return
      end


      subroutine opennw(lun,filnam)

c     open a new file, replacing any pre-existing one

c input

c     I/O unit number:
      integer lun
c     file name:
      character*(*) filnam

c local

c     whether the file already exists:
      logical exists

c     local version of the file name
      character*(40) filnm
      integer filnml

c     put file name in local storage (needed for gnu fortran)
      filnm=filnam
      filnml=index(filnm,' ')-1
      if(filnml.lt.1)filnml=40

c     get rid of any previous version:
      inquire(file=filnam,exist=exists,err=900)
      if(exists)then
         open(lun,file=filnam,status='old',err=910)
         close(lun,status='delete',err=910)
      endif

c     open the new version:
      open(lun,file=filnam,status='new',err=920)

      return

 900  call out('*** Unable to look for file '//filnm(1:filnml))
      call out('    check disk!')
 910  call out('*** Unable to delete file '//filnm(1:filnml))
      call out('    check file permissions and quota!')
 920  call out('*** Unable to open file '//filnm(1:filnml))
      call out('    check file permissions and quota!')
      return
      end
