----------------------------------------------------------------------

This directory contains a set of fortran subroutines that
- simplify reading user input
- allow line editing
- keep a log file of input and output
- allow error recovery from the log file
- allow input lines to the program to be given on the command line

Copyright to any parts not owned by others belongs to FAMU-FSU
College of Engineering.  First version 2003.

This program and source can be freely used as long as this file
is included.

----------------------------------------------------------------------

The main subroutines can be found in file lib.f.  See there for their
function.  Your main program should run libini at its startup to
initialize the I/O channels used by lib.f.

To compile your program, in addition to lib.f you will also need a
con....f file and an args....f file.  The generic files congen.f and
argsgen.f will work on most systems, or can readily be fixed if not.

However, to enable line editing and other neat stuff, you need better
than those two generic files.  There are con*.f files and args*.f
files for a number of common compliers included, such as Gnu Fortran
for Unix, for Windows, Watcom, Lahey lp77, etcetera.  You can always
write your own.

----------------------------------------------------------------------

Let me know if you find any bugs or have improvements.
Leon van Dommelen (dommelen@eng.fsu.edu)
