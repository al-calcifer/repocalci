#include <conio.h>
#include <i86.h>


/**********************************************************************\
This version for the open Watcom C and Fortran compiler pair.

Read a key from the keyboard.

Warning: linked into Fortran, neither returns Ctrl-C or terminates on it
\**********************************************************************/

int getkey(void)
{
   return getch();
}


/**********************************************************************\
This version for the open Watcom C and Fortran compiler pair.

Wait for a given number of miliseconds
\**********************************************************************/

void idle2(int msecs)
{
   unsigned int msecs2;

   msecs2=msecs;
   delay(msecs2);

}
