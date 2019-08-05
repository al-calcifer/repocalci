      subroutine xyzw(filnam,kdim,kmax,an,r0)

c     Create file filnam in xyz format.  Uses unit 3.

      implicit none

c input

c     name of the file to create, overwritten if it already exists
      character*(*) filnam

c     declared number of atoms
      integer kdim
c     actual number of atoms
      integer kmax
c     atom number
      integer an(kdim)
c     first Nordsieck parameter (atom positions)
      real r0(kdim,3)

c local

c     local version of the file name
      character*(40) filnm
      integer filnml

c     length and radius of the cylinder with the atoms
      real lt,rad

c     atom index
      integer k

c executable

c     put file name in local storage (needed for gnu fortran)
      filnm=filnam
      filnml=index(filnm,' ')-1
      if(filnml.lt.1)filnml=40

c     find the limits of the atom positions
      lt=0.
      rad=0.
      do 100 k=1,kmax
         lt=max(lt,abs(r0(k,3)))
         rad=max(rad,r0(k,1)**2+r0(k,2)**2)
 100  continue
      rad=sqrt(rad)

c     open the file, replacing any pre-existing one
      call opennw(3,filnam)

c     write the header
      write(3,110,err=910)kmax
 110  format(i6)
      write(3,120,err=910)lt,rad
 120  format(2g16.5)

c     write the atom positions
      do 150 k=1,kmax
         if(an(k).eq.1)then
            write(3,130,err=910)'H',r0(k,1),r0(k,2),r0(k,3)
 130        format(a2,' ',3f10.3)
         else if(an(k).eq.6)then
            write(3,130,err=910)'C',r0(k,1),r0(k,2),r0(k,3)
         else
            write(3,140,err=910)an(k),r0(k,1),r0(k,2),r0(k,3)
 140        format(i3,3f10.3)
         endif
 150  continue

c     close file
      close(3,err=920)

      call out('File '//filnm(1:filnml)//' generated')
      return

 910  call out('*** unable to write to file '//filnm(1:filnml))
      call out('    Check disk space, permissions, and quota!')
 920  call out('*** unable to close file '//filnm(1:filnml))
      call out('    Check disk space, permissions, and quota!')
      return
      end
