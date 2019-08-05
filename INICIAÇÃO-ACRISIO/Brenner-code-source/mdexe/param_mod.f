      subroutine par_mod

c     change some parameters

      implicit none
      include 'common_files.inc'

c     turn the torsional potential off
c     ndihed=10
c     turn the torsional potential on
      ndihed=2

c     use the original torsional potential which does not conserve energy
c     sinlo=1.0d-01
c     sinhi=1.0d-01
c     make sure that the torsional potential conserves energy
      sinlo=0.0d0
      sinhi=0.2d0

      return
      end
