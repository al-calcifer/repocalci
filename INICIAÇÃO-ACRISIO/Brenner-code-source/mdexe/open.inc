
c standard input/output files

c     used by md/Subroutines/General/read_write.f
      open(13,file='input.d',status='old') 

c     used by md/Subroutines/General/read_write.f
      open(11,file='coord.d',status='old') 

c     used by md/Subroutines/General/read_write.f
c     used by md/Subroutines/General/setin.f
      open(9,file='output.d',status='unknown') 

c data files for REBO potential

c     used by md/Subroutines/Bond_Order/param.f
      open(14,file='../md/Spline/inter3d_iv_new.d',
     &status='old')

c     used by md/Subroutines/Bond_Order/param.f
      open(15,file='../md/Spline/inter2d_iv.d',
     &status='old')

c     used by md/Subroutines/Bond_Order/param.f
      open(16,file='../md/Spline/inter3dtors.d',
     &status='old')

c     used by md/Subroutines/Bond_Order/param.f
      open(17,file='../md/Spline/inter3d_h.d',
     &status='old')

c     used by md/Subroutines/Bond_Order/param.f
      open(18,file='../md/Spline/inter3d_ch.d',
     &status='old')
 
c optional input/output files

c     used by md/Subroutines/Special/max_ke.f
c     open(55,file='max_ke.d',status='unknown') 

c     used by md/Subroutines/General/xmol.f
      open(85,file='pair_energy.d',status='unknown')

c     used by md/Subroutines/General/overwr.f
c     open(50,file='overwrite.d',status='unknown')

c     not used by anything.  Presumably for load.f
c     open(51,file='load.d',status='unknown')

c     used by md/Subroutines/General/xmol.f
      open(1,file='xmol.d',status='unknown') 

C output files for tight binding 

c      used by md/Subroutines/Tight_Binding/dos.f
       open(84,file='eigenvectors.d',status='unknown')

c      used by md/Subroutines/Tight_Binding/dos.f
       open(86,file='eigenenergies.d',status='unknown')

c      used by md/Subroutines/Tight_Binding/dos.f
       open(87,file='dos.d',status='unknown')

c      used by md/Subroutines/Tight_Binding/dos.f
       open(88,file='ldos.d',status='unknown')
