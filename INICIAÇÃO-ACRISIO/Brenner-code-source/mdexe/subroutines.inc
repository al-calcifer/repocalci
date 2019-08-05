c routine calling potentials/forces - application specific 
      include '../md/Subroutines/General/model.f' 

c bond order potential subroutines 
      include '../md/Subroutines/Bond_Order/caguts.f'
      include '../md/Subroutines/Bond_Order/bcuint.f'
      include '../md/Subroutines/Bond_Order/mtable.f'
c     set its input files in open.inc, close.inc
      include '../md/Subroutines/Bond_Order/param.f'
      include '../md/Subroutines/Bond_Order/pibond.f'
      include '../md/Subroutines/Bond_Order/radic.f'
      include '../md/Subroutines/Bond_Order/sili_germ.f'
      include '../md/Subroutines/Bond_Order/tor.f'

c tight binding 
      include '../md/Subroutines/Tight_Binding/E_corr.f' 
      include '../md/Subroutines/Tight_Binding/dsmooth.f'
      include '../md/Subroutines/Tight_Binding/force_tb.f'
      include '../md/Subroutines/Tight_Binding/laske_H.f'
      include '../md/Subroutines/Tight_Binding/neighbor_tb.f'
      include '../md/Subroutines/Tight_Binding/smooth.f'
      include '../md/Subroutines/Tight_Binding/tight_bind.f'
      include '../md/Subroutines/Tight_Binding/tilatiheys.f'
      include '../md/Subroutines/Tight_Binding/xkorjaus.f'
      include '../md/Subroutines/Tight_Binding/eispack.f'
c     set its input files in open.inc, close.inc
      include '../md/Subroutines/Tight_Binding/dos.f'

c Lennard-Jones routines
c     include '../md/Subroutines/Lennard_Jones/ljcont.f'
      include '../md/Subroutines/Lennard_Jones/ljcset.f'
      include '../md/Subroutines/Lennard_Jones/ljguts.f'
      include '../md/Subroutines/Lennard_Jones/ljparam.f'

c general MD routines 
c     set its input files in open.inc, close.inc
      include 'read_write.f' 
      include 'setin.f' 
c     set its input file in open.inc, close.inc
c     include '../md/Subroutines/General/overwr.f'
      include '../md/Subroutines/General/pred_corr.f'
      include '../md/Subroutines/General/reflect.f'
      include '../md/Subroutines/General/thermostats.f'
      include 'random_numbers.f'

c generate data 
c     set its input files in open.inc, close.inc
      include '../md/Subroutines/General/xmol.f'

c energy minimizer
      include '../md/Subroutines/General/minimize.f'

c volume scaling
c      include '../md/Subroutines/Special/vscale.f'

c specialized routines
c
c    indenting
c 
c      include '../md/Subroutines/Special/move_tip.f'
c      include '../md/Subroutines/Special/load.f'
c
c    adding energetic particles
c 
c      include '../md/Subroutines/Special/bombard.f' 
c      include '../md/Subroutines/Special/reset_itr.f'
c      set its input file in open.inc, close.inc
c      include '../md/Subroutines/Special/max_ke.f' 

c subroutines added by leon
       include 'check_box.f'
       include 'coord_read_hook.f'
       include 'coord_write_hook.f'
       include 'flush.f'
       include 'param_mod.f'
       include 'setr2.f'
       include 'setran_hook.f'
       include 'step_end_hook.f'
