      SUBROUTINE LJGUTS
      IMPLICIT REAL*8(A-H,O-Z)
      include 'common_files.inc'

cleon:
c     Set the following variable to 1 to revert to original discontinuous
c     potential which does not conserve energy and maybe save a slight bit
c     of operations
      ijump=0
cleon

C
c Find the LJ neighbor list
c
      IF(LCHK.EQ.1) THEN
      kli=0
      do i=1,np-1
          do 311 j=i+1,np
               ki = ktype(i)
               kj = ktype(j)

               if(RSLJ(ki,kj).eq.0.0d0) go to 311
               RSQS=0.0D0
c
               DO L=1,3
                    RR=R0(I,L)-R0(J,L)
                    RR = RR - CUBE(L)*ANINT(RR/CUBE(L))
                    RSQS=RSQS+rr*rr
                    if(rsqs.gt.RSLJ(ki,kj)) go to 311
               enddo
cleon: we cannot skip atoms that are close enough to be in the bond-order range
c              IF(RSQS.lt.xmms(ki,kj)) GO TO 311
               IF(RSQS.lt.xmms(ki,kj) .and. ijump.ne.0) GO TO 311
cleon
               kli=kli+1
cleon: moved up to avoid violations
               if(kli.gt.nmabig) then
                  write(*,*) 'LJ neighbor array= ',kli
                  write(*,*) 'increase nmabig and recompile'
                  stop 
               endif
cleon
               iv(kli)=i
               jv(kli)=j
 311       continue
      enddo

      kliend=kli
      endif
c
300   format(2i10,3f15.5)
      pvdw=0.D0
      do 323 llj=1,kliend
           i=iv(llj)
           j=jv(llj)
           ki=ktype(i)
           kj=ktype(j)
c      write(802,*) i,j,ki,kj,RMAXLJ(ki,kj),XMM(KI,kj)
c
           RSQS=0.0D0

           DO L=1,3
               RRS(L)=R0(I,L)-R0(J,L)
               RRS(L) = RRS(L) - CUBE(L)*ANINT(RRS(L)/CUBE(L))
               RSQS=RSQS+RRS(L)*RRS(L)
               IF(RSQS.GT.RMAXLJ(ki,kj)) go to 323
           ENDDO
C
cleon: add the potential in the bond-order range
           if(RSQS.LT.XMM(KI,kj) .and. ijump.eq.0)
     &      pvdw=pvdw+vlook(1,ki,kj)
cleon
           if(RSQS.LT.XMM(KI,kj)) go to 323
c table look up with linear interpolation
                 r = sqrt(rsqs) - xm(ki,kj)
                 rt = r/dellj
                 ii = int(rt)+1
                 vdw  = vlook(ii,ki,kj) +
     &                  (vlook(ii+1,ki,kj)-vlook(ii,ki,kj))*(rt-ii+1)
                 dvdw = dlook(ii,ki,kj) +
     &                  (dlook(ii+1,ki,kj)-dlook(ii,ki,kj))*(rt-ii+1)


c$            endif
c
            pvdw=pvdw+vdw
c included by Acrisio (Energy with no ar-ar interaction)
            if((ki.eq.5).and.(kj.eq.5)) then
            pvdw2=pvdw-vdw
            endif
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
            do l=1,3
                 dr = dvdw*rrs(l)
                 rnp(i,l)=rnp(i,l) + dr
                 rnp(j,l)=rnp(j,l) - dr
            enddo
c      write(803,*) i,j,ki,kj,RMAXLJ(ki,kj),XMM(KI,kj),RSQS
 323  continue

      toteNOAr=tote + pvdw2
      toteNOAr2=tote + pvdw
      tote = tote + pvdw
      return
      end

