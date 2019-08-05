      Parameter(Cmass = 12.011*1.660538782e-27, hbar=6.582119e-16)

      Parameter(pi=3.1415926536,c=2.99792458e8,ndos=2000)

      Parameter (boltzmann=8.617342e-5)

      Include 'Param'

      Dimension A(3*Nat,3*Nat),D(3*Nat), ZZ(3*Nat)



      Open(10,File='diagonal')

      Open(11,File='frequencias')

      Open(12,File='energialivre')

      Open(13,File='dos')

      



c *** Supondo aceleracao em A/(fs)^2, deltar em A



      Do i=1,3*Nat

      Do j=1,3*Nat

         A(i,j)=0.

      Enddo

      Enddo



      do ipm=1,2

         Do iat=1,3*nat

            do jat=1,3*nat

               read(10,*)df

               A(iat,jat) = A(iat,jat) + (-1)**(ipm)*df

            enddo

         enddo

      enddo



      Do i=1,3*Nat

      Do j=1,3*Nat

         A(i,j)=-8.*A(i,j)/(2*deltar)

      Enddo

      Enddo



      CALL TRED2(A,3*Nat,3*nat,D,ZZ)

      CALL TQLI(D,ZZ,3*nat,3*nat,A)

      call eigsrt(D,A,3*nat,3*nat)



      do iat=1,3*nat

         freq=1.e15*Sqrt(Abs(d(iat)))

         waveinv=0.01*freq/(2*pi*c)

         write(11,*)Abs(d(iat))*waveinv/d(iat)

      enddo



c *** Calculo da energia livre na aproximacao harmonica

      Do iT=0,400,10

         sum=0.

         do iat=4,3*nat

            freq=1.e15*Sqrt(Abs(d(iat)))

            energy=freq*hbar

            If(iT.ne.0)Then 

               zlog = log(1.- (exp(-energy/(boltzmann*iT))))

            Else

               zlog = 0.

            Endif   

            sum=sum + boltzmann*iT*zlog + 0.5*energy

         enddo

         sum=sum/Nat

      Write(12,*)iT, sum

      Enddo



c *** Calculo da densidade de estados de fonons

c *** alargamento gaussiano



      sigma = 10.*1800./(3.*nat)

      

      Do iwave=1,ndos

         density = 0.

         wavenumberinv=(iwave-1.)*1800./(ndos-1.)-10.

         do iat=1,3*nat

            freq=1.e15*Sqrt(Abs(d(iat)))

            waveinv=0.01*freq/(2*pi*c)

            waveinv=Abs(d(iat))*waveinv/d(iat)

            delta=wavenumberinv-waveinv

            If(Abs(delta).lt.(5*sigma))Then

               gauss = exp(-(delta**2)/(sigma**2))

               density = density + gauss

            EndIf   

         enddo

         write(13,*)wavenumberinv,density

      Enddo   



c      Print*,'Zero-point energy is ', sum, ' eV/atom.'



      end



      SUBROUTINE EIGSRT(D,V,N,NP)

      DIMENSION D(NP),V(NP,NP)

      DO 13 I=1,N-1

        K=I

        P=D(I)

        DO 11 J=I+1,N

          IF(D(J).LE.P)THEN

            K=J

            P=D(J)

          ENDIF

11      CONTINUE

        IF(K.NE.I)THEN

          D(K)=D(I)

          D(I)=P

          DO 12 J=1,N

            P=V(J,I)

            V(J,I)=V(J,K)

            V(J,K)=P

12        CONTINUE

        ENDIF

13    CONTINUE

      RETURN

      END



      SUBROUTINE TRED2(A,N,NP,D,E)

      DIMENSION A(NP,NP),D(NP),E(NP)

      IF(N.GT.1)THEN

        DO 18 I=N,2,-1

          L=I-1

          H=0.

          SCALE=0.

          IF(L.GT.1)THEN

            DO 11 K=1,L

              SCALE=SCALE+ABS(A(I,K))

11          CONTINUE

            IF(SCALE.EQ.0.)THEN

              E(I)=A(I,L)

            ELSE

              DO 12 K=1,L

                A(I,K)=A(I,K)/SCALE

                H=H+A(I,K)**2

12            CONTINUE

              F=A(I,L)

              G=-SIGN(SQRT(H),F)

              E(I)=SCALE*G

              H=H-F*G

              A(I,L)=F-G

              F=0.

              DO 15 J=1,L

c only eigval                A(J,I)=A(I,J)/H

                G=0.

                DO 13 K=1,J

                  G=G+A(J,K)*A(I,K)

13              CONTINUE

                IF(L.GT.J)THEN

                  DO 14 K=J+1,L

                    G=G+A(K,J)*A(I,K)

14                CONTINUE

                ENDIF

                E(J)=G/H

                F=F+E(J)*A(I,J)

15            CONTINUE

              HH=F/(H+H)

              DO 17 J=1,L

                F=A(I,J)

                G=E(J)-HH*F

                E(J)=G

                DO 16 K=1,J

                  A(J,K)=A(J,K)-F*E(K)-G*A(I,K)

16              CONTINUE

17            CONTINUE

            ENDIF

          ELSE

            E(I)=A(I,L)

          ENDIF

          D(I)=H

18      CONTINUE

      ENDIF

c only eigval      D(1)=0.

      E(1)=0.

      DO 23 I=1,N

c        L=I-1

c        IF(D(I).NE.0.)THEN

c          DO 21 J=1,L

c            G=0.

c            DO 19 K=1,L

c              G=G+A(I,K)*A(K,J)

c19          CONTINUE

c            DO 20 K=1,L

c              A(K,J)=A(K,J)-G*A(K,I)

c20          CONTINUE

c21        CONTINUE

c        ENDIF

        D(I)=A(I,I)

c        A(I,I)=1.

c        IF(L.GE.1)THEN

c          DO 22 J=1,L

c            A(I,J)=0.

c            A(J,I)=0.

c22        CONTINUE

c        ENDIF

23    CONTINUE

      RETURN

      END



      SUBROUTINE TQLI(D,E,N,NP,Z)

      DIMENSION D(NP),E(NP),Z(NP,NP)

      IF (N.GT.1) THEN

        DO 11 I=2,N

          E(I-1)=E(I)

11      CONTINUE

        E(N)=0.

        DO 15 L=1,N

          ITER=0

1         DO 12 M=L,N-1

            DD=ABS(D(M))+ABS(D(M+1))

            IF (ABS(E(M))+DD.EQ.DD) GO TO 2

12        CONTINUE

          M=N

2         IF(M.NE.L)THEN

            IF(ITER.EQ.30)PAUSE 'too many iterations'

            ITER=ITER+1

            G=(D(L+1)-D(L))/(2.*E(L))

            R=SQRT(G**2+1.)

            G=D(M)-D(L)+E(L)/(G+SIGN(R,G))

            S=1.

            C=1.

            P=0.

            DO 14 I=M-1,L,-1

              F=S*E(I)

              B=C*E(I)

              IF(ABS(F).GE.ABS(G))THEN

                C=G/F

                R=SQRT(C**2+1.)

                E(I+1)=F*R

                S=1./R

                C=C*S

              ELSE

                S=F/G

                R=SQRT(S**2+1.)

                E(I+1)=G*R

                C=1./R

                S=S*C

              ENDIF

              G=D(I+1)-P

              R=(D(I)-G)*S+2.*C*B

              P=S*R

              D(I+1)=G+P

              G=C*R-B

c              DO 13 K=1,N

c                F=Z(K,I+1)

c                Z(K,I+1)=S*Z(K,I)+C*F

c                Z(K,I)=C*Z(K,I)-S*F

c13            CONTINUE

14          CONTINUE

            D(L)=D(L)-P

            E(L)=G

            E(M)=0.

            GO TO 1

          ENDIF

15      CONTINUE

      ENDIF

      RETURN

      END



