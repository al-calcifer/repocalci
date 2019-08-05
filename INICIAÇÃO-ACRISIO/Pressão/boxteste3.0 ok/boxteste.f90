PROGRAM Nparticle_box
IMPLICIT NONE


REAL(KIND=8), PARAMETER :: J2eV=103.642698612
REAL(KIND=8), PARAMETER :: Kb2Kelvin=0.86173308
REAL(KIND=8), PARAMETER :: PI = 3.14159265358979323846
REAL(KIND=8), PARAMETER :: KbeVK = Kb2Kelvin*0.0001
REAL(KIND=8), PARAMETER :: eVA3GPa = 160.217

REAL(KIND=8), PARAMETER :: sigma=3.41d0
REAL(KIND=8), PARAMETER :: rcutoff = 3*sigma


REAL(KIND=8), ALLOCATABLE :: rt(:,:,:),V0(:,:), r0(:,:), vt(:,:), at(:,:)
REAL(KIND=8), ALLOCATABLE :: ut(:,:)
REAL(KIND=8), ALLOCATABLE :: Kin(:)
REAL(KIND=8) :: dt, t, m
INTEGER :: I, J, Ttotal, alloc, Nat, K
REAL(KIND=8) :: r, s, L, theta, phi, modV0, Ec, E, V, dmin
REAL(KIND=8) :: N, Te, Tk, Press, Vol 
INTEGER :: Nc, Nar



OPEN(UNIT=10, FILE='mov3D.ANI')
OPEN(UNIT=13, FILE='energiac.dat')
OPEN(UNIT=14, FILE='temperatura.dat')
OPEN(UNIT=15, FILE='energiacp.dat')
OPEN(UNIT=16, FILE='energiap.dat')
OPEN(UNIT=17, FILE='pressao')
OPEN(UNIT=20, FILE='mov3D.xyz')


PRINT*,'Digite o tempo total (Ttotal):'
READ*, Ttotal
PRINT*,'Numero de particulas por aresta da caixa:'
READ*, Nar
PRINT*,'Digite o passo (dt):'
READ*, dt
PRINT*,'Digite a dimensao minima entre os atomos:'
READ*, dmin
PRINT*,'Modulo da Velocidade inicial das particulas:'
READ*, modV0

m=39.948

L=dmin*Nar


! Convencao de mínima imagem
if (L<2*rcutoff) then
   write(*,*) 'STOP!! Tente uma distancia entre os atomos um pouco maior'
   write(*,*) '...Ou mais particulas por aresta'
   write(*,*) L, 2*rcutoff
   stop
end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!

Nat=Nar**3


ALLOCATE (rt(Nat,3,3))
ALLOCATE (vt(Nat,3))
ALLOCATE (ut(Nat,3))
ALLOCATE (at(Nat,3))
ALLOCATE (V0(Nat,3))
ALLOCATE (r0(Nat,3))
ALLOCATE (Kin(Ttotal))




CALL numeros(V0,r0,Nat,modV0,L,Nar,dmin)				


DO I=1,Nat

rt(I,1,1)=r0(I,1)
rt(I,1,2)=r0(I,2)
rt(I,1,3)=r0(I,3)

rt(I,2,1)=rt(I,1,1)+V0(I,1)*dt
rt(I,2,2)=rt(I,1,2)+V0(I,2)*dt
rt(I,2,3)=rt(I,1,3)+V0(I,3)*dt

ENDDO


DEALLOCATE(V0)
DEALLOCATE(r0)


DO J=3,Ttotal
 
 t=J*dt

 DO I=1,Nat
 ut(I,1)=rt(I,2,1)
 ut(I,2)=rt(I,2,2)
 ut(I,3)=rt(I,2,3) 
 ENDDO



 CALL vetores(ut, Nat, at, m, V, L, rcutoff, t)


 DO I=1,Nat

 
 rt(I,3,1)=2*rt(I,2,1)-rt(I,1,1)+at(I,1)*(dt**2)
 rt(I,3,2)=2*rt(I,2,2)-rt(I,1,2)+at(I,2)*(dt**2)
 rt(I,3,3)=2*rt(I,2,3)-rt(I,1,3)+at(I,3)*(dt**2)

 vt(I,1)=(rt(I,3,1)-rt(I,1,1))/(2*dt)
 vt(I,2)=(rt(I,3,2)-rt(I,1,2))/(2*dt)
 vt(I,3)=(rt(I,3,3)-rt(I,1,3))/(2*dt)




   DO k=1, 3
  
     IF (rt(I,3,k) > L) THEN
        rt(I,3,k)=rt(I,3,k)-L
        rt(I,2,k)=rt(I,2,k)-L


     ENDIF

     IF (rt(I,3,k) < 0) THEN
        rt(I,3,k)=rt(I,3,k)+L
        rt(I,2,k)=rt(I,2,k)+L

     ENDIF

   ENDDO



 ENDDO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!! Imprimindo frames da animacao
IF(MOD(J,500)==0) THEN

WRITE(10,*) Nat
WRITE(10,*) ' '

DO I=1,Nar
   WRITE(10,*) 'C', rt(I,3,1),rt(I,3,2),rt(I,3,3)
   WRITE(20,*) 'C', rt(I,3,1),rt(I,3,2),rt(I,3,3)
END DO


END IF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



 
 Ec=0
 Te=0
	DO i=1, Nat
	Ec=Ec+0.5*m*(vt(i,1)*vt(i,1)+vt(i,2)*vt(i,2)+vt(i,3)*vt(i,3))
	ENDDO 

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Converting kinetic energy to eV...
! Calculating temperature to Kelvin...
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
E=Ec*J2eV
Te=(Ec*2*10000)/(Nat*3*Kb2Kelvin)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

Vol=(L)**3
Press = Press/(3*Vol)

Press = Press + ((Nat*KbeVK*Te)/Vol)

Press = eVA3GPa*Press

  IF(MOD(J,100)==0) THEN
    WRITE(13,*) t, E
    WRITE(14,*) t, Te
    WRITE(15,*) t, E + V
    WRITE(16,*) t, V
    WRITE(17,*) t, Press
    WRITE(*,*) J, E, V, Te, Press
    WRITE(500,*) J, E, V, E+V
  END IF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Transformando t em t-dt / t-dt em t-2dt

DO I=1,Nat

DO k=1, 3
 
rt(I,1,k)=rt(I,2,k)
rt(I,2,k)=rt(I,3,k)
  
END DO

END DO


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END DO





END PROGRAM Nparticle_box

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE vetores(st,Na,ac,mar,Pot,box,rcut,frame)
INTEGER :: a, b, Na
REAL(KIND=8), DIMENSION(:,:), INTENT(INOUT) :: st(Na,3), ac(Na,3)
REAL(KIND=8) :: stmod, stabx, staby, stabz, sig, eps, Frx, Fry, Frz, acx, acy, acz, acmod
REAL(KIND=8) :: mar, Pot, box, rcut, Press, frame
REAL(KIND=8), PARAMETER :: Kb2Kelvin=0.86173308


sig=3.41
eps=0.01
DO a=1, Na
  ac(a,1)=0.
  ac(a,2)=0.
  ac(a,3)=0.
ENDDO

Pot=0.
Press=0
DO a=1, Na-1
	DO b=a+1, Na

!!!! Aplicando correcao das imagens periodicas
        stabx=st(a,1)-st(b,1)-box*NINT((st(a,1)-st(b,1))/box)
        staby=st(a,2)-st(b,2)-box*NINT((st(a,2)-st(b,2))/box)
        stabz=st(a,3)-st(b,3)-box*NINT((st(a,3)-st(b,3))/box)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        stmod=sqrt((stabx)**2+(staby)**2+(stabz)**2)

!!!! Aplicando o radio de corte (rcutoff) do potencial LJ
        IF (stmod < rcut ) THEN

   Frx=((24.*eps)/stmod)*(2.*(sig/stmod)**12 - (sig/stmod)**6 )*(stabx/stmod)
   Fry=((24.*eps)/stmod)*(2.*(sig/stmod)**12 - (sig/stmod)**6 )*(staby/stmod)
   Frz=((24.*eps)/stmod)*(2.*(sig/stmod)**12 - (sig/stmod)**6 )*(stabz/stmod)

	ac(a,1)=ac(a,1)+(1.0/mar)*Frx
	ac(a,2)=ac(a,2)+(1.0/mar)*Fry
	ac(a,3)=ac(a,3)+(1.0/mar)*Frz

	ac(b,1)=ac(b,1)-(1.0/mar)*Frx
	ac(b,2)=ac(b,2)-(1.0/mar)*Fry
	ac(b,3)=ac(b,3)-(1.0/mar)*Frz

	Frmod=sqrt((Frx)**2+(Fry)**2+(Frz)**2)

        Pot = Pot + 4*eps*((sig/stmod)**12 - (sig/stmod)**6)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!Cálculo da pressão!!!!!!!!!!!!

        Press= Press + Frmod*stmod
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        END IF 

	
 




	ENDDO
ENDDO


RETURN

END SUBROUTINE


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE numeros(w0,pt,Na3,modw0,B,Na,dij)

INTEGER :: Na3, Na, a, k, jx, jy, jz, t

integer, dimension(:,:) :: values(1:8)
integer, dimension(:), allocatable :: seed

REAL(KIND=8), DIMENSION(:,:), INTENT(INOUT) :: w0(Na3,3)
REAL(KIND=8), DIMENSION(:,:), INTENT(INOUT) :: pt(Na3,3)
REAL(KIND=8) :: theta, phi, B, r, s, modw0
REAL(KIND=8) :: xij,yij,zij,d,dij
REAL(KIND=8), PARAMETER :: PI = 3.14159265358979323846

!!!! Distribuição inicial homogenea de particulas
a=1
DO jx=1, Na	
	DO jy=1, Na
      		DO jz=1, Na
		     pt(a,1) = (jx-1)*dij
		     pt(a,2) = (jy-1)*dij
		     pt(a,3) = (jz-1)*dij
                     a=a+1  
       		END DO
	END DO
END DO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!! Velocidades iniciais aleatorias

DO a=1, Na3


     call random_seed(size=k)
     allocate(seed(1:k))
200  call date_and_time(values=values)

     seed(:) = (a*5)*values(8)
     call random_seed(put=seed)
     call random_number(r)

     seed(:) = (a*500)*values(8)
     call random_seed(put=seed)
     call random_number(s)


     theta=r*PI
     phi=s*2*PI

     w0(a,1) = modw0*SIN(theta)*COS(phi)
     w0(a,2) = modw0*SIN(theta)*SIN(phi)
     w0(a,3) = modw0*COS(theta)

   
    DEALLOCATE(seed)

ENDDO

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

RETURN
END SUBROUTINE

