

PROGRAM ellis_4_10
IMPLICIT NONE
REAL(KIND=8),ALLOCATABLE:: a(:)
REAL(KIND=8):: ma,mg
INTEGER:: i,n

DO 
  WRITE(*,*) 'Determine quantos números você quer (n)?'
  WRITE(*,*) 'Escolha n de 1 a 10.'  
  READ(*,*) n
  IF((n.gt.0).AND.(n.le.10))THEN
    EXIT
  END IF
  WRITE(*,*) 'Valor não válido. Tente novamente.'
END DO

ALLOCATE(a(n))

WRITE(*,*) 'Entre com estes n numeros positivos.'
! DO i=1,n
!   READ(*,*) a(i)
! END DO
READ(*,*) (a(i),i=1,n)

CALL media_a(n,a,ma)
CALL media_g(n,a,mg)

WRITE(*,*) 'A média aritmética vale',ma
WRITE(*,*) 'A média geométrica vale',mg
END PROGRAM ellis_4_10

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE media_a(n,a,ma)
IMPLICIT NONE
INTEGER:: n
REAL(KIND=8):: ma,a(n)

ma=SUM(a)/DFLOAT(n)

END SUBROUTINE media_a

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE media_g(n,a,mg)
IMPLICIT NONE
INTEGER:: n,i
REAL(KIND=8):: mg,a(n)

mg=1.0D0
DO i=1,n
  mg=mg*a(i)
END DO
mg=mg**(1.0D0/DFLOAT(n))

END SUBROUTINE media_g








