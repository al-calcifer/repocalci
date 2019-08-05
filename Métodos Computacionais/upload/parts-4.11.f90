

PROGRAM ellis_4_10
IMPLICIT NONE
REAL(KIND=8),ALLOCATABLE:: a(:)

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


END PROGRAM ellis_4_10

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!









