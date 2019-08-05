


PROGRAM ellis_5_14
IMPLICIT NONE
REAL:: l1,l2,l3
LOGICAL:: triangle,isosceles,equilat


! Dados de entrada (input)
PRINT*,'Por favor, digite três números reais.'
READ*, l1,l2,l3
! Dados iniciais
triangle=.true.
isosceles=.false.
equilat=.false.

! Teste do triângulo
! IF(l1.ge.l2+l3)THEN
!   triangle=.false.
! END IF
! IF(l2.ge.l1+l3)THEN
!   triangle=.false.
! END IF
! IF(l3.ge.l1+l2)THEN
!   triangle=.false.
! END IF
IF((l1.ge.l2+l3).OR.(l2.ge.l1+l3).OR.(l3.ge.l1+l2))THEN
  triangle=.false.
END IF


! Teste isosceles
IF((l1.eq.l2).AND.(l2.ne.l3))THEN
  isosceles=.true.
END IF
IF((l2.eq.l3).AND.(l3.ne.l1))THEN
  isosceles=.true.
END IF
IF((l3.eq.l1).AND.(l1.ne.l2))THEN
  isosceles=.true.
END IF

! Teste equilatero
IF((l1.eq.l2).AND.(l2.eq.l3))THEN
  equilat=.true.
END IF





! Resultado
IF(triangle)THEN
  PRINT*,'As distâncias formam um triângulo.'
  IF(isosceles)THEN
    PRINT*,'O triângulo é isosceles'
  ELSE    
    PRINT*,'O triângulo não é isosceles'
  END IF
  IF(equilat)THEN
    PRINT*,'O triângulo é equilátero'
  ELSE    
    PRINT*,'O triângulo não é equilátero'
  END IF  
ELSE
  PRINT*,'As distâncias não formam um triângulo.'
END IF








END PROGRAM ellis_5_14






