

PROGRAM ellis_4_9
IMPLICIT NONE
REAL(KIND=8):: m,e,energia

WRITE(*,*) 'Entre com um valor de massa em kg.'
READ(*,*) m

e=energia(m)

WRITE(*,*) 'A energia correspondente a essa massa vale',e

END PROGRAM ellis_4_9

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION energia(m)
IMPLICIT NONE
REAL(KIND=8):: energia,m,c

 c=2.9979D8
energia=m*c*c
 
END FUNCTION energia








