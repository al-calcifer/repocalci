PROGRAM dfloat_use
IMPLICIT NONE

INTEGER:: i,j
REAL(KIND=8):: x
i=5
j=2
x=DFLOAT(i)/DFLOAT(j)
print*,x


END PROGRAM dfloat_use
