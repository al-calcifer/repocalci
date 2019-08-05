

PROGRAM ellis_4_1
IMPLICIT NONE
REAL(KIND=8):: x1,y1,x2,y2,d1,d2,d12

WRITE(*,*) 'Digite as coordenadas do primeiro ponto (x1,y1).'
READ(*,*) x1,y1
WRITE(*,*) 'Digite as coordenadas do segundo ponto (x2,y2).'
READ(*,*) x2,y2

CALL dist(x1,y1,x2,y2,d1,d2,d12)

WRITE(*,*) 'A distância de (x1,y1) até a origem é',d1
WRITE(*,*) 'A distância de (x2,y2) até a origem é',d2
WRITE(*,*) 'A distância de (x1,y1) até (x2,y2) é',d12

END PROGRAM ellis_4_1

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE dist(x1,y1,x2,y2,d1,d2,d12)
IMPLICIT NONE
REAL(KIND=8),INTENT(IN):: x1,y1,x2,y2
REAL(KIND=8),INTENT(OUT):: d1,d2,d12

d1=DSQRT(x1*x1+y1*y1)
d2=DSQRT(x2*x2+y2*y2)
d12=DSQRT((x1-x2)**2+(y1-y2)**2)

END SUBROUTINE dist







