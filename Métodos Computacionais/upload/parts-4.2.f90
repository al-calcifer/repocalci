

PROGRAM ellis_4_2
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

END PROGRAM ellis_4_2

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE dist(x1,y1,x2,y2,d1,d2,d12)
IMPLICIT NONE
REAL(KIND=8),INTENT(IN):: x1,y1,x2,y2
REAL(KIND=8),INTENT(OUT):: d1,d2,d12
REAL(KIND=8):: distancia

d1=distancia(x1,y1,0.0D0,0.0D0)
d2=distancia(0.0D0,0.0D0,x2,y2)
d12=distancia(x1,y1,x2,y2)

END SUBROUTINE dist

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FUNCTION distancia(x1,y1,x2,y2)
IMPLICIT NONE
REAL(KIND=8):: x1,y1,x2,y2,distancia

distancia=DSQRT((x1-x2)**2+(y1-y2)**2)

END FUNCTION distancia





