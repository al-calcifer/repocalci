


PROGRAM derivatives
IMPLICIT NONE
INTEGER:: i,n
REAL(KIND=8):: x1,x2,x,h,f,df,ddf
REAL(KIND=8)::func,dfunc,dfunc_da,dfunc_dc,ddfunc_dc,ddfunc
n=51
x1=0.0D0
x2=5.0D0
h=1.0D-5
OPEN(UNIT=1,FILE='function.dat')
OPEN(UNIT=2,FILE='dfunction.dat')
OPEN(UNIT=3,FILE='dfunction_da.dat')
OPEN(UNIT=4,FILE='dfunction_dc.dat')
OPEN(UNIT=7,FILE='ddfunction.dat')
OPEN(UNIT=8,FILE='ddfunction_dc.dat')
DO i=1,51
  x=x1+(x2-x1)*DFLOAT(i-1)/DFLOAT(n-1)
  func=f(x)
  dfunc=df(x)
  ddfunc=ddf(x)  
  dfunc_da=(f(x+h)-f(x))/h
  dfunc_dc=(f(x+h*0.5D0)-f(x-h*0.5D0))/h
  ddfunc_dc=(f(x+h)+f(x-h)-2.0D0*f(x))/(h*h)
  WRITE(1,*) x,func
  WRITE(2,*) x,dfunc
  WRITE(3,*) x,dfunc_da
  WRITE(4,*) x,dfunc_dc
  WRITE(7,*) x,ddfunc
  WRITE(8,*) x,ddfunc_dc
END DO
WRITE(1,*)
WRITE(2,*)
WRITE(3,*)
WRITE(4,*)
WRITE(7,*)
WRITE(8,*)
CLOSE(UNIT=1)
CLOSE(UNIT=2)
CLOSE(UNIT=3)
CLOSE(UNIT=4)
CLOSE(UNIT=7)
CLOSE(UNIT=8)
END PROGRAM derivatives

FUNCTION f(x)
IMPLICIT NONE
REAL(KIND=8):: f,x
f=x*x*DSIN(x)
END FUNCTION f

FUNCTION df(x)
IMPLICIT NONE
REAL(KIND=8):: df,x
df=2.0D0*x*DSIN(x)+x*x*DCOS(x)
END FUNCTION df

FUNCTION ddf(x)
IMPLICIT NONE
REAL(KIND=8):: ddf,x
ddf=2.0D0*DSIN(x)+2.0D0*x*DCOS(x)+2.0D0*x*DCOS(x)-x*x*DSIN(x)
END FUNCTION ddf




