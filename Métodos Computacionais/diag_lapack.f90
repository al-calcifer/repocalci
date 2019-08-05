program diag
implicit none

integer :: LWORK, INFO, i
real(kind=8) :: A(4,4), W(4)
real(kind=8), allocatable :: WORK(:)


A(1,1)=3.0d0
A(1,2)=3.0d0
A(1,3)=0.0d0
A(1,4)=0.0d0
A(2,1)=3.0d0
A(2,2)=3.0d0
A(2,3)=0.0d0
A(2,4)=0.0d0
A(3,1)=0.0d0
A(3,2)=0.0d0
A(3,3)=3.0d0
A(3,4)=3.0d0
A(4,1)=0.0d0
A(4,2)=0.0d0
A(4,3)=3.0d0
A(4,4)=3.0d0


!1st call
allocate(WORK(1))
call DSYEV ('V', 'U', 4, A, 4, W, WORK, -1, INFO)
LWORK=WORK(1)


deallocate(WORK)
allocate(WORK(LWORK))
!2nd call
call DSYEV ('V', 'U', 4, A, 4, W, WORK, LWORK, INFO)
do i=1, 4
	write(*,*) 'Autovalor', i, '=', W(i)
end do

do i=1, 4
	write(*,*) 'Autovetor', i, '=', A(:,i)
end do
deallocate(WORK)
end program
